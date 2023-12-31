//! Handling of boolean evaluation (conditions)

use super::framework::*;

/// Implementor of [`SubstParseContext`] for booleans
///
/// No values of this type are ever created -
/// it's just a generic parameter, used to select the associated
/// marker types (and their constructor methods( in SubstParseContext.
///
/// So it can be uninhabited.
#[derive(Debug)]
pub struct BooleanContext(Void);

struct Found;

fn is_found(r: Result<(), Found>) -> bool {
    r.is_err()
}

impl SubstParseContext for BooleanContext {
    type NotInPaste = ();
    type NotInBool = Void;
    type BoolOnly = ();
    fn not_in_paste(_: &impl Spanned) -> syn::Result<()> {
        Ok(())
    }
    fn bool_only(_: &impl Spanned) -> syn::Result<()> {
        Ok(())
    }

    fn not_in_bool(span: &impl Spanned) -> syn::Result<Void> {
        Err(span.error(
            "derive-adhoc construct is an expansion - not valid in a condition",
        ))
    }

    type SpecialParseContext = ();
}

impl Subst<BooleanContext> {
    pub fn eval_bool(&self, ctx: &Context) -> syn::Result<bool> {
        // eprintln!("@@@@@@@@@@@@@@@@@@@@ EVAL {:?}", self);

        let v_fields = || ctx.variant(&self.kw_span).map(|v| &v.fields);
        use syn::Fields as SF;

        let r = match &self.sd {
            SD::is_enum(..) => ctx.is_enum(),
            SD::is_struct(..) => matches!(ctx.top.data, syn::Data::Struct(_)),
            SD::is_union(..) => matches!(ctx.top.data, syn::Data::Union(_)),
            SD::v_is_unit(..) => matches!(v_fields()?, SF::Unit),
            SD::v_is_tuple(..) => matches!(v_fields()?, SF::Unnamed(..)),
            SD::v_is_named(..) => matches!(v_fields()?, SF::Named(..)),

            SD::xmeta(sm) => {
                let SubstMeta {
                    path,
                    as_,
                    level: _,
                } = sm;
                use SubstMetaAs as SMA;
                if let Some(as_) = as_ {
                    match as_ {
                        SMA::str(nb) | SMA::tokens(nb, ..) | SMA::ty(nb) => {
                            void::unreachable(*nb)
                        }
                    }
                };
                is_found(path.search_eval_bool(sm.pmetas(ctx)?))
            }

            SD::approx_equal(_, [a, b]) => {
                let s = |x: &Template<_>| {
                    let mut out = TokenAccumulator::new();
                    x.expand(ctx, &mut out);
                    let out = out.tokens()?;
                    Ok::<TokenStream, syn::Error>(out)
                };
                tokens_cmp(s(a)?, s(b)?) == Ordering::Equal
            }

            SD::UserDefined(name) => name.lookup_eval_bool(ctx)?,

            SD::False(..) => false,
            SD::True(..) => true,

            SD::not(v, _) => !v.eval_bool(ctx)?,
            SD::any(vs, _) => vs
                .iter()
                .find_map(|v| match v.eval_bool(ctx) {
                    Ok(true) => Some(Ok(true)),
                    Err(e) => Some(Err(e)),
                    Ok(false) => None,
                })
                .unwrap_or(Ok(false))?,
            SD::all(vs, _) => vs
                .iter()
                .find_map(|v| match v.eval_bool(ctx) {
                    Ok(true) => None,
                    Err(e) => Some(Err(e)),
                    Ok(false) => Some(Ok(false)),
                })
                .unwrap_or(Ok(true))?,

            SD::Vis(vis, _) => match vis.syn_vis(ctx, self.kw_span)? {
                syn::Visibility::Public(_) => true,
                _ => false,
            },

            // ## maint/check-keywords-documented NotInBool ##
            SD::tname(not_in_bool)
            | SD::ttype(not_in_bool)
            | SD::tdeftype(not_in_bool)
            | SD::vname(not_in_bool)
            | SD::fname(not_in_bool)
            | SD::ftype(not_in_bool)
            | SD::vtype(_, _, not_in_bool)
            | SD::tdefkwd(not_in_bool)
            | SD::tattrs(_, _, not_in_bool)
            | SD::vattrs(_, _, not_in_bool)
            | SD::fattrs(_, _, not_in_bool)
            | SD::tgens(_, not_in_bool)
            | SD::tdefgens(_, not_in_bool)
            | SD::tgnames(_, not_in_bool)
            | SD::twheres(_, not_in_bool)
            | SD::vpat(_, _, not_in_bool)
            | SD::fpatname(not_in_bool)
            | SD::tdefvariants(_, _, not_in_bool)
            | SD::fdefine(_, _, not_in_bool)
            | SD::vdefbody(_, _, _, not_in_bool)
            | SD::paste(_, not_in_bool)
            | SD::ChangeCase(_, _, not_in_bool)
            | SD::when(_, not_in_bool)
            | SD::define(_, not_in_bool)
            | SD::defcond(_, not_in_bool)
            | SD::For(_, not_in_bool)
            | SD::If(_, not_in_bool)
            | SD::select1(_, not_in_bool)
            | SD::dbg_all_keywords(not_in_bool)
            | SD::Crate(_, not_in_bool) => void::unreachable(*not_in_bool),
        };
        Ok(r)
    }
}

impl DefinitionName {
    fn lookup_eval_bool(&self, ctx: &Context<'_>) -> syn::Result<bool> {
        let (def, ctx) = ctx.find_definition::<DefCondBody>(self)?.ok_or_else(|| {
            let mut error = self.error("user-defined condition not fund");
            if let Some(def) = ctx.definitions.find_raw::<DefinitionBody>(self) {
                // Condition syntax looks like fine tokens,
                // so the ${define } wouldn't spot this mistake.
                error.combine(
                    def.name.error(
"this user-defined expansion used as a condition (perhaps you meant ${defcond ?}"
                    )
                );
            }
            error
        })?;

        def.body.eval_bool(&ctx)
    }
}

impl SubstMetaPath {
    fn search_eval_bool(
        &self,
        pmetas: &PreprocessedMetas,
    ) -> Result<(), Found> {
        self.search(pmetas, &mut |_av| /* got it! */ Err(Found))
    }

    pub fn search<'a, A, F, E>(&self, pmetas: A, f: &mut F) -> Result<(), E>
    where
        F: FnMut(MetaNode<'a>) -> Result<(), E>,
        A: IntoIterator<Item = &'a PreprocessedMeta>,
    {
        for pmeta in pmetas {
            self.search_1(pmeta, &mut *f)?;
        }
        Ok(())
    }

    fn search_1<'a, E, F>(
        &self,
        pmeta: &'a PreprocessedMeta,
        f: &mut F,
    ) -> Result<(), E>
    where
        F: FnMut(MetaNode<'a>) -> Result<(), E>,
    {
        #![allow(non_camel_case_types)]
        use syn::Meta as sM;

        if pmeta.path() != &self.path {
            return Ok(());
        }

        let vspan = pmeta.span();
        match (&self.deeper, pmeta) {
            (None, sM::Path(_)) => f(MN::Unit(vspan))?,
            (None, sM::List(_)) => f(MN::Deeper(vspan))?,
            (None, sM::NameValue(nv)) => f(MN::Lit(&nv.lit))?,
            (Some(_), sM::NameValue(_)) => {}
            (Some(_), sM::Path(_)) => {} // self is deeper than pmeta
            (Some(d), sM::List(l)) => {
                for nm in &l.nested {
                    let m = match nm {
                        syn::NestedMeta::Meta(m) => m,
                        syn::NestedMeta::Lit(_) => continue,
                    };
                    d.search_1(m, &mut *f)?;
                }
            }
        }
        Ok(())
    }
}

/// Compares two `TokenStream`s for "equivalence"
///
/// We intend that two `TokenStream`s count as "equivalent"
/// if they mean the same thing to the compiler,
/// modulo any differences in spans.
///
/// We also disregard spacing.  This is not 100% justifiable but
/// I think there are no token sequences differing only in spacing
/// which are *both* valid and which differ in meaning.
///
/// ### Why ?!
///
/// `< <` and `<<` demonstrate that it is not possible to provide
/// a fully correct and coherent equality function on Rust tokens,
/// without knowing the parsing context:
///
/// In places where `<<` is a shift operator, `< <` is not legal.
/// But in places where `<<` introduces two lots of generics,
/// `<<` means the same.
///
/// I think a function which treats `< <` and `<<` as equal is more useful
/// than one that doesn't, because it will DTRT for types.
//
// Comparing for equality has to be done by steam.
// And a lot of stringification.
pub fn tokens_cmp(a: TokenStream, b: TokenStream) -> cmp::Ordering {
    use proc_macro2::Group;

    fn tt_cmp(a: TokenTree, b: TokenTree) -> Ordering {
        let discrim = |tt: &_| match tt {
            TT::Punct(_) => 0,
            TT::Literal(_) => 1,
            TT::Ident(_) => 2,
            TT::Group(_) => 3,
        };

        discrim(&a).cmp(&discrim(&b)).then_with(|| match (a, b) {
            (TT::Group(a), TT::Group(b)) => group_cmp(a, b),
            (l, r) => l.to_string().cmp(&r.to_string()),
        })
    }

    fn group_cmp(a: Group, b: Group) -> Ordering {
        let delim = |g: &Group| {
            proc_macro2::Group::new(g.delimiter(), TokenStream::new())
                .to_string()
        };
        delim(&a)
            .cmp(&delim(&b))
            .then_with(|| tokens_cmp(a.stream(), b.stream()))
    }

    for (a, b) in izip!(a, b) {
        match tt_cmp(a, b) {
            Ordering::Equal => {}
            neq => return neq,
        }
    }
    return Ordering::Equal;
}
