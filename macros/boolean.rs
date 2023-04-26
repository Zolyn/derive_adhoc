//! Handling of boolean evaluation (conditions)

use crate::framework::*;

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
    type NotInCase = ();
    type NotInBool = Void;
    type AllowNonterminal = ();
    type BoolOnly = ();
    fn not_in_paste(_: &impl Spanned) -> syn::Result<()> {
        Ok(())
    }
    fn not_in_case(_: &impl Spanned) -> syn::Result<()> {
        Ok(())
    }
    fn allow_nonterminal(_: &impl Spanned) -> syn::Result<()> {
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
}

impl Subst<BooleanContext> {
    pub fn eval_bool(&self, ctx: &Context) -> syn::Result<bool> {
        // eprintln!("@@@@@@@@@@@@@@@@@@@@ EVAL {:?}", self);

        macro_rules! eval_attr { { $wa:expr, $lev:ident, $($pattrs:tt)* } => { {
            let SubstMeta { path, as_, as_span: _ } = $wa;
            if let Some(as_) = as_ {
                void::unreachable(as_.1)
            }
            is_found(ctx.for_with_within::<$lev,_,_>(|_ctx, within| {
                path.search_eval_bool(&within . $($pattrs)*)
            }))
        } } }
        let v_fields = || ctx.variant(&self.kw_span).map(|v| &v.fields);
        use syn::Fields as SF;

        let r = match &self.sd {
            SD::tmeta(wa) => is_found(wa.path.search_eval_bool(ctx.tattrs)),
            SD::vmeta(wa) => eval_attr! { wa, WithinVariant, pattrs },
            SD::fmeta(wa) => eval_attr! { wa, WithinField, pfield.pattrs },
            SD::is_enum(..) => ctx.is_enum(),
            SD::is_struct(..) => matches!(ctx.top.data, syn::Data::Struct(_)),
            SD::is_union(..) => matches!(ctx.top.data, syn::Data::Union(_)),
            SD::v_is_unit(..) => matches!(v_fields()?, SF::Unit),
            SD::v_is_tuple(..) => matches!(v_fields()?, SF::Unnamed(..)),
            SD::v_is_named(..) => matches!(v_fields()?, SF::Named(..)),

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
            | SD::paste(_, _, _, not_in_bool)
            | SD::ChangeCase(_, _, _, not_in_bool)
            | SD::when(_, not_in_bool, _)
            | SD::For(_, not_in_bool)
            | SD::If(_, not_in_bool)
            | SD::select1(_, not_in_bool)
            | SD::dbg_all_keywords(not_in_bool)
            | SD::Crate(_, not_in_bool) => void::unreachable(*not_in_bool),
        };
        Ok(r)
    }
}

impl SubstMetaPath {
    fn search_eval_bool(
        &self,
        pattrs: &PreprocessedMetas,
    ) -> Result<(), Found> {
        self.search(pattrs, &mut |_av| /* got it! */ Err(Found))
    }

    pub fn search<'a, A, F, E>(&self, pattrs: A, f: &mut F) -> Result<(), E>
    where
        F: FnMut(AttrValue<'a>) -> Result<(), E>,
        A: IntoIterator<Item = &'a PreprocessedMeta>,
    {
        for pattr in pattrs {
            self.search_1(pattr, &mut *f)?;
        }
        Ok(())
    }

    fn search_1<'a, E, F>(
        &self,
        pattr: &'a PreprocessedMeta,
        f: &mut F,
    ) -> Result<(), E>
    where
        F: FnMut(AttrValue<'a>) -> Result<(), E>,
    {
        #![allow(non_camel_case_types)]
        use syn::Meta as sM;

        if pattr.path() != &self.path {
            return Ok(());
        }

        let vspan = pattr.span();
        match (&self.deeper, pattr) {
            (None, sM::Path(_)) => f(AV::Unit(vspan))?,
            (None, sM::List(_)) => f(AV::Deeper(vspan))?,
            (None, sM::NameValue(nv)) => f(AV::Lit(&nv.lit))?,
            (Some(_), sM::NameValue(_)) => {}
            (Some(_), sM::Path(_)) => {} // self is deeper than pattr
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
