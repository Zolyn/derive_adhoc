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
    type NoPaste = ();
    type NoCase = ();
    type NoBool = Void;
    type NoNonterminal = ();
    type BoolOnly = ();
    fn no_paste(_: &impl Spanned) -> syn::Result<()> {
        Ok(())
    }
    fn no_case(_: &impl Spanned) -> syn::Result<()> {
        Ok(())
    }
    fn no_nonterminal(_: &impl Spanned) -> syn::Result<()> {
        Ok(())
    }
    fn bool_only(_: &impl Spanned) -> syn::Result<()> {
        Ok(())
    }

    fn no_bool(span: &impl Spanned) -> syn::Result<Void> {
        Err(span.error(
            "derive-adhoc keyword is an expansion - not valid as a condition",
        ))
    }
}

impl Subst<BooleanContext> {
    pub fn eval_bool(&self, ctx: &Context) -> syn::Result<bool> {
        // eprintln!("@@@@@@@@@@@@@@@@@@@@ EVAL {:?}", self);

        macro_rules! eval_attr { { $wa:expr, $lev:ident, $($pattrs:tt)* } => {
            is_found(ctx.for_with_within::<$lev,_,_>(|_ctx, within| {
                $wa.path.search_eval_bool(&within . $($pattrs)*)
            }))
        } }

        let r = match &self.sd {
            SD::tmeta(wa) => is_found(wa.path.search_eval_bool(ctx.tattrs)),
            SD::vmeta(wa) => eval_attr! { wa, WithinVariant, pattrs },
            SD::fmeta(wa) => eval_attr! { wa, WithinField, pfield.pattrs },
            SD::is_enum(..) => matches!(ctx.top.data, syn::Data::Enum(_)),

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

            SD::tname(no_bool)
            | SD::ttype(no_bool)
            | SD::vname(no_bool)
            | SD::fname(no_bool)
            | SD::ftype(no_bool)
            | SD::tattrs(_, _, no_bool)
            | SD::vattrs(_, _, no_bool)
            | SD::fattrs(_, _, no_bool)
            | SD::tgens(_, no_bool)
            | SD::tgnames(_, no_bool)
            | SD::twheres(_, no_bool)
            | SD::paste(_, _, no_bool)
            | SD::when(_, no_bool, _)
            | SD::For(_, no_bool)
            | SD::If(_, no_bool) => void::unreachable(*no_bool),
        };
        Ok(r)
    }
}

impl SubstAttrPath {
    fn search_eval_bool(
        &self,
        pattrs: &PreprocessedAttrs,
    ) -> Result<(), Found> {
        self.search(pattrs, &mut |_av| /* got it! */ Err(Found))
    }

    pub fn search<'a, A, F, E>(&self, pattrs: A, f: &mut F) -> Result<(), E>
    where
        F: FnMut(AttrValue<'a>) -> Result<(), E>,
        A: IntoIterator<Item = &'a PreprocessedAttr>,
    {
        for pattr in pattrs {
            self.search_1(pattr, &mut *f)?;
        }
        Ok(())
    }

    fn search_1<'a, E, F>(
        &self,
        pattr: &'a PreprocessedAttr,
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

        match (&self.deeper, pattr) {
            (None, sM::Path(_)) => f(AV::Unit)?,
            (None, sM::List(_)) => f(AV::Deeper)?,
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
