#![allow(dead_code)]

pub use crate::prelude::*;
pub use crate::boolean::*;
pub use crate::repeat::*;
pub use crate::syntax::*;

pub(crate) use crate::paste;

#[derive(Debug, Clone)]
pub struct Context<'c> {
    pub top: &'c syn::DeriveInput,
    pub tattrs: &'c PreprocessedAttrs,
    pub variant: Option<&'c WithinVariant<'c>>,
    pub field: Option<&'c WithinField<'c>>,
    pub pvariants: &'c [PreprocessedVariant<'c>],
}

#[derive(Debug, Clone)]
pub struct PreprocessedVariant<'f> {
    pub fields: &'f syn::Fields,
    pub pattrs: PreprocessedAttrs,
    pub pfields: Vec<PreprocessedField>,
}

#[derive(Debug, Clone)]
pub struct PreprocessedField {
    pub pattrs: PreprocessedAttrs,
}

pub type PreprocessedAttr = syn::Meta;
pub type PreprocessedAttrs = Vec<PreprocessedAttr>;

#[derive(Debug, Clone)]
pub struct WithinVariant<'c> {
    pub variant: Option<&'c syn::Variant>,
    pub fields: &'c syn::Fields,
    pub pattrs: &'c PreprocessedAttrs,
    pub pfields: &'c [PreprocessedField],
}

#[derive(Debug, Clone)]
pub struct WithinField<'c> {
    pub field: &'c syn::Field,
    pub pfield: &'c PreprocessedField,
    pub index: u32,
}

enum Todo {}

pub trait SubstParseContext {
    type NoPaste: Debug + Copy + Sized;
    type NoBool: Debug + Copy + Sized;
    type BoolOnly: Debug + Copy + Sized;

    fn no_paste(span: &impl Spanned) -> syn::Result<Self::NoPaste>;
    fn no_bool(span: &impl Spanned) -> syn::Result<Self::NoBool>;
    fn bool_only(span: &impl Spanned) -> syn::Result<Self::BoolOnly> {
        Err(span.error(
            "derive-adhoc keyword is a condition - not valid as an expansion",
        ))
    }
}

pub trait ExpansionOutput: SubstParseContext {
    fn push_lit<I: Display + Spanned + ToTokens>(&mut self, ident: &I);
    fn push_ident<I: quote::IdentFragment + Spanned + ToTokens>(
        &mut self,
        ident: &I,
    );
    fn push_idpath<A, B>(&mut self, pre: A, ident: &syn::Ident, post: B)
    where
        A: FnOnce(&mut TokenStream),
        B: FnOnce(&mut TokenStream);
    fn push_syn_lit(&mut self, v: &syn::Lit);
    fn push_syn_type(&mut self, v: &syn::Type);
    fn push_other_subst<S, F>(
        &mut self,
        np: &Self::NoPaste,
        _: &S,
        f: F,
    ) -> syn::Result<()>
    where
        S: Spanned,
        F: FnOnce(&mut TokenStream) -> syn::Result<()>;

    fn expand_bool_only(&mut self, bool_only: &Self::BoolOnly) -> !;

    fn expand_paste(
        &mut self,
        ctx: &Context,
        span: Span,
        paste_body: &Template<paste::Items>,
    ) -> syn::Result<()>;

    fn record_error(&mut self, err: syn::Error);

    fn write_error<S: Spanned, M: Display>(&mut self, s: &S, m: M) {
        self.record_error(s.error(m));
    }
}

pub trait Expand<O, R = syn::Result<()>> {
    fn expand(&self, ctx: &Context, out: &mut O) -> R;
}

impl SubstParseContext for TokenStream {
    type NoPaste = ();
    type NoBool = ();
    fn no_bool(_: &impl Spanned) -> syn::Result<()> {
        Ok(())
    }
    fn no_paste(_: &impl Spanned) -> syn::Result<()> {
        Ok(())
    }

    type BoolOnly = Void;
}

impl ExpansionOutput for TokenStream {
    fn push_lit<L: Display + Spanned + ToTokens>(&mut self, lit: &L) {
        lit.to_tokens(self)
    }
    fn push_ident<I: quote::IdentFragment + Spanned + ToTokens>(
        &mut self,
        ident: &I,
    ) {
        ident.to_tokens(self)
    }
    fn push_idpath<A, B>(&mut self, pre: A, ident: &syn::Ident, post: B)
    where
        A: FnOnce(&mut TokenStream),
        B: FnOnce(&mut TokenStream),
    {
        pre(self);
        ident.to_tokens(self);
        post(self);
    }
    fn push_syn_lit(&mut self, lit: &syn::Lit) {
        lit.to_tokens(self);
    }
    fn push_syn_type(&mut self, ty: &syn::Type) {
        ty.to_tokens(self);
    }
    /*    fn push_attr_value(&mut self, av: AttrValue, as_: SubstAttrAs) {
        let mut buf = TokenStream::new();
        av.expand(self.span(), &self.as_, &mut buf)?;
        let found = Some(buf);

        self.extend(found);
    }*/
    fn push_other_subst<S, F>(
        &mut self,
        _no_paste: &(),
        _: &S,
        f: F,
    ) -> syn::Result<()>
    where
        S: Spanned,
        F: FnOnce(&mut TokenStream) -> syn::Result<()>,
    {
        f(self)
    }
    fn expand_paste(
        &mut self,
        ctx: &Context,
        span: Span,
        paste_body: &Template<paste::Items>,
    ) -> syn::Result<()> {
        let mut items = paste::Items::new(span);
        paste_body.expand(ctx, &mut items);
        items.assemble(self)
    }
    fn expand_bool_only(&mut self, bool_only: &Self::BoolOnly) -> ! {
        void::unreachable(*bool_only)
    }

    fn record_error(&mut self, err: syn::Error) {
        self.extend(err.into_compile_error())
    }
}
