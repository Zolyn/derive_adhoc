#![allow(dead_code)]

use super::*;

use TemplateElement as TE;

pub struct PasteItems {
    items: Vec<PasteItem>,
    errors: Vec<syn::Error>,
}

#[derive(Debug)]
enum PasteItem {
    Plain {
        text: String,
        span: Span,
    },
    IdPath {
        pre: TokenStream,
        text: String,
        span: Span,
        post: TokenStream,
    },
    Path(syn::TypePath),
}

impl PasteItems {
    fn push_lit_pair<V: Display, S: Spanned>(&mut self, v: &V, s: &S) {
        self.items.push(PasteItem::Plain {
            text: v.to_string(),
            span: s.span(),
        })
    }
}

impl ExpansionOutput for PasteItems {
    fn push_lit<S: Display + Spanned>(&mut self, plain: &S) {
        self.push_lit_pair(plain, plain);
    }
    fn push_ident<I: quote::IdentFragment + Spanned + ToTokens>(
        &mut self,
        ident: &I,
    ) {
        use quote::IdentFragment as QIF;
        struct AsIdentFragment<'i, I>(&'i I);
        impl<'i, I: QIF> Display for AsIdentFragment<'i, I> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                QIF::fmt(&self.0, f)
            }
        }
        self.push_lit_pair(&AsIdentFragment(ident), ident);
    }
    fn push_idpath<A, B>(&mut self, pre_: A, ident: &syn::Ident, post_: B)
    where
        A: FnOnce(&mut TokenStream),
        B: FnOnce(&mut TokenStream),
    {
        let mut pre = TokenStream::new();
        pre_(&mut pre);
        let mut post = TokenStream::new();
        post_(&mut post);
        let text = ident.to_string();
        let span = ident.span();
        self.items.push(PasteItem::IdPath {
            pre,
            post,
            text,
            span,
        });
    }
    fn push_syn_lit(&mut self, lit: &syn::Lit) {
        use syn::Lit as L;
        match lit {
            L::Str(s) => self.items.push(PasteItem::Plain {
                text: s.value(),
                span: s.span(),
            }),
            L::Int(v) => self.push_lit(v),
            L::Bool(v) => self.push_lit_pair(&v.value(), lit),
            L::Verbatim(v) => self.push_lit(v),
            x => self.write_error(
                x,
                "derive-adhoc macro wanted to do identifier pasting, but inappropriate literal provided",
            ),
        }
    }
    fn push_syn_type(&mut self, ty: &syn::Type) {
        match ty {
            syn::Type::Path(path) => {
                self.items.push(PasteItem::Path(path.clone()))
            },
            x => self.write_error(
                x,
                "derive-adhoc macro wanted to do identifier pasting, but complex type provided"
            ),
        }
    }
    fn push_other_subst<S, F>(&mut self, subst: &S, _: F) -> syn::Result<()>
    where
        S: Spanned,
        F: FnOnce(&mut TokenStream) -> syn::Result<()>,
    {
        Err(subst
            .span()
            .error("unsupported substitution in identifier pasting"))
    }
    fn record_error(&mut self, err: syn::Error) {
        self.errors.push(err);
    }
}

impl Expand<PasteItems> for TemplateElement {
    fn expand(&self, ctx: &Context, out: &mut PasteItems) -> syn::Result<()> {
        let bad = |span: Span| Err(span.error("not allowed in ${paste }"));
        match self {
            TE::Pass(TT::Ident(ident)) => out.push_ident(&ident),
            TE::Pass(TT::Group(x)) => return bad(x.span()),
            TE::Pass(TT::Punct(x)) => return bad(x.span()),
            TE::Pass(TT::Literal(lit)) => out.push_lit(&lit),
            TE::Group { delim_span, .. } => return bad(*delim_span),
            TE::Subst(e) => e.expand(ctx, out)?,
            TE::Repeat(e) => e.expand(ctx, out),
        }
        Ok(())
    }
}
