#![allow(dead_code)]

use crate::framework::*;

#[derive(Debug)]
pub struct Items {
    span: Span,
    items: Vec<Item>,
    errors: Vec<syn::Error>,
}

#[derive(Debug)]
enum Item {
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

impl Spanned for Item {
    fn span(&self) -> Span {
        match self {
            Item::Plain { span, .. } => *span,
            Item::IdPath { span, .. } => *span,
            Item::Path(path) => path.span(),
        }
    }
}

impl Items {
    pub fn new(span: Span) -> Self {
        Items {
            span,
            items: vec![],
            errors: vec![],
        }
    }

    fn push_lit_pair<V: Display, S: Spanned>(&mut self, v: &V, s: &S) {
        self.items.push(Item::Plain {
            text: v.to_string(),
            span: s.span(),
        })
    }

    pub fn assemble(self, out: &mut TokenStream) -> syn::Result<()> {
        if !self.errors.is_empty() {
            for error in self.errors {
                out.record_error(error);
            }
            return Ok(());
        }

        let nontrivial = self
            .items
            .iter()
            .positions(|it| !matches!(it, Item::Plain { .. }))
            .at_most_one()
            .map_err(|mut eoe| {
                self.items[eoe.next().unwrap()]
                    .span()
                    .error("multiple nontrivial entries in ${paste ...}")
            })?;

        fn plain_strs(items: &[Item]) -> impl Iterator<Item = &str> {
            items.iter().map(|item| match item {
                Item::Plain { text, .. } => text.as_str(),
                _ => panic!("non plain item"),
            })
        }

        fn mk_ident<'i>(
            span: Span,
            items: impl Iterator<Item = &'i str>,
        ) -> syn::Result<syn::Ident> {
            let ident = items.collect::<String>();
            catch_unwind(|| format_ident!("{}", ident, span = span)).map_err(
                |_| {
                    span.error(format_args!(
                        "pasted identifier {:?} is invalid",
                        ident
                    ))
                },
            )
        }

        if let Some(nontrivial) = nontrivial {
            let mut items = self.items;
            let (items, items_before) = items.split_at_mut(nontrivial + 1);
            let (items_after, items) = items.split_at_mut(nontrivial);
            let nontrivial = &mut items[0];

            let mk_ident_nt = |span, text: &str| {
                mk_ident(
                    span,
                    chain!(
                        plain_strs(items_before),
                        iter::once(text),
                        plain_strs(items_after),
                    ),
                )
            };

            match nontrivial {
                Item::IdPath {
                    pre,
                    text,
                    span,
                    post,
                } => {
                    out.extend(mem::take(pre));
                    mk_ident_nt(*span, text)?.to_tokens(out);
                    out.extend(mem::take(post));
                }
                Item::Path(path) => {
                    let span = path.span();
                    let last = &mut path
                        .path
                        .segments
                        .last_mut()
                        .ok_or_else(|| {
                            span.error(
               "derive-adhoc token pasting applied to path with no components"
                        )
                        })?
                        .ident;
                    *last = mk_ident_nt(last.span(), &last.to_string())?;
                    path.to_tokens(out);
                }
                Item::Plain { .. } => panic!("trivial nontrivial"),
            }
        } else {
            let span = self
                .items
                .first()
                .ok_or_else(|| self.span.error("empty ${paste ... }"))?
                .span();
            mk_ident(span, plain_strs(&self.items))?.to_tokens(out);
        }

        Ok(())
    }
}

impl SubstParseContext for Items {
    type NoPaste = Void;
    type NoBool = ();
    type BoolOnly = Void;

    fn no_bool(_: &impl Spanned) -> syn::Result<()> {
        Ok(())
    }

    fn no_paste(span: &impl Spanned) -> syn::Result<Void> {
        Err(span.error("not allowed in within ${paste ...}"))
    }
}

impl ExpansionOutput for Items {
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
        self.items.push(Item::IdPath {
            pre,
            post,
            text,
            span,
        });
    }
    fn push_syn_lit(&mut self, lit: &syn::Lit) {
        use syn::Lit as L;
        match lit {
            L::Str(s) => self.items.push(Item::Plain {
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
                self.items.push(Item::Path(path.clone()))
            },
            x => self.write_error(
                x,
                "derive-adhoc macro wanted to do identifier pasting, but complex type provided"
            ),
        }
    }
    fn push_other_subst<S, F>(
        &mut self,
        no_paste: &Void,
        _: &S,
        _: F,
    ) -> syn::Result<()>
    where
        S: Spanned,
        F: FnOnce(&mut TokenStream) -> syn::Result<()>,
    {
        void::unreachable(*no_paste)
    }
    fn expand_paste(
        &mut self,
        ctx: &Context,
        _span: Span,
        paste_body: &Template<paste::Items>,
    ) -> syn::Result<()> {
        // ${pate .. ${pate ..} ...}
        // Strange, but, whatever.
        paste_body.expand(ctx, self);
        Ok(())
    }
    fn expand_bool_only(&mut self, bool_only: &Self::BoolOnly) -> ! {
        void::unreachable(*bool_only)
    }

    fn record_error(&mut self, err: syn::Error) {
        self.errors.push(err);
    }
}

impl Expand<Items> for TemplateElement<Items> {
    fn expand(&self, ctx: &Context, out: &mut Items) -> syn::Result<()> {
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
