//! Implementation of identifier pasting expansion `${paste }`

use crate::framework::*;

/// Accumulator for things to be pasted
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

    /// Combine the accumulated pieces and write them as tokens
    pub fn assemble(self, out: &mut TokenAccumulator) -> syn::Result<()> {
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
            let (items, items_after) = items.split_at_mut(nontrivial + 1);
            let (items_before, items) = items.split_at_mut(nontrivial);
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
                    out.write_tokens(mem::take(pre));
                    out.write_tokens(mk_ident_nt(*span, text)?);
                    out.write_tokens(/*mem::take(*/post/*)*/);
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
                    out.write_tokens(path);
                }
                Item::Plain { .. } => panic!("trivial nontrivial"),
            }
        } else {
            let span = self
                .items
                .first()
                .ok_or_else(|| self.span.error("empty ${paste ... }"))?
                .span();
            out.write_tokens(mk_ident(span, plain_strs(&self.items))?);
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
    fn push_display<S: Display + Spanned>(&mut self, plain: &S) {
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
        A: FnOnce(&mut TokenAccumulator),
        B: FnOnce(&mut TokenAccumulator),
    {
        let mut pre = TokenAccumulator::new();
        pre_(&mut pre);
        let mut post = TokenAccumulator::new();
        post_(&mut post);
        let text = ident.to_string();
        let span = ident.span();
        let mut handle_err = |prepost: TokenAccumulator| {
            prepost.tokens().unwrap_or_else(|err| {
                self.record_error(err);
                TokenStream::new()
            })
        };
        let pre = handle_err(pre);
        let post = handle_err(post);
        self.items.push(Item::IdPath {
            pre,
            post,
            text,
            span,
        });
    }
    fn push_tt_literal(&mut self, literal: &Literal) {
        match syn::parse2(TokenTree::Literal(literal.clone()).into()) {
            Ok(lit) => self.push_syn_lit(&lit),
            Err(err) => self.record_error(err),
        }
    }
    fn push_syn_lit(&mut self, lit: &syn::Lit) {
        use syn::Lit as L;
        match lit {
            L::Str(s) => self.items.push(Item::Plain {
                text: s.value(),
                span: s.span(),
            }),
            L::Int(v) => self.push_display(v),
            L::Bool(v) => self.push_lit_pair(&v.value(), lit),
            L::Verbatim(v) => self.push_display(v),
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
        F: FnOnce(&mut TokenAccumulator) -> syn::Result<()>,
    {
        void::unreachable(*no_paste)
    }

    // We forbid ${pate } inside itself, because when we do case
    // conversion this will get very fiddly to implement.
    fn expand_paste(
        &mut self,
        no_paste: &Void,
        _ctx: &Context,
        _span: Span,
        _paste_body: &Template<paste::Items>,
    ) -> syn::Result<()> {
        void::unreachable(*no_paste)
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
            TE::Pass(TT::Literal(lit)) => out.push_tt_literal(&lit),
            TE::Group { delim_span, .. } => return bad(*delim_span),
            TE::Subst(e) => e.expand(ctx, out)?,
            TE::Repeat(e) => e.expand(ctx, out),
        }
        Ok(())
    }
}
