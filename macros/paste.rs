//! Implementation of identifier pasting expansion `${paste }`

use crate::framework::*;

/// Accumulator for things to be pasted
///
/// Implements [`ExpansionOutput`] and [`SubstParseContext`]:
/// i.e., it corresponds to the lexical context for a `${paste }`,
/// and collects the identifier fragments being pasted.
#[derive(Debug)]
pub struct Items {
    tspan: Span,
    items: Vec<Item>,
    errors: Vec<syn::Error>,
}

#[derive(Debug)]
/// Entry in a `${paste ...}` or `${CASE ...}`
///
/// `te_span` is the span of this item entry in the template.
/// It is used for error reporting if we can't cope with this entry
/// (for example, if we have multiple nontrivial entries.)
enum Item {
    Plain {
        text: String,
    },
    Complex {
        pre: TokenStream,
        text: String,
        post: TokenStream,
        te_span: Span,
    },
}

/// Define cases using heck
///
/// heck doesn't provide standard names for case conversions,
/// or an enum to represent a case conversion.
/// This macro defines both.  Specifically:
///  * The fieldless enum `ChangeCase`.
///  * Its `FromStr` implementation, which accepts the `$keyword`s.
///  * Its `apply()` method, which actually performs the conversion.
///
/// `$heck` is the name of the `As` conversion struct in the heck
/// API.  It will become the variant name in `ChangeCase`.
///
/// `$keyword` are the keywords we recognise for this case conversion.
#[cfg(feature = "case")]
macro_rules! define_cases { {
    $(
        $heck:ident $( $keyword:literal )*,
    )*
} => {
    #[derive(Debug, Clone, Copy)]
    pub enum ChangeCase {
        $( $heck, )*
    }

    impl FromStr for ChangeCase {
        type Err = ();
        fn from_str(s: &str) -> Result<Self, ()> {
            Ok(match s {
              $(
                $( $keyword )|* => ChangeCase::$heck,
              )*
                _ => return Err(()),
            })
        }
    }

    impl ChangeCase {
        fn apply(self, input: &str) -> String {
            match self {
              $(
                ChangeCase::$heck => heck::$heck(input).to_string(),
              )*
            }
        }
    }
} }

#[cfg(not(feature = "case"))]
macro_rules! define_cases { {
    $(
        $heck:ident $( $keyword:literal )*,
    )*
} => {
    #[derive(Debug, Clone, Copy)]
    pub enum ChangeCase {}

    impl FromStr for ChangeCase {
        type Err = ();
        fn from_str(s: &str) -> Result<Self, ()> {
            match s {
              $(
                $( $keyword )|* => {}
              )*
                _ => return Err(()),
            }
            // Minimal build, so no need to bother with a proper error.
            panic!(
 "case changing not supported, enable `case` feature of `derive-adhoc`"
            );
        }
    }

    impl ChangeCase {
        fn apply(self, _input: &str) -> String {
            match self {}
        }
    }
} }

define_cases! {
    // heck API       our keyword (and aliases)  our example(s)
    AsUpperCamelCase    "pascal_case"              "PascalCase"
                        "upper_camel_case"         "UpperCamelCase"    ,
    AsLowerCamelCase    "lower_camel_case"         "lowerCamelCase"    ,
    AsSnakeCase         "snake_case",
    AsShoutySnakeCase   "shouty_snake_case"        "SHOUTY_SNAKE_CASE" ,
}

impl Items {
    pub fn new(tspan: Span) -> Self {
        Items {
            tspan,
            items: vec![],
            errors: vec![],
        }
    }
}

impl Items {
    fn append_item(&mut self, item: Item) {
        self.items.push(item);
    }
    /// Like `ExpansionOutput::append_display` but doesn't need `Spanned`
    fn append_display<V: Display>(&mut self, v: &V) {
        self.append_item(Item::Plain {
            text: v.to_string(),
        })
    }
    pub fn append_fixed_string(&mut self, text: String) {
        self.append_item(Item::Plain { text });
    }

    /// Combine the accumulated pieces and append them to `out`
    ///
    /// Calls
    /// [`append_idpath`](ExpansionOutput::append_idpath)
    /// if the content contained a nontrivial expansion
    /// or
    /// [`append_identfrag_toks`](ExpansionOutput::append_identfrag_toks)
    /// otherwise.
    pub fn assemble(
        self,
        out: &mut impl ExpansionOutput,
        change_case: Option<ChangeCase>,
    ) -> syn::Result<()> {
        if !self.errors.is_empty() {
            for error in self.errors {
                out.record_error(error);
            }
            return Ok(());
        }

        match Self::assemble_inner(self.tspan, self.items, change_case)? {
            Either::Left(ident) => out.append_identfrag_toks(
                &ident, //
            ),
            Either::Right((tspan, pre, ident, post)) => out.append_idpath(
                tspan,
                |ta| ta.append(pre),
                &ident,
                |ta| ta.append(post),
            ),
        }

        Ok(())
    }

    /// Combine the accumulated pieces and say what to do
    ///
    /// Inner, non-monomorphised, function for [`Items::assemble`].
    ///
    /// Returns `Right` with values to pass to
    /// [`append_idpath`](ExpansionOutput::append_idpath)
    /// or
    /// `Left` with the value to pass to
    /// [`append_identfrag_toks`](ExpansionOutput::append_identfrag_toks).
    fn assemble_inner(
        tspan: Span,
        items: Vec<Item>,
        change_case: Option<ChangeCase>,
    ) -> syn::Result<
        Either<syn::Ident, (Span, TokenStream, syn::Ident, TokenStream)>,
    > {
        // We must always use a similar span when we emit identifiers
        // that are going to be used to bind variables, or the hygiene
        // system doesn't think they're the same identifier.
        //
        // We choose the template keyword span for this.
        // (The span of `paste` in `${paste ...}`).
        // This isn't perfect, since we might want to point at the driver,
        // but we don't always have a suitable driver span.
        //
        // This applies to `fpatname`, `vpat`, and so on, too.
        //
        // TODO should this apply to fname too?  The template author ought to
        // us $vpat to bind fields, not $fname, since $fname risks clashes
        // with other variables that might be in scope.  But the rustc error
        // messages for identifiers with the wrong span are rather poor.
        let out_span = tspan;

        let nontrivial = items
            .iter()
            .enumerate()
            .filter_map(|(pos, it)| match it {
                Item::Plain { .. } => None,
                Item::Complex { te_span, .. } => Some((pos, te_span)),
            })
            .at_most_one()
            .map_err(|several| {
                // Report one error for each nontrivial expansion
                let mut several = several.map(|(_pos, span)| {
                    span.error("multiple nontrivial entries in ${paste ...}")
                });
                let mut collect = several.next().unwrap();
                collect.extend(several);
                collect
            })?
            .map(|(pos, _)| pos);

        fn plain_strs(items: &[Item]) -> impl Iterator<Item = &str> {
            items.iter().map(|item| match item {
                Item::Plain { text, .. } => text.as_str(),
                _ => panic!("non plain item"),
            })
        }

        fn mk_ident<'i>(
            out_span: Span,
            change_case: Option<ChangeCase>,
            items: impl Iterator<Item = &'i str>,
        ) -> syn::Result<syn::Ident> {
            let ident = items.collect::<String>();
            let ident = if let Some(change_case) = change_case {
                change_case.apply(&ident)
            } else {
                ident
            };
            catch_unwind(|| format_ident!("{}", ident, span = out_span))
                .map_err(|_| {
                    out_span.error(format_args!(
                        "pasted identifier {:?} is invalid",
                        ident
                    ))
                })
        }

        if let Some(nontrivial) = nontrivial {
            let mut items = items;
            let (items, items_after) = items.split_at_mut(nontrivial + 1);
            let (items_before, items) = items.split_at_mut(nontrivial);
            let nontrivial = &mut items[0];

            let mk_ident_nt = |text: &str| {
                mk_ident(
                    out_span,
                    change_case,
                    chain!(
                        plain_strs(items_before),
                        iter::once(text),
                        plain_strs(items_after),
                    ),
                )
            };

            match nontrivial {
                Item::Complex {
                    pre,
                    text,
                    post,
                    te_span: _,
                } => {
                    return Ok(Either::Right((
                        tspan,
                        mem::take(pre),
                        mk_ident_nt(text)?,
                        mem::take(post),
                    )))
                }
                Item::Plain { .. } => panic!("trivial nontrivial"),
            }
        } else {
            return Ok(Either::Left(
                mk_ident(out_span, change_case, plain_strs(&items))?, //
            ));
        }
    }
}

impl SubstParseContext for Items {
    type NotInPaste = Void;
    type NotInBool = ();
    type BoolOnly = Void;

    fn not_in_bool(_: &impl Spanned) -> syn::Result<()> {
        Ok(())
    }

    fn not_in_paste(span: &impl Spanned) -> syn::Result<Void> {
        Err(span
            .error("not allowed in within ${paste ...} (or case_changing)"))
    }
}

impl ExpansionOutput for Items {
    fn append_display<S: Display + Spanned>(&mut self, plain: &S) {
        self.append_display(plain);
    }
    fn append_identfrag_toks<I: quote::IdentFragment + ToTokens>(
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
        self.append_display(&AsIdentFragment(ident));
    }
    fn append_idpath<A, B>(
        &mut self,
        te_span: Span,
        pre_: A,
        ident: &syn::Ident,
        post_: B,
    ) where
        A: FnOnce(&mut TokenAccumulator),
        B: FnOnce(&mut TokenAccumulator),
    {
        let mut pre = TokenAccumulator::new();
        pre_(&mut pre);
        let mut post = TokenAccumulator::new();
        post_(&mut post);
        let text = ident.to_string();
        let mut handle_err = |prepost: TokenAccumulator| {
            prepost.tokens().unwrap_or_else(|err| {
                self.record_error(err);
                TokenStream::new()
            })
        };
        let pre = handle_err(pre);
        let post = handle_err(post);
        self.append_item(Item::Complex {
            pre,
            post,
            text,
            te_span,
        });
    }
    fn append_syn_lit(&mut self, lit: &syn::Lit) {
        use syn::Lit as L;
        match lit {
            L::Str(s) => self.append_item(Item::Plain {
                text: s.value(),
            }),
            L::Int(v) => self.append_display(v),
            L::Bool(v) => self.append_display(&v.value()),
            L::Verbatim(v) => self.append_display(v),
            x => self.write_error(
                x,
                "derive-adhoc macro wanted to do identifier pasting, but inappropriate literal provided",
            ),
        }
    }
    fn append_syn_type(&mut self, te_span: Span, ty: &syn::Type) {
        (|| {
            match ty {
                syn::Type::Path(path) => {
                    let mut path = path.clone();
                    let (last_segment, last_punct) = path
                        .path
                        .segments
                        .pop()
                        .ok_or_else(|| {
                            te_span.error(
                                "derive-adhoc token pasting applied to path with no components",
                            )
                        })?
                        .into_tuple();
                    let syn::PathSegment { ident, arguments } = last_segment;
                    let mut pre = TokenStream::new();
                    path.to_tokens(&mut pre);
                    let text = ident.to_string();
                    let mut post = TokenStream::new();
                    arguments.to_tokens(&mut post);
                    last_punct.to_tokens(&mut post);
                    let item = Item::Complex {
                        pre,
                        text,
                        post,
                        te_span,
                    };
                    self.append_item(item)
                }
                x => {
                    return Err(x.error(
                        "derive-adhoc macro wanted to do identifier pasting, but complex type provided",
                    ))
                }
            }
            Ok::<_, syn::Error>(())
        })()
        .unwrap_or_else(|e| self.record_error(e));
    }
    fn append_meta_value(
        &mut self,
        _tspan: Span,
        lit: &syn::Lit,
    ) -> syn::Result<()> {
        self.append_syn_lit(lit);
        Ok(())
    }
    fn append_tokens_with(
        &mut self,
        not_in_paste: &Void,
        _: impl FnOnce(&mut TokenAccumulator) -> syn::Result<()>,
    ) -> syn::Result<()> {
        void::unreachable(*not_in_paste)
    }

    fn append_bool_only(&mut self, bool_only: &Self::BoolOnly) -> ! {
        void::unreachable(*bool_only)
    }

    fn record_error(&mut self, err: syn::Error) {
        self.errors.push(err);
    }
}

impl Expand<Items> for TemplateElement<Items> {
    fn expand(&self, ctx: &Context, out: &mut Items) -> syn::Result<()> {
        match self {
            TE::Ident(ident) => out.append_identfrag_toks(&ident),
            TE::Literal(lit) => out.append_syn_lit(&lit),
            TE::Subst(e) => e.expand(ctx, out)?,
            TE::Repeat(e) => e.expand(ctx, out),
            TE::Punct(_, not_in_paste) | TE::Group { not_in_paste, .. } => {
                void::unreachable(*not_in_paste)
            }
        }
        Ok(())
    }
}
