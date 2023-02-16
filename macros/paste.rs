//! Implementation of identifier pasting expansion `${paste }`

use crate::framework::*;

/// Accumulator for things to be pasted
///
/// Implements [`ExpansionOutput`] and [`SubstParseContext`]:
/// i.e., it corresponds to the lexical context for a `${paste }`,
/// and collects the identifier fragments being pasted.
#[derive(Debug)]
pub struct Items<C = ()>
where
    C: CaseContext,
{
    data: ItemsData,
    /// The case change requested by the surrounding syntactic context.
    case: C::ChangeCase,
}

/// Accumulator for things to be pasted
///
/// This contains the actual fragments to be pasted.
/// Any case changing is done during assembly.
#[derive(Debug)]
pub struct ItemsData {
    span: Span,
    items: Vec<ItemEntry>,
    errors: Vec<syn::Error>,
}

#[derive(Debug)]
struct ItemEntry {
    item: Item,
    /// The case change to make to this identifier fragment
    ///
    /// We accumulate these, and apply them during assembly, because it is
    /// the assembly code which actually figures out what part of each
    /// `Item` is even a string for pasting (and case conversion).
    case: Option<ChangeCase>,
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
        te_span: Span,
    },
    Path {
        path: syn::TypePath,
        te_span: Span,
    },
}

#[derive(Debug, Default)]
pub struct WithinCaseContext;

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

define_cases! {
    // heck API       our keyword (and aliases)  our example(s)
    AsUpperCamelCase    "pascal_case"              "PascalCase"
                        "upper_camel_case"         "UpperCamelCase"    ,
    AsLowerCamelCase    "lower_camel_case"         "lowerCamelCase"    ,
    AsSnakeCase         "snake_case",
    AsShoutySnakeCase   "shouty_snake_case"        "SHOUTY_SNAKE_CASE" ,
}

/// Kind of lexical context in which we are pasting identifiers
///
/// Depends on whether we are trying to change the case.
///
/// There are two implementors:
///  * `()` for `${paste }`
///  * `WithinCase` for `${case }`
pub trait CaseContext: Sized + Default + Debug {
    /// The type representing what case change to perform, as parsed.
    type ChangeCase: Debug + Copy + Sized;
    /// Uninhabited iff this lexical context is within `${case }`
    ///
    /// `<Items as SubstParseContext>::NotInCase` delegates to this
    type NotInCase: Debug + Copy + Sized;
    /// Uninhabited unless lexical context allows other than a single subst
    ///
    /// `<Items as SubstParseContext>::AllowNonterminal` delegates to this
    type AllowNonterminal: Debug + Copy + Sized;
    /// Whether and how, in fact, to change the case, when expanding.
    fn push_case(case: Self::ChangeCase) -> Option<ChangeCase>;
    fn not_in_case(span: &impl Spanned) -> syn::Result<Self::NotInCase>;
    fn allow_nonterminal(
        span: &impl Spanned,
    ) -> syn::Result<Self::AllowNonterminal>;
    /// Expand a `${case }`
    ///
    /// `<Items as SubstParseContext>::expand_case` delegates to this
    ///
    /// `content` is a function which "expands" the contents of the
    /// `${paste }` using the content's `.expand()` method on the
    /// `Items`, so that the `push_*` methods on `Items` accumulate
    /// the identifier fragments to be pasted.
    ///
    /// The implementation for `WithinCase` is unreachable, because we
    /// lexically forbid nested `${case }`, since chained case
    /// conversions are a bad idea that we don't want to implement.
    fn expand_case<R>(
        outer: &mut Items<Self>,
        not_in_case: &Self::NotInCase,
        case: ChangeCase,
        content: impl FnOnce(&mut Items<WithinCaseContext>) -> R,
    ) -> R;
}

impl CaseContext for () {
    type ChangeCase = ();
    type NotInCase = ();
    type AllowNonterminal = ();
    fn push_case((): Self::ChangeCase) -> Option<ChangeCase> {
        None
    }
    fn not_in_case(_span: &impl Spanned) -> syn::Result<()> {
        Ok(())
    }
    fn allow_nonterminal(_span: &impl Spanned) -> syn::Result<()> {
        Ok(())
    }
    fn expand_case<R>(
        Items {
            data: outer,
            case: (),
        }: &mut Items,
        _not_in_case: &(),
        case: ChangeCase,
        f: impl FnOnce(&mut Items<WithinCaseContext>) -> R,
    ) -> R {
        let placeholder = ItemsData::new(outer.span);
        let inner = mem::replace(outer, placeholder);
        let mut inner = Items { data: inner, case };
        let r = catch_unwind(AssertUnwindSafe(|| f(&mut inner))).unwrap();
        *outer = inner.data;
        r
    }
}

impl CaseContext for WithinCaseContext {
    type ChangeCase = ChangeCase;
    type NotInCase = Void;
    type AllowNonterminal = Void;
    fn push_case(case: Self::ChangeCase) -> Option<ChangeCase> {
        Some(case)
    }
    fn not_in_case(span: &impl Spanned) -> syn::Result<Void> {
        Err(span.error("${case } may not be nested"))
    }
    fn allow_nonterminal(span: &impl Spanned) -> syn::Result<Void> {
        Err(span
            .error("${case } may contain only a single expansion (or token)"))
    }
    // We forbid ${pate } inside itself, because when we do case
    // conversion this will get very fiddly to implement.
    fn expand_case<R>(
        _: &mut Items<Self>,
        not_in_case: &Void,
        _case: ChangeCase,
        _f: impl FnOnce(&mut Items<WithinCaseContext>) -> R,
    ) -> R {
        void::unreachable(*not_in_case)
    }
}

impl Spanned for Item {
    fn span(&self) -> Span {
        match self {
            Item::Plain { span, .. } => *span,
            Item::IdPath { span, .. } => *span,
            Item::Path { path, .. } => path.span(),
        }
    }
}

impl Items<()> {
    pub fn new(span: Span) -> Items<()> {
        Items {
            data: ItemsData::new(span),
            case: (),
        }
    }
}

impl Items<WithinCaseContext> {
    pub fn new_case(span: Span, case: ChangeCase) -> Self {
        Items {
            data: ItemsData::new(span),
            case,
        }
    }
}

impl ItemsData {
    fn new(span: Span) -> Self {
        ItemsData {
            span,
            items: vec![],
            errors: vec![],
        }
    }
}

impl<C: CaseContext> Items<C> {
    fn push_item(&mut self, item: Item) {
        let case = C::push_case(self.case);
        self.data.items.push(ItemEntry { item, case });
    }
    fn push_lit_pair<V: Display, S: Spanned>(&mut self, v: &V, s: &S) {
        self.push_item(Item::Plain {
            text: v.to_string(),
            span: s.span(),
        })
    }
    pub fn push_fixed_string(&mut self, text: String, span: Span) {
        self.push_item(Item::Plain { text, span });
    }

    /// Combine the accumulated pieces and write them as tokens
    pub fn assemble(self, out: &mut TokenAccumulator) -> syn::Result<()> {
        self.data.assemble(out)
    }
}

impl ItemsData {
    fn assemble(self, out: &mut TokenAccumulator) -> syn::Result<()> {
        if !self.errors.is_empty() {
            for error in self.errors {
                out.record_error(error);
            }
            return Ok(());
        }

        let nontrivial = self
            .items
            .iter()
            .enumerate()
            .filter_map(|(pos, it)| match it.item {
                Item::Plain { .. } => None,
                Item::IdPath { te_span, .. } | Item::Path { te_span, .. } => {
                    Some((pos, te_span))
                }
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

        fn plain_strs(
            items: &[ItemEntry],
        ) -> impl Iterator<Item = (&str, Option<ChangeCase>)> {
            items.iter().map(|item| match &item.item {
                Item::Plain { text, .. } => (text.as_str(), item.case),
                _ => panic!("non plain item"),
            })
        }

        fn mk_ident<'i>(
            span: Span,
            items: impl Iterator<Item = (&'i str, Option<ChangeCase>)>,
        ) -> syn::Result<syn::Ident> {
            let items = items.map(|(s, case)| {
                if let Some(case) = case {
                    case.apply(s).into()
                } else {
                    Cow::Borrowed(s)
                }
            });
            let ident = items.collect::<String>();
            catch_unwind(|| format_ident!("{}", ident, span = span)).map_err(
                |_| {
                    span.error(format!(
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
            let nontrivial_case = nontrivial.case;

            let mk_ident_nt = |span, text: &str| {
                mk_ident(
                    span,
                    chain!(
                        plain_strs(items_before),
                        iter::once((text, nontrivial_case)),
                        plain_strs(items_after),
                    ),
                )
            };

            match &mut nontrivial.item {
                Item::IdPath {
                    pre,
                    text,
                    span,
                    post,
                    te_span: _,
                } => {
                    out.write_tokens(mem::take(pre));
                    out.write_tokens(mk_ident_nt(*span, text)?);
                    out.write_tokens(/*mem::take(*/ post /*)*/);
                }
                Item::Path { path, .. } => {
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
                .item
                .span();
            out.write_tokens(mk_ident(span, plain_strs(&self.items))?);
        }

        Ok(())
    }
}

impl<C: CaseContext> SubstParseContext for Items<C> {
    type NotInPaste = Void;
    type NotInBool = ();
    type BoolOnly = Void;
    type AllowNonterminal = C::AllowNonterminal;
    type NotInCase = C::NotInCase;

    fn not_in_bool(_: &impl Spanned) -> syn::Result<()> {
        Ok(())
    }
    fn not_in_case(span: &impl Spanned) -> syn::Result<Self::NotInCase> {
        C::not_in_case(span)
    }
    fn allow_nonterminal(
        span: &impl Spanned,
    ) -> syn::Result<C::AllowNonterminal> {
        C::allow_nonterminal(span)
    }

    fn not_in_paste(span: &impl Spanned) -> syn::Result<Void> {
        Err(span.error("not allowed in within ${paste ...} (or ${case })"))
    }
}

impl<C: CaseContext> ExpansionOutput for Items<C> {
    fn push_display<S: Display + Spanned>(&mut self, plain: &S) {
        self.push_lit_pair(plain, plain);
    }
    fn push_identfrag_toks<I: quote::IdentFragment + Spanned + ToTokens>(
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
    fn push_idpath<A, B>(
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
        let span = ident.span();
        let mut handle_err = |prepost: TokenAccumulator| {
            prepost.tokens().unwrap_or_else(|err| {
                self.record_error(err);
                TokenStream::new()
            })
        };
        let pre = handle_err(pre);
        let post = handle_err(post);
        self.push_item(Item::IdPath {
            pre,
            post,
            text,
            span,
            te_span,
        });
    }
    fn push_syn_lit(&mut self, lit: &syn::Lit) {
        use syn::Lit as L;
        match lit {
            L::Str(s) => self.push_item(Item::Plain {
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
    fn push_syn_type(&mut self, te_span: Span, ty: &syn::Type) {
        match ty {
            syn::Type::Path(path) => {
                self.push_item(Item::Path { te_span, path: path.clone() })
            },
            x => self.write_error(
                x,
                "derive-adhoc macro wanted to do identifier pasting, but complex type provided"
            ),
        }
    }
    fn push_attr_value(
        &mut self,
        _tspan: Span,
        lit: &syn::Lit,
    ) -> syn::Result<()> {
        self.push_syn_lit(lit);
        Ok(())
    }
    fn push_other_subst<F>(
        &mut self,
        not_in_paste: &Void,
        _: F,
    ) -> syn::Result<()>
    where
        F: FnOnce(&mut TokenAccumulator) -> syn::Result<()>,
    {
        void::unreachable(*not_in_paste)
    }

    // We forbid ${pate } inside itself, because when we do case
    // conversion this will get very fiddly to implement.
    fn expand_paste(
        &mut self,
        not_in_paste: &Void,
        _ctx: &Context,
        _span: Span,
        _paste_body: &Template<paste::Items>,
    ) -> syn::Result<()> {
        void::unreachable(*not_in_paste)
    }
    fn expand_case(
        &mut self,
        not_in_case: &Self::NotInCase,
        case: paste::ChangeCase,
        ctx: &Context,
        _span: Span,
        content: &Subst<paste::Items<WithinCaseContext>>,
    ) -> syn::Result<()> {
        C::expand_case(self, not_in_case, case, |out| content.expand(ctx, out))
    }
    fn expand_bool_only(&mut self, bool_only: &Self::BoolOnly) -> ! {
        void::unreachable(*bool_only)
    }

    fn record_error(&mut self, err: syn::Error) {
        self.data.errors.push(err);
    }
}

impl<C: CaseContext> Expand<Items<C>> for TemplateElement<Items<C>> {
    fn expand(&self, ctx: &Context, out: &mut Items<C>) -> syn::Result<()> {
        match self {
            TE::Ident(ident) => out.push_identfrag_toks(&ident),
            TE::Literal(lit) => out.push_syn_lit(&lit),
            TE::Subst(e) => e.expand(ctx, out)?,
            TE::Repeat(e) => e.expand(ctx, out),
            TE::Punct(_, not_in_paste) | TE::Group { not_in_paste, .. } => {
                void::unreachable(*not_in_paste)
            }
        }
        Ok(())
    }
}
