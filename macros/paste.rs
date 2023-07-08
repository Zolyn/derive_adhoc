//! Implementation of identifier pasting expansion `${paste }`

use super::framework::*;

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
    atoms: Vec<AtomForReport>,
}

/// One spanned input element, for use when reporting bad ident errors
///
/// We call this an "atom"; in nested pastes,
/// the atoms are the original input items.
#[derive(Debug, Clone)]
pub struct AtomForReport {
    text: String,
    span: Span,
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
        span: Option<Span>,
    },
    Complex {
        pre: TokenStream,
        text: String,
        post: TokenStream,
        te_span: Span,
    },
}

//---------- IdentFrag ----------

/// Uninhabited "bad identifier" error for conversions from already-tokens
#[derive(Copy, Clone, Debug)]
pub struct IdentFragInfallible(pub Void);

impl From<IdentFragInfallible> for syn::Error {
    fn from(i: IdentFragInfallible) -> syn::Error {
        void::unreachable(i.0)
    }
}

impl IdentFragInfallible {
    pub fn unreachable(&self) -> ! {
        void::unreachable(self.0)
    }
}

/// For use with `ExpansionOutput.append_identfrag_toks` etc.
///
/// Sort of like `quote::IdentFragment`.
/// But:
///
///  * Strings available directly, not via the inconvenient `fmt`,
///  * identifier construction doesn't involve `format_ident!` and panics.
///  * `paste::InputAtom` passed through separately,
///
/// The main purpose of this trait is to allow deferral of errors
/// from constructing bad identifiers.  Some *inputs* (implementors
/// of `IdentFrag`, typically those which are already tokens) can
/// infallibly be appended as tokens.
///
/// But paste results aren't converted to identifiers until the last
/// moment: they can'tbed converted infallibly, and the error surfaces
/// in `convert_to_ident`.
///
/// The framework methods `append_*ident*` propagate any error to the
/// call site.  Call sites which pass already-tokens can just use `?`
/// to convert the uninhabited error to `syn::Error` - or they can
/// use `IdentFragInfallible::unreachable`.
///
/// Call sites which pass actually-fallible content (ie, paste results)
/// end up with a `syn::Error`.
pub trait IdentFrag: Spanned {
    type BadIdent: Clone;

    /// (Try to) convert to tokens (ie, real `Ident`)
    ///
    /// Depending on the implementor, this might be fallible, or not.
    fn frag_to_tokens(
        &self,
        out: &mut TokenStream,
    ) -> Result<(), Self::BadIdent>;

    /// The fragment as a string
    fn fragment(&self) -> String;

    /// Transfer information about the atoms in this `IdentFrag` into `out`
    ///
    /// If, ultimately, a bad identifier is constructed using
    /// some of this input,
    /// these atoms will be reported.
    fn note_atoms(&self, out: &mut Vec<AtomForReport>) {
        out.push(AtomForReport {
            text: self.fragment(),
            span: self.span(),
        });
    }
}

impl<T: ToTokens + quote::IdentFragment + Display> IdentFrag for T {
    type BadIdent = IdentFragInfallible;
    fn frag_to_tokens(
        &self,
        out: &mut TokenStream,
    ) -> Result<(), IdentFragInfallible> {
        Ok(self.to_tokens(out))
    }

    fn fragment(&self) -> String {
        self.to_string()
    }
}

//---------- case conversion ----------

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
    AsUpperCamelCase    "pascal_case"         "upper_camel_case"   ,
    AsLowerCamelCase    "lower_camel_case"                         ,
    AsSnakeCase         "snake_case"                               ,
    AsShoutySnakeCase   "shouty_snake_case"                        ,
}

//---------- TokenPastesAsIdent ----------

/// For use with `ExpansionOutput.push_identfrag_toks`
///
/// Will then write out `T` as its tokens.
/// In identifier pasting, converts the tokens to a string first
/// (so they had better be identifiers, or ident fragments).
pub struct TokenPastesAsIdent<T>(pub T);

impl<T: ToTokens> Spanned for TokenPastesAsIdent<T> {
    fn span(&self) -> Span {
        self.0.span()
    }
}

impl<T: ToTokens> IdentFrag for TokenPastesAsIdent<T> {
    type BadIdent = IdentFragInfallible;

    fn frag_to_tokens(
        &self,
        out: &mut TokenStream,
    ) -> Result<(), Self::BadIdent> {
        Ok(self.0.to_tokens(out))
    }

    fn fragment(&self) -> String {
        self.0.to_token_stream().to_string()
    }
}

//---------- implementation ----------

impl Items {
    pub fn new(tspan: Span) -> Self {
        Items {
            tspan,
            items: vec![],
            errors: vec![],
            atoms: vec![],
        }
    }
}

/// Core of the results from mk_ident
#[derive(Debug)]
struct Pasted {
    /// String if we're to make an identifier, or for more pasting
    whole: String,
    /// Span if we're to make an identifier
    span: Span,
    /// What to consider complaining about if we can't make an identifier
    atoms: Vec<AtomForReport>,
}

impl Spanned for Pasted {
    fn span(&self) -> Span {
        self.span
    }
}
impl IdentFrag for Pasted {
    type BadIdent = syn::Error;

    fn fragment(&self) -> String {
        self.whole.clone()
    }

    fn note_atoms(&self, atoms: &mut Vec<AtomForReport>) {
        atoms.extend(self.atoms.iter().cloned())
    }

    fn frag_to_tokens(&self, out: &mut TokenStream) -> syn::Result<()> {
        let ident = convert_to_ident(self)?;
        ident.to_tokens(out);
        Ok(())
    }
}

/// Element of input to `mk_ident`: one bit of the leaf identifier
///
/// This is, essentially, the part of an `Item` which contributes to the
/// pasting.
type Piece<'i> = (&'i str, Option<Span>);

/// Make a leaf identifier out of pieces
///
/// Actually pastes together the pieces.
///
/// Any surrounding path elements, generics, etc., of a nontrivial
/// expansion, are handled by `assemble`, not here.
fn mk_ident<'i>(
    out_span: Span,
    change_case: Option<ChangeCase>,
    pieces: impl Iterator<Item = Piece<'i>> + Clone,
    atoms: Vec<AtomForReport>,
) -> Pasted {
    let whole = pieces.clone().map(|i| i.0).collect::<String>();
    let whole = if let Some(change_case) = change_case {
        change_case.apply(&whole)
    } else {
        whole
    };
    Pasted {
        span: out_span,
        whole,
        atoms,
    }
}

/// Error that stands in for the `syn::Error` from an invalid identifier
///
/// We don't expose this outside this module.
/// It's used internally in `convert_to_ident`, and in tests.
#[derive(Eq, PartialEq, Debug)]
struct InvalidIdent;

/// Obtain an actual `syn::Ident` from the results of pasting
//
/// The meat of `<Pasted as IdentFrag>::frag_to_tokens`.
/// Split off largely to save on rightward drift.
fn convert_to_ident(pasted: &Pasted) -> syn::Result<syn::Ident> {
    // Try to make an identifier from a string
    //
    // `format_ident!` and `Ident::new` and so on all panic if the
    // identifier is invalid.  That's quite inconvenient.  In particular,
    // it can result in tests spewing junk output with RUST_BACKTRACE=1.
    //
    // syn::parse_str isn't perfect either:
    //
    // 1. It accepts whitespace and perhaps other irregularities.
    //    We want to accept only precisely the identifier string.
    //
    // 2. It generates random extra errors, via some out of band means,
    //    if the string can't be tokenised.
    //    Eg, `<proc_macro2::TokenStream as FromStr>::parse("0_end")`
    //    generates a spurious complaint to stderr as well as
    //    a strange OK result containing a literal.
    //    This doesn't matter very much for our purposes because we
    //    never try to completely *swallow* a bad identifier error -
    //    we always surface an error of our own, and the extra one
    //    from parse_str is tolerable.
    //
    // 3. The syn::Error from an invalid identifier is not very illuminating.
    //    So we discard it, and replace it with our own.
    //
    // 4. Just parsing Ident won't accept keywords.  We could use
    //    IdentAny but that would give us keywords *as non-raw identifiers*
    //    but we need *raw* identifiers if the string was a keyword:
    //    i.e., in that case we want a raw identifier instead.
    //    (This can happen if pasting or case changing generates a keyword,
    //    or if a raw identifier euqal to a keyword is pasted with nothing.)
    let mut ident = (|| {
        let s = &pasted.whole;

        let prefixed;
        let (ident, comparator) = match syn::parse_str::<Ident>(s) {
            Ok(ident) => {
                // parse_str thought it was a valid identifier as-is
                (ident, s)
            }
            Err(_) => {
                // Problem 4 (needs raw) has arisen maybe?
                prefixed = format!("r#{}", s);
                let ident = syn::parse_str::<Ident>(&prefixed)
                    // Oh, it doesn't parse this way either, bail
                    .map_err(|_| InvalidIdent)?;
                (ident, &prefixed)
            }
        };

        // Check for problem 1 (accepting extraneous spaces etc.)
        if &ident.to_string() != comparator {
            return Err(InvalidIdent);
        }

        Ok(ident)
    })()
    .map_err(|_| {
        // Make our own error (see problem 3 above)
        let mut err = pasted.span.error(format_args!(
            "constructed identifier {:?} is invalid",
            &pasted.whole,
        ));
        // We want to show the user where the bad part is.  In
        // particular, if it came from somewhere nontrivial like an
        // ${Xmeta}.  But, we don't want to dump one error for every
        // input, because most of them will be harmless fixed
        // identifiers, in the template right next to the ${paste}.
        // So, try out each input bit and see if it would make an
        // identifier by itself.
        for (
            AtomForReport {
                text: piece,
                span: pspan,
            },
            pfx,
        ) in izip!(
            &pasted.atoms,
            // The first entry must be valid as an identifier start.
            // The subsequent entries, we prepend with "X".  If the first
            // entry was empty, that would be reported too.  This may
            // mean we make more reports than needed, which is why we say
            // "probably".
            chain!(iter::once(""), iter::repeat("X")),
        ) {
            // We accept keywords.  If the problem was that the output
            // was a keyword because one of the inputs was, we hope that
            // this is because one of the other inputs was empty.
            //
            // If the output was a keyword for some other reason, it
            // probably means the identifier construction scheme is
            // defective and hopefully the situation will be obvious to
            // the user.
            match syn::parse_str(&format!("{}{}", pfx, piece)) {
                Ok::<IdentAny, _>(_) => {}
                Err(_) => err
                    .combine(pspan.error(
                        "probably-invalid input to identifier pasting",
                    )),
            }
        }
        err
    })?;

    ident.set_span(pasted.span);
    Ok(ident)
}

#[test]
fn ident_from_str() {
    let span = Span::call_site();
    let chk = |s: &str, exp: Result<&str, _>| {
        let p = Pasted {
            whole: s.to_string(),
            span,
            atoms: vec![],
        };
        let parsed = convert_to_ident(&p)
            .map(|i| i.to_string())
            .map_err(|_| InvalidIdent);
        let exp = exp.map(|i| i.to_string());
        assert_eq!(parsed, exp);
    };
    let chk_ok = |s| chk(s, Ok(s));
    let chk_err = |s| chk(s, Err(InvalidIdent));

    chk("for", Ok("r#for"));
    chk_ok("r#for");
    chk_ok("_thing");
    chk_ok("thing_");
    chk_ok("r#raw");
    chk_err("");
    chk_err("a b");
    chk_err("spc ");
    chk_err(" spc");
    chk_err("r#a spc");
    chk_err(" r#a ");
    chk_err(" r#for ");
    chk_err("r#r#doubly_raw");
    chk_err("0");
}

pub fn expand(
    ctx: &Context<'_>,
    kw_span: Span,
    content: &Template<paste::Items>,
    out: &mut impl ExpansionOutput,
) -> syn::Result<()> {
    let mut items = paste::Items::new(kw_span);
    content.expand(ctx, &mut items);
    items.assemble(out, None)
}

impl Items {
    fn append_atom(&mut self, item: Item) {
        match &item {
            Item::Plain {
                text,
                span: Some(span),
                ..
            }
            | Item::Complex {
                text,
                te_span: span,
                ..
            } => {
                self.atoms.push(AtomForReport {
                    text: text.clone(),
                    span: *span,
                });
            }
            Item::Plain { span: None, .. } => {}
        };
        self.items.push(item);
    }
    fn append_item_raw(&mut self, item: Item) {
        self.items.push(item);
    }
    /// Append a plain entry from something `Display`
    ///
    /// Like `ExpansionOutput::append_display` but doesn't need `Spanned`
    fn append_plain<V: Display>(&mut self, span: Span, v: V) {
        self.append_atom(Item::Plain {
            text: v.to_string(),
            span: Some(span),
        })
    }
    pub fn append_fixed_string(&mut self, text: &'static str) {
        self.append_atom(Item::Plain {
            text: text.into(),
            span: None,
        })
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

        match Self::assemble_inner(
            self.tspan,
            self.items,
            change_case,
            self.atoms,
        )? {
            Either::Left(ident) => out.append_identfrag_toks(
                &ident, //
            )?,
            Either::Right((tspan, pre, ident, post)) => out.append_idpath(
                tspan,
                |ta| ta.append(pre),
                &ident,
                |ta| ta.append(post),
            )?,
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
        atoms: Vec<AtomForReport>,
    ) -> syn::Result<Either<Pasted, (Span, TokenStream, Pasted, TokenStream)>>
    {
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

        fn plain_strs(items: &[Item]) -> impl Iterator<Item = Piece> + Clone {
            items.iter().map(|item| match item {
                Item::Plain { text, span } => (text.as_str(), *span),
                _ => panic!("non plain item"),
            })
        }

        if let Some(nontrivial) = nontrivial {
            let mut items = items;
            let (items, items_after) = items.split_at_mut(nontrivial + 1);
            let (items_before, items) = items.split_at_mut(nontrivial);
            let nontrivial = &mut items[0];

            let mk_ident_nt = |(text, txspan): Piece, atoms| {
                mk_ident(
                    out_span,
                    change_case,
                    chain!(
                        plain_strs(items_before),
                        iter::once((text, txspan)),
                        plain_strs(items_after),
                    ),
                    atoms,
                )
            };

            match nontrivial {
                Item::Complex {
                    pre,
                    text,
                    post,
                    te_span,
                } => {
                    return Ok(Either::Right((
                        tspan,
                        mem::take(pre),
                        mk_ident_nt((text, Some(*te_span)), atoms),
                        mem::take(post),
                    )))
                }
                Item::Plain { .. } => panic!("trivial nontrivial"),
            }
        } else {
            return Ok(Either::Left(
                mk_ident(out_span, change_case, plain_strs(&items), atoms), //
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
        self.append_plain(plain.span(), plain);
    }
    fn append_identfrag_toks<I: IdentFrag>(
        &mut self,
        ident: &I,
    ) -> Result<(), I::BadIdent> {
        ident.note_atoms(&mut self.atoms);
        self.append_plain(ident.span(), ident.fragment());
        Ok(())
    }
    fn append_idpath<A, B, I>(
        &mut self,
        te_span: Span,
        pre_: A,
        ident: &I,
        post_: B,
    ) -> Result<(), I::BadIdent>
    where
        A: FnOnce(&mut TokenAccumulator),
        B: FnOnce(&mut TokenAccumulator),
        I: IdentFrag,
    {
        let mut pre = TokenAccumulator::new();
        pre_(&mut pre);
        let mut post = TokenAccumulator::new();
        post_(&mut post);
        let text = ident.fragment();
        let mut handle_err = |prepost: TokenAccumulator| {
            prepost.tokens().unwrap_or_else(|err| {
                self.record_error(err);
                TokenStream::new()
            })
        };
        let pre = handle_err(pre);
        let post = handle_err(post);
        ident.note_atoms(&mut self.atoms);
        self.append_item_raw(Item::Complex {
            pre,
            post,
            text,
            te_span,
        });
        Ok(())
    }
    fn append_syn_litstr(&mut self, lit: &syn::LitStr) {
        self.append_plain(lit.span(), lit.value());
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
                    self.append_atom(item)
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
            TE::Ident(ident) => out.append_identfrag_toks(&ident)?,
            TE::LitStr(lit) => out.append_syn_litstr(&lit),
            TE::Subst(e) => e.expand(ctx, out)?,
            TE::Repeat(e) => e.expand(ctx, out),
            TE::Literal(_, not_in_paste)
            | TE::Punct(_, not_in_paste)
            | TE::Group { not_in_paste, .. } => {
                void::unreachable(*not_in_paste)
            }
        }
        Ok(())
    }
}
