//! Template syntax
//!
//! This module contains:
//!
//!  * The types representing a parsed template.
//!  * The `parse` methods.

use crate::framework::*;

pub use SubstDetails as SD;
pub use TemplateElement as TE;

#[derive(Debug)]
pub struct SubstInput {
    pub brace_token: token::Brace,
    pub driver: syn::DeriveInput,
    pub template: Template<TokenAccumulator>,
}

#[derive(Debug)]
pub struct Template<O: SubstParseContext> {
    pub elements: Vec<TemplateElement<O>>,
    pub no_nonterminal: O::NoNonterminal,
}

#[derive(Debug)]
pub enum TemplateElement<O: SubstParseContext> {
    Ident(Ident),
    Literal(syn::Lit),
    Punct(Punct, O::NoPaste),
    Group {
        /// Sadly Group's constructors let us only set *both* delimiters
        delim_span: Span,
        delimiter: Delimiter,
        template: Template<O>,
        no_paste: O::NoPaste,
    },
    Subst(Subst<O>),
    Repeat(RepeatedTemplate<O>),
}

#[derive(Debug)]
pub struct RepeatedTemplate<O: SubstParseContext> {
    pub template: Template<O>,
    #[allow(clippy::vec_box)]
    pub whens: Vec<Box<Subst<BooleanContext>>>,
    pub over: RepeatOver,
}

#[derive(Debug)]
pub struct Subst<O: SubstParseContext> {
    pub kw: Ident,
    pub sd: SubstDetails<O>,
    pub output_marker: PhantomData<O>,
}

/// Enum representing nature and payload of a substitution
///
/// This is a single enum, for all of the different lexical contexts
/// (principal expansion, identifier pasting, boolean predicates)
/// because this:
///   * Unifies the parsing code, ensuring that all these
///     constructs are parsed the same everywhere
///     (unless we deviate deliberately).
///   * Mostly unifies the expansion and loop/conditional walking code.
///   * Avoids the need to recapitulate the keywords in multiple
///     enums, or contexts with inner nested enums, or something.
///
/// The enum is generic over the lexical context [`SubstParseContext`].
/// This allows the variants that are inapplicable in a particular
/// lexical context to be made uninhabited:
/// that ensures that detection of such errors occurs during template parsing.
#[allow(non_camel_case_types)] // clearer to use the exact ident
#[derive(Debug)]
// TODO: This comment is largely obsolete,
// but we should review the ${if } parsing to see if it can be
// dealt with at parse type using `SubstParseContext`
//
// TODO: it might be good to separate this into separate enums for
// conditions and substitutions? -nickm
// I don't think so.  I unified these because the following places
// wanted to treat them very similarly:
//   - parsing
//   - iteration inspection
//   - attribute recursive descent matching
// Keeping them a single type avoids us making weird syntactic
// wrinkles (and may help avoid semantic wrinkles). -Diziet
//
// Hrm, actually, compare
//     ${if fmeta(foo) { ...
//     ${fmeta(foo) as ty}
// and this demonstrates different parsing.  We want to forbid
//     ${if fmeta(foo) as ty { ...
// and right now that is a bit funny.
pub enum SubstDetails<O: SubstParseContext> {
    // variables
    tname(O::NoBool),
    ttype(O::NoBool),
    vname(O::NoBool),
    fname(O::NoBool),
    ftype(O::NoBool),

    // attributes
    tmeta(SubstAttr),
    vmeta(SubstAttr),
    fmeta(SubstAttr),
    tattrs(RawAttr, O::NoPaste, O::NoBool),
    vattrs(RawAttr, O::NoPaste, O::NoBool),
    fattrs(RawAttr, O::NoPaste, O::NoBool),

    // generics
    tgens(O::NoPaste, O::NoBool),
    tgnames(O::NoPaste, O::NoBool),
    twheres(O::NoPaste, O::NoBool),

    // expansion manipulation
    paste(Template<paste::Items>, O::NoPaste, O::NoCase, O::NoBool),

    // special
    when(Box<Subst<BooleanContext>>, O::NoBool, O::NoNonterminal),

    // expressions
    False(O::BoolOnly),
    True(O::BoolOnly),
    not(Box<Subst<BooleanContext>>, O::BoolOnly),
    any(Punctuated<Subst<BooleanContext>, token::Comma>, O::BoolOnly),
    all(Punctuated<Subst<BooleanContext>, token::Comma>, O::BoolOnly),
    is_enum(O::BoolOnly),

    // Explicit iteration
    For(RepeatedTemplate<O>, O::NoBool),
    // Conditional substitution.
    If(SubstIf<O>, O::NoBool),
}

#[derive(Debug)]
pub struct SubstIf<O: SubstParseContext> {
    /// A series of test/result pairs.
    ///
    /// The test that gives "true"
    /// short-circuits the rest.
    pub tests: Vec<(Subst<BooleanContext>, Template<O>)>,
    /// A final element to expand if all tests fail.
    pub otherwise: Option<Box<Template<O>>>,
}

#[derive(Debug, Clone)]
pub struct SubstAttr {
    pub path: SubstAttrPath,
    pub as_: SubstAttrAs,
    pub as_span: Span,
}

#[derive(Debug, Clone, AsRefStr, Display, EnumIter)]
#[allow(non_camel_case_types)] // clearer to use the exact ident
pub enum SubstAttrAs {
    lit,
    ty,
}

#[derive(Debug, Clone)]
pub struct SubstAttrPath {
    pub path: syn::Path, // nonempty segments
    pub deeper: Option<Box<SubstAttrPath>>,
}

/// Parses `(foo,bar(baz),zonk="value")`
///
/// Like `NestedMeta` but doesn't allow lit,
/// since we forbid `#[adhoc("some")]`
/// And discards the paren and the `ahoc` introducer.
#[derive(Debug, Clone)]
struct AdhocAttrList {
    meta: Punctuated<syn::Meta, token::Comma>,
}

impl Parse for SubstInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let driver;
        let brace_token = braced!(driver in input);
        let driver = driver.parse()?;
        let template = Template::parse(input, ())?;
        Ok(SubstInput {
            brace_token,
            driver,
            template,
        })
    }
}

#[derive(Debug, Clone)]
pub enum RawAttr {
    Include {
        entries: Punctuated<RawAttrEntry, token::Comma>,
    },
    Exclude {
        exclusions: Punctuated<syn::Path, token::Comma>,
    },
}

#[derive(Debug, Clone)]
pub struct RawAttrEntry {
    pub path: syn::Path,
}

impl<O: SubstParseContext> Spanned for Subst<O> {
    fn span(&self) -> Span {
        self.kw.span()
    }
}

impl Spanned for SubstAttr {
    fn span(&self) -> Span {
        self.path.span()
    }
}

impl Spanned for SubstAttrPath {
    fn span(&self) -> Span {
        self.path.segments.first().expect("empty path!").span()
    }
}

impl<O: SubstParseContext> Template<O> {
    fn parse(
        input: ParseStream,
        no_nonterminal: O::NoNonterminal,
    ) -> syn::Result<Self> {
        // eprintln!("@@@@@@@@@@ PARSE {}", &input);
        let mut good = vec![];
        let mut errors = ErrorAccumulator::default();
        while !input.is_empty() {
            errors.handle_in(|| {
                let elem = input.parse()?;
                good.push(elem);
                Ok(())
            });
        }
        errors.finish_with(Template {
            elements: good,
            no_nonterminal,
        })
    }
}

impl<O: SubstParseContext> Parse for TemplateElement<O> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(match input.parse()? {
            TT::Group(group) => {
                let delim_span = group.span_open();
                let delimiter = group.delimiter();
                let no_nonterminal = O::no_nonterminal(&delim_span)?;
                let t_parser = |input: ParseStream| {
                    Template::parse(input, no_nonterminal)
                };
                let template = t_parser.parse2(group.stream())?;
                TE::Group {
                    delim_span,
                    delimiter,
                    template,
                    no_paste: O::no_paste(&delim_span)?,
                }
            }
            TT::Ident(tt) => TE::Ident(tt),
            tt @ TT::Literal(_) => TE::Literal(syn::parse2(tt.into())?),
            TT::Punct(tok) if tok.as_char() != '$' => {
                let span = tok.span();
                TE::Punct(tok, O::no_paste(&span)?)
            }
            TT::Punct(_dollar) => {
                let la = input.lookahead1();
                if la.peek(Token![$]) {
                    // $$
                    let dollar: Punct = input.parse()?;
                    let span = dollar.span();
                    TE::Punct(dollar, O::no_paste(&span)?)
                } else if la.peek(token::Brace) {
                    let exp;
                    struct Only<O: SubstParseContext>(Subst<O>);
                    impl<O: SubstParseContext> Parse for Only<O> {
                        fn parse(input: ParseStream) -> syn::Result<Self> {
                            let subst = input.parse()?;
                            let unwanted: Option<TT> = input.parse()?;
                            if let Some(unwanted) = unwanted {
                                return Err(unwanted.error(
                                    "unexpected arguments to expansion keyword"
                                ));
                            }
                            Ok(Only(subst))
                        }
                    }
                    let _brace = braced!(exp in input);
                    let exp = exp.parse()?;
                    let Only(exp) = exp;
                    TE::Subst(exp)
                } else if la.peek(token::Paren) {
                    RepeatedTemplate::parse_in_parens(input)?
                } else if la.peek(syn::Ident::peek_any) {
                    let exp: TokenTree = input.parse()?; // get it as TT
                    let exp = syn::parse2(exp.to_token_stream())?;
                    TE::Subst(exp)
                } else {
                    return Err(la.error());
                }
            }
        })
    }
}

impl Parse for AdhocAttrList {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let meta;
        let _paren = parenthesized!(meta in input);
        let meta = Punctuated::parse_terminated(&meta)?;
        Ok(AdhocAttrList { meta })
    }
}

impl Parse for SubstAttr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let path: SubstAttrPath = input.parse()?;

        let as_;
        let as_span;

        if input.peek(Token![as]) {
            let _: Token![as] = input.parse()?;
            let kw = input.call(syn::Ident::parse_any)?;
            as_span = kw.span();
            as_ = SubstAttrAs::iter().find(|as_| kw == as_).ok_or_else(
                || kw.error("unknown derive-adhoc 'as' syntax type keyword"),
            )?;
        } else {
            as_ = SubstAttrAs::lit;
            as_span = path.span();
        }

        Ok(SubstAttr { path, as_, as_span })
    }
}

impl Parse for SubstAttrPath {
    fn parse(outer: ParseStream) -> syn::Result<Self> {
        let input;
        let paren = parenthesized!(input in outer);
        let path = input.call(syn::Path::parse_mod_style)?;
        if path.segments.is_empty() {
            return Err(paren
                .span
                .error("adhoc attribute must have nonempty path"));
        }

        let deeper = if input.is_empty() {
            None
        } else {
            let deeper = input.parse()?;
            Some(Box::new(deeper))
        };

        Ok(SubstAttrPath { path, deeper })
    }
}

impl<O: SubstParseContext> Parse for Subst<O> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let kw = input.call(syn::Ident::parse_any)?;
        let output_marker = PhantomData;
        let from_sd = |sd| {
            Ok(Subst {
                sd,
                kw: kw.clone(),
                output_marker,
            })
        };

        // keyword!{ KEYWORD [ {BLOCK WITH BINDINGS} ] [ CONSTRUCTOR-ARGS ] }
        // expands to   if ... { return ... }
        macro_rules! keyword {
            { $kw:ident $( $ca:tt )? } => { keyword!{ @ $kw { } $( $ca )? } };
            { $kw:ident { $( $bindings:tt )* } $ca:tt } => {
                keyword!{ @ $kw { $( $bindings )* } $ca }
            };
            { @ $kw:ident { $( $bindings:tt )* } $( $constr_args:tt )? } => {
                if kw == stringify!($kw) {
                    $( $bindings )*
                    return from_sd(SD::$kw $( $constr_args )*);
                }
            };
        }

        let no_paste = O::no_paste(&kw);
        let no_case = O::no_case(&kw);
        let no_bool = O::no_bool(&kw);
        let bool_only = O::bool_only(&kw);
        let no_nonterminal = O::no_nonterminal(&kw);

        keyword! { tname(no_bool?) }
        keyword! { ttype(no_bool?) }
        keyword! { vname(no_bool?) }
        keyword! { fname(no_bool?) }
        keyword! { ftype(no_bool?) }
        keyword! { is_enum(bool_only?) }

        keyword! { tgens(no_paste?, no_bool?) }
        keyword! { tgnames(no_paste?, no_bool?) }
        keyword! { twheres(no_paste?, no_bool?) }

        keyword! { tmeta(input.parse()?) }
        keyword! { vmeta(input.parse()?) }
        keyword! { fmeta(input.parse()?) }

        keyword! { tattrs(input.parse()?, no_paste?, no_bool?) }
        keyword! { vattrs(input.parse()?, no_paste?, no_bool?) }
        keyword! { fattrs(input.parse()?, no_paste?, no_bool?) }

        keyword! {
            paste {
                let template = Template::parse(input, ())?;
            }
            (template, no_paste?, no_case?, no_bool?)
        }
        keyword! { when(input.parse()?, no_bool?, no_nonterminal?) }

        if kw == "false" {
            return from_sd(SD::False(bool_only?));
        }
        if kw == "true" {
            return from_sd(SD::True(bool_only?));
        }
        if kw == "if" {
            return from_sd(SD::If(
                SubstIf::parse(input, no_nonterminal?)?,
                no_bool?,
            ));
        }
        if kw == "for" {
            return from_sd(SD::For(
                RepeatedTemplate::parse_for(input)?,
                no_bool?,
            ));
        }

        keyword! {
            all {
                let inner;
                let _paren = parenthesized!(inner in input);
            }
            (Punctuated::parse_terminated(&inner)?, bool_only?)
        }
        keyword! {
            any {
                let inner;
                let _paren = parenthesized!(inner in input);
            }
            (Punctuated::parse_terminated(&inner)?, bool_only?)
        }
        keyword! {
            not {
                let inner;
                let _paren = parenthesized!(inner in input);
            }
            (inner.parse()?, bool_only?)
        }

        Err(kw.error("unknown derive-adhoc keyword"))
    }
}

impl<O: SubstParseContext> SubstIf<O> {
    fn parse(
        input: ParseStream,
        no_nonterminal: O::NoNonterminal,
    ) -> syn::Result<Self> {
        let mut tests = Vec::new();
        let mut otherwise = None;

        loop {
            let condition = input.parse()?;
            let content;
            let _br = braced![ content in input ];
            let consequence = Template::parse(&content, no_nonterminal)?;
            tests.push((condition, consequence));

            // (I'd like to use a lookahead here too, but it doesn't
            // accept "Nothing")
            if input.is_empty() {
                // no more conditions if there is not an "else"
                break;
            }
            let _else: Token![else] = input.parse()?;

            let lookahead = input.lookahead1();
            if lookahead.peek(Token![if]) {
                let _if: Token![if] = input.parse()?;
                // got an "else if": continue to the next iteration.
                continue;
            } else if lookahead.peek(token::Brace) {
                let content;
                let _br = braced![ content in input ];
                otherwise =
                    Some(Template::parse(&content, no_nonterminal)?.into());
                break; // no more input allowed.
            } else {
                return Err(lookahead.error());
            }
        }

        // TODO: Q: What ensures that there are no unhandled
        // tokens left for us?

        Ok(SubstIf { tests, otherwise })
    }
}

impl RawAttrEntry {
    fn simple(self, _negated: &Token![!]) -> syn::Result<syn::Path> {
        Ok(self.path)
    }
}

impl Parse for RawAttr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let la = input.lookahead1();
        let negated;
        if la.peek(Token![!]) {
            negated = Some(input.parse()?);
        } else if la.peek(Token![=]) {
            let _: Token![=] = input.parse()?;
            negated = None;
        } else {
            negated = None;
        }

        let entries: Punctuated<RawAttrEntry, _> =
            input.call(Punctuated::parse_terminated)?;

        if let Some(negated) = &negated {
            let exclusions = entries
                .into_iter()
                .map(|ent| ent.simple(negated))
                .try_collect()?;
            Ok(RawAttr::Exclude { exclusions })
        } else {
            Ok(RawAttr::Include { entries })
        }
    }
}

impl Parse for RawAttrEntry {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let path = input.parse()?;
        Ok(RawAttrEntry { path })
    }
}

impl<O: SubstParseContext> RepeatedTemplate<O> {
    fn parse_in_parens(input: ParseStream) -> syn::Result<TemplateElement<O>> {
        let template;
        let paren = parenthesized!(template in input);
        let rt = RepeatedTemplate::parse(&template, paren.span, None)?;
        Ok(TE::Repeat(rt))
    }

    fn parse_for(input: ParseStream) -> syn::Result<RepeatedTemplate<O>> {
        let over: Ident = input.parse()?;
        let over = if over == "fields" {
            RepeatOver::Fields
        } else if over == "variants" {
            RepeatOver::Variants
        } else {
            return Err(
                over.error("$for must be followed by 'fields' or 'variants'")
            );
        };
        let template;
        let brace = braced!(template in input);
        RepeatedTemplate::parse(&template, brace.span, Some(over))
    }

    fn parse(
        input: ParseStream,
        span: Span,
        over: Option<RepeatOver>,
    ) -> Result<RepeatedTemplate<O>, syn::Error> {
        let no_nonterminal = O::no_nonterminal(&span)?;
        let mut template = Template::parse(input, no_nonterminal)?;

        // split `when` (and [todo] `for`) off
        let mut whens = vec![];
        let mut elements = Vec::with_capacity(template.elements.len());
        let mut beginning = true;
        for elem in template.elements.drain(..) {
            let not_special = match elem {
                _ if !beginning => elem,
                TE::Ident(_) | TE::Literal(_) | TE::Punct(..) => elem,

                TE::Subst(subst) => match subst.sd {
                    SD::when(when, ..) => {
                        whens.push(when);
                        continue;
                    }
                    _ => TE::Subst(subst),
                },

                _ => {
                    beginning = false;
                    elem
                }
            };
            elements.push(not_special);
        }
        template.elements = elements;

        let over = match over {
            Some(over) => Ok(over),
            None => {
                let mut visitor = RepeatAnalysisVisitor::default();
                template.analyse_repeat(&mut visitor)?;
                visitor.finish(span)
            }
        };

        match over {
            Ok(over) => Ok(RepeatedTemplate {
                over,
                template,
                whens,
            }),
            Err(errs) => Err(errs),
        }
    }
}

pub fn preprocess_attrs(
    attrs: &[syn::Attribute],
) -> syn::Result<PreprocessedAttrs> {
    attrs
        .iter()
        .filter_map(|attr| {
            // infallible filtering for attributes we are interested in
            match attr.style {
                syn::AttrStyle::Outer => {}
                syn::AttrStyle::Inner(_) => return None,
            };
            if attr.path.leading_colon.is_some() {
                return None;
            }
            let segment = attr.path.segments.iter().exactly_one().ok()?;
            if segment.ident != "adhoc" {
                return None;
            }
            Some(attr)
        })
        .map(|attr| {
            let attr: AdhocAttrList = syn::parse2(attr.tokens.clone())?;
            Ok(attr.meta.into_iter())
        })
        .flatten_ok()
        .collect()
}

pub fn preprocess_fields(
    fields: &syn::Fields,
) -> syn::Result<Vec<PreprocessedField>> {
    let fields = match fields {
        syn::Fields::Named(f) => &f.named,
        syn::Fields::Unnamed(f) => &f.unnamed,
        syn::Fields::Unit => return Ok(vec![]),
    };
    fields
        .into_iter()
        .map(|field| {
            let pattrs = preprocess_attrs(&field.attrs)?;
            Ok(PreprocessedField { pattrs })
        })
        .collect()
}
