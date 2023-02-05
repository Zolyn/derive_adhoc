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
pub struct Template<O: SubstParseContext> {
    pub elements: Vec<TemplateElement<O>>,
    pub allow_nonterminal: O::AllowNonterminal,
}

#[derive(Debug)]
pub enum TemplateElement<O: SubstParseContext> {
    Ident(Ident),
    Literal(syn::Lit),
    Punct(Punct, O::NotInPaste),
    Group {
        /// Sadly Group's constructors let us only set *both* delimiters
        delim_span: Span,
        delimiter: Delimiter,
        template: Template<O>,
        not_in_paste: O::NotInPaste,
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
    tname(O::NotInBool),
    ttype(O::NotInBool),
    vname(O::NotInBool),
    fname(O::NotInBool),
    ftype(O::NotInBool),

    // attributes
    tmeta(SubstAttr),
    vmeta(SubstAttr),
    fmeta(SubstAttr),
    tattrs(RawAttr, O::NotInPaste, O::NotInBool),
    vattrs(RawAttr, O::NotInPaste, O::NotInBool),
    fattrs(RawAttr, O::NotInPaste, O::NotInBool),

    // generics
    tgens(O::NotInPaste, O::NotInBool),
    tgnames(O::NotInPaste, O::NotInBool),
    twheres(O::NotInPaste, O::NotInBool),

    // expansion manipulation
    paste(
        Template<paste::Items>,
        O::NotInPaste,
        O::NotInCase,
        O::NotInBool,
    ),
    ChangeCase(
        Box<Subst<paste::Items<paste::WithinCaseContext>>>,
        paste::ChangeCase,
        O::NotInCase,
        O::NotInBool,
    ),

    // special
    when(
        Box<Subst<BooleanContext>>,
        O::NotInBool,
        O::AllowNonterminal,
    ),

    // expressions
    False(O::BoolOnly),
    True(O::BoolOnly),
    not(Box<Subst<BooleanContext>>, O::BoolOnly),
    any(Punctuated<Subst<BooleanContext>, token::Comma>, O::BoolOnly),
    all(Punctuated<Subst<BooleanContext>, token::Comma>, O::BoolOnly),
    is_enum(O::BoolOnly),

    // Explicit iteration
    For(RepeatedTemplate<O>, O::NotInBool),
    // Conditional substitution.
    If(SubstIf<O>, O::NotInBool),
    select1(SubstIf<O>, O::NotInBool),
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
    pub kw_span: Span,
}

#[derive(Debug, Clone)]
pub struct SubstAttr {
    pub path: SubstAttrPath,
    pub as_: Option<SubstAttrAs>,
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
        let driver_brace = braced!(driver in input);
        let driver = driver.parse()?;
        let template;
        let template_brace = braced!(template in input);
        let template = Template::parse(&template, ())?;
        Ok(SubstInput {
            driver_brace,
            driver,
            template_brace,
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
        allow_nonterminal: O::AllowNonterminal,
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
            allow_nonterminal,
        })
    }
}

impl<O: SubstParseContext> Parse for TemplateElement<O> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(match input.parse()? {
            TT::Group(group) => {
                let delim_span = group.span_open();
                let delimiter = group.delimiter();
                let allow_nonterminal = O::allow_nonterminal(&delim_span)?;
                let t_parser = |input: ParseStream| {
                    Template::parse(input, allow_nonterminal)
                };
                let template = t_parser.parse2(group.stream())?;
                TE::Group {
                    delim_span,
                    delimiter,
                    template,
                    not_in_paste: O::not_in_paste(&delim_span)?,
                }
            }
            TT::Ident(tt) => TE::Ident(tt),
            tt @ TT::Literal(_) => TE::Literal(syn::parse2(tt.into())?),
            TT::Punct(tok) if tok.as_char() != '$' => {
                let span = tok.span();
                TE::Punct(tok, O::not_in_paste(&span)?)
            }
            TT::Punct(_dollar) => {
                let la = input.lookahead1();
                if la.peek(Token![$]) {
                    // $$
                    let dollar: Punct = input.parse()?;
                    let span = dollar.span();
                    TE::Punct(dollar, O::not_in_paste(&span)?)
                } else if la.peek(token::Paren) {
                    RepeatedTemplate::parse_in_parens(input)?
                } else {
                    TE::Subst(Subst::parse_after_dollar(la, input)?)
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
            as_ = Some(SubstAttrAs::iter().find(|as_| kw == as_).ok_or_else(
                || kw.error("unknown derive-adhoc 'as' syntax type keyword"),
            )?);
        } else {
            as_ = None;
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

impl<O: SubstParseContext> Subst<O> {
    /// Parses everything including a `$` (which we insist on)
    fn parse_entire(input: ParseStream) -> syn::Result<Self> {
        let _dollar: Token![$] = input.parse()?;
        let la = input.lookahead1();
        Self::parse_after_dollar(la, input)
    }

    /// Parses everything after the `$`, possibly including a pair of `{ }`
    fn parse_after_dollar(
        la: Lookahead1,
        input: ParseStream,
    ) -> syn::Result<Self> {
        if la.peek(token::Brace) {
            let exp;
            struct Only<O: SubstParseContext>(Subst<O>);
            impl<O: SubstParseContext> Parse for Only<O> {
                fn parse(input: ParseStream) -> syn::Result<Self> {
                    let subst = input.parse()?;
                    let unwanted: Option<TT> = input.parse()?;
                    if let Some(unwanted) = unwanted {
                        return Err(unwanted.error(
                            "unexpected arguments to expansion keyword",
                        ));
                    }
                    Ok(Only(subst))
                }
            }
            let _brace = braced!(exp in input);
            let exp = exp.parse()?;
            let Only(exp) = exp;
            Ok(exp)
        } else if la.peek(syn::Ident::peek_any) {
            let exp: TokenTree = input.parse()?; // get it as TT
            let exp = syn::parse2(exp.to_token_stream())?;
            Ok(exp)
        } else {
            return Err(la.error());
        }
    }
}

/// Parses only the content (ie, after the `$` and inside any `{ }`)
impl<O: SubstParseContext> Parse for Subst<O> {
    fn parse<'i>(input: ParseStream<'i>) -> syn::Result<Self> {
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
        // KEYWORD can be "KEYWORD_STRING": CONSTRUCTOR
        macro_rules! keyword {
            { $kw:ident $( $rest:tt )* } => {
                keyword!{ @ 1 stringify!($kw), $kw, $($rest)* }
            };
            { $kw:literal: $constr:ident $( $rest:tt )* } => {
                keyword!{ @ 1 $kw, $constr, $($rest)* }
            };
            { @ 1 $kw:expr, $constr:ident, $( $ca:tt )? } => {
                keyword!{ @ 2 $kw, $constr, { } $( $ca )? }
            };
            { @ 1 $kw:expr, $constr:ident, { $( $bindings:tt )* } $ca:tt } => {
                keyword!{ @ 2 $kw, $constr, { $( $bindings )* } $ca }
            };
            { @ 2 $kw:expr, $constr:ident,
              { $( $bindings:tt )* } $( $constr_args:tt )?
            } => {
                if kw == $kw {
                    $( $bindings )*
                    return from_sd(SD::$constr $( $constr_args )*);
                }
            };
        }

        let not_in_paste = O::not_in_paste(&kw);
        let not_in_case = O::not_in_case(&kw);
        let not_in_bool = O::not_in_bool(&kw);
        let bool_only = O::bool_only(&kw);
        let allow_nonterminal = O::allow_nonterminal(&kw);

        let parse_if = |input| {
            SubstIf::parse(input, kw.span(), allow_nonterminal.clone()?)
        };

        let in_parens = |input: ParseStream<'i>| {
            let inner;
            let _paren = parenthesized!(inner in input);
            Ok(inner)
        };

        keyword! { tname(not_in_bool?) }
        keyword! { ttype(not_in_bool?) }
        keyword! { vname(not_in_bool?) }
        keyword! { fname(not_in_bool?) }
        keyword! { ftype(not_in_bool?) }
        keyword! { is_enum(bool_only?) }

        keyword! { tgens(not_in_paste?, not_in_bool?) }
        keyword! { tgnames(not_in_paste?, not_in_bool?) }
        keyword! { twheres(not_in_paste?, not_in_bool?) }

        keyword! { tmeta(input.parse()?) }
        keyword! { vmeta(input.parse()?) }
        keyword! { fmeta(input.parse()?) }

        keyword! { tattrs(input.parse()?, not_in_paste?, not_in_bool?) }
        keyword! { vattrs(input.parse()?, not_in_paste?, not_in_bool?) }
        keyword! { fattrs(input.parse()?, not_in_paste?, not_in_bool?) }

        keyword! {
            paste {
                let template = Template::parse(input, ())?;
            }
            (template, not_in_paste?, not_in_case?, not_in_bool?)
        }
        keyword! { when(input.parse()?, not_in_bool?, allow_nonterminal?) }

        keyword! { "false": False(bool_only?) }
        keyword! { "true": True(bool_only?) }
        keyword! { "if": If(parse_if(input)?, not_in_bool?) }
        keyword! { select1(parse_if(input)?, not_in_bool?) }

        keyword! { "for": For(
            RepeatedTemplate::parse_for(input)?,
            not_in_bool?,
        )}

        let any_all_contents = |input: ParseStream<'i>| {
            Punctuated::parse_terminated(&in_parens(input)?)
        };
        keyword! { any(any_all_contents(input)?, bool_only?) }
        keyword! { all(any_all_contents(input)?, bool_only?) }
        keyword! { not(in_parens(input)?.parse()?, bool_only?) }

        if let Ok(case) = kw.to_string().parse() {
            return from_sd(SD::ChangeCase(
                Box::new(Subst::parse_entire(input)?),
                case,
                not_in_case?,
                not_in_bool?,
            ));
        }

        Err(kw.error("unknown derive-adhoc keyword"))
    }
}

impl<O: SubstParseContext> SubstIf<O> {
    fn parse(
        input: ParseStream,
        kw_span: Span,
        not_in_nonterminal: O::AllowNonterminal,
    ) -> syn::Result<Self> {
        let mut tests = Vec::new();
        let mut otherwise = None;

        loop {
            let condition = input.parse()?;
            let content;
            let _br = braced![ content in input ];
            let consequence = Template::parse(&content, not_in_nonterminal)?;
            tests.push((condition, consequence));

            // (I'd like to use a lookahead here too, but it doesn't
            // accept "Nothing")
            if input.is_empty() {
                // no more conditions if there is not an "else"
                break;
            }

            let lookahead1 = input.lookahead1();
            if lookahead1.peek(syn::Ident) {
                // this is another expansion keyword, then
                // skipped `else if`
                continue;
            } else if lookahead1.peek(Token![else]) {
                // else clause
            } else {
                return Err(lookahead1.error());
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
                otherwise = Some(
                    Template::parse(&content, not_in_nonterminal)?.into(),
                );
                break; // no more input allowed.
            } else {
                return Err(lookahead.error());
            }
        }

        // TODO: Q: What ensures that there are no unhandled
        // tokens left for us?

        Ok(SubstIf {
            kw_span,
            tests,
            otherwise,
        })
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
        let allow_nonterminal = O::allow_nonterminal(&span)?;
        let mut template = Template::parse(input, allow_nonterminal)?;

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
