//! "Options", keyword modifiers for an expansion
//!
//! These "combine"

use crate::prelude::*;

use OptionDetails as OD;

//---------- types, ordered from general to specific ----------

/// All the template options, as a tokenstream, but sanity-checked
#[derive(Default, Debug, Clone)]
pub struct UnprocessedOptions(TokenStream);

/// All the template options, semantically resolved
#[derive(Default, Debug, Clone)]
pub struct DaOptions {
    pub driver_kind: Option<(ExpectedDriverKind, Span)>,
}

/// A single template option
#[derive(Debug)]
struct DaOption {
    pub kw_span: Span,
    pub od: OptionDetails,
}

/// Enum for the details of a template option
#[derive(Debug, Clone)]
#[allow(non_camel_case_types)] // clearer to use the exact ident
enum OptionDetails {
    // TODO DOCS, in template-syntax.md I guess
    For(ExpectedDriverKind),
}

/// The (single) expected driver kind
//
// At some future point, we may want `for ...` keywords that
// specify a range of possible drivers.  Then we'll need both
// this enum, and a *set* of it, and calculate the intersection
// in update_from_option.
#[derive(Debug, Clone, Copy, Eq, PartialEq, EnumString, Display)]
#[strum(serialize_all = "snake_case")]
pub enum ExpectedDriverKind {
    Struct,
    Enum,
    Union,
}

//---------- parsing ----------

impl Parse for UnprocessedOptions {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // Scan ahead for a syntax check
        DaOption::parse_several(&input.fork(), |_| Ok(()))?;

        // Collect everything until the : or =
        let mut out = TokenStream::new();
        while !(input.peek(Token![:]) || input.peek(Token![=])) {
            let tt: TokenTree = input.parse()?;
            out.extend([tt]);
        }
        Ok(UnprocessedOptions(out))
    }
}

impl Parse for DaOptions {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut self_ = DaOptions::default();
        DaOption::parse_several(input, |option| {
            self_.update_from_option(option)
        })?;
        Ok(self_)
    }
}

impl DaOption {
    fn parse_several(
        input: ParseStream,
        mut each: impl FnMut(DaOption) -> syn::Result<()>,
    ) -> syn::Result<()> {
        while input.peek(Ident::peek_any) {
            each(input.parse()?)?;
        }
        Ok(())
    }
}

impl Parse for DaOption {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let kw = input.call(syn::Ident::parse_any)?;

        let from_od = |od| {
            Ok(DaOption {
                kw_span: kw.span(),
                od,
            })
        };

        // keyword!{ KEYWORD [ {BLOCK WITH BINDINGS} ] [ CONSTRUCTOR-ARGS ] }
        // expands to   if ... { return ... }
        // KEYWORD can be "KEYWORD_STRING": CONSTRUCTOR
        macro_rules! keyword { { $($args:tt)* } => {
            keyword_general! { kw from_od OD; $($args)* }
        } }

        keyword! { "for": For(input.parse()?) }

        Err(kw.error("unknown derive-adhoc option"))
    }
}

impl Parse for ExpectedDriverKind {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let kw = input.call(Ident::parse_any)?;
        kw.to_string()
            .parse()
            .map_err(|_| kw.error("unknown value for `for`"))
    }
}

//---------- processing ----------

impl ToTokens for UnprocessedOptions {
    fn to_tokens(&self, out: &mut TokenStream) {
        out.extend(self.0.clone());
    }
}

impl UnprocessedOptions {
    #[allow(dead_code)] // Currently unused, retain it in case we need it
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl DaOptions {
    /// Update `self` according to the option specified in `option`
    ///
    /// On error (eg, contradictory options), fails.
    fn update_from_option(&mut self, option: DaOption) -> syn::Result<()> {
        fn store<T>(
            already: &mut Option<(T, Span)>,
            new: T,
            span: Span,
            on_contradiction: impl Display,
        ) -> syn::Result<()>
        where
            T: PartialEq,
        {
            match already {
                Some((already, _)) if already == &new => Ok(()),
                Some((_, already)) => {
                    Err([(*already, "first"), (span, "second")]
                        .error(on_contradiction))
                }
                None => {
                    *already = Some((new, span));
                    Ok(())
                }
            }
        }

        match option.od {
            OD::For(spec) => store(
                &mut self.driver_kind,
                spec,
                option.kw_span,
                "contradictory `for` options",
            ),
        }
    }
}
