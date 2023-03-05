//! "Options", keyword modifiers for an expansion
//!
//! These "combine": multiple specifications of the same option
//! are allowed, so long as they are compatible.

use crate::prelude::*;

use OptionDetails as OD;

//---------- types, ordered from general to specific ----------

/// Where are we finding these options?
#[derive(Debug, Copy, Clone)]
pub enum OpContext {
    /// Just before the template
    ///
    /// `derivae_adhoc!{ Driver OPTIONS: ...`
    ///
    /// `define_derivae_adhoc!{ Template OPTIONS = ...`
    Template,
}

/// All the template options, as a tokenstream, but sanity-checked
///
/// These have been syntax checked, but not semantically checked.
/// The purpose of the syntax check is to get syntax errors early,
/// when a template is defined - rather than when it's applied.
///
/// This also helps with cross-crate compatibility.
#[derive(Default, Debug, Clone)]
pub struct UnprocessedOptions(TokenStream);

/// Template options, semantically resolved
#[derive(Default, Debug, Clone)]
pub struct DaOptions {
    pub dbg: bool,
    pub driver_kind: Option<(ExpectedDriverKind, Span)>,
}

/// A single template option
#[derive(Debug)]
struct DaOption {
    #[allow(dead_code)] // TODO maybe remove instead ?
    pub kw_span: Span,
    pub od: OptionDetails,
}

/// Enum for the details of a template option
#[derive(Debug, Clone)]
#[allow(non_camel_case_types)] // clearer to use the exact ident
enum OptionDetails {
    // TODO DOCS, in template-syntax.md I guess
    dbg,
    // TODO DOCS, in template-syntax.md I guess
    For((ExpectedDriverKind, Span)),
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

impl OpContext {
    fn allowed(self, _option: &DaOption) -> syn::Result<()> {
        Ok(())
    }
}

fn continue_options(input: ParseStream) -> Option<Lookahead1> {
    if input.is_empty() {
        return None;
    }
    let la = input.lookahead1();
    if la.peek(Token![:]) || la.peek(Token![=]) {
        return None;
    }
    Some(la)
}

impl UnprocessedOptions {
    pub fn parse(
        input: ParseStream,
        opcontext: OpContext,
    ) -> syn::Result<Self> {
        // Scan ahead for a syntax check
        DaOption::parse_several(&input.fork(), |opt| opcontext.allowed(&opt))?;

        // Collect everything until the : or =
        let mut out = TokenStream::new();
        while continue_options(input).is_some() {
            let tt: TokenTree = input.parse()?;
            out.extend([tt]);
        }
        Ok(UnprocessedOptions(out))
    }
}

impl DaOptions {
    pub fn parse_update(
        &mut self,
        input: ParseStream,
        opcontext: OpContext,
    ) -> syn::Result<()> {
        DaOption::parse_several(input, |option| {
            opcontext.check(&option)?;
            self.update_from_option(option)
        })
    }
}

impl DaOption {
    fn parse_several(
        input: ParseStream,
        mut each: impl FnMut(DaOption) -> syn::Result<()>,
    ) -> syn::Result<()> {
        while let Some(la) = continue_options(input) {
            if !la.peek(Ident::peek_any) {
                return Err(la.error());
            }
            each(input.parse()?)?;

            let la = if let Some(la) = continue_options(input) {
                la
            } else {
                break;
            };
            if !la.peek(Token![,]) {
                return Err(la.error());
            }
            let _: Token![,] = input.parse()?;
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

        // See keyword_general! in utils.rs
        macro_rules! keyword { { $($args:tt)* } => {
            keyword_general! { kw from_od OD; $($args)* }
        } }

        keyword! { dbg }
        keyword! { "for": For(ExpectedDriverKind::parse(input)?) }

        Err(kw.error("unknown derive-adhoc option"))
    }
}

impl ExpectedDriverKind {
    fn parse(input: ParseStream) -> syn::Result<(Self, Span)> {
        let kw = input.call(Ident::parse_any)?;
        let exp = kw
            .to_string()
            .parse()
            .map_err(|_| kw.error("unknown value for `for`"))?;
        let span = kw.span();
        Ok((exp, span))
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
            (new, span): (T, Span),
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

        Ok(match option.od {
            OD::dbg => self.dbg = true,
            OD::For(spec) => store(
                &mut self.driver_kind,
                spec,
                "contradictory `for` options",
            )?,
        })
    }
}
