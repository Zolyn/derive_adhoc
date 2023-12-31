//! "Options", keyword modifiers for an expansion
//!
//! These "combine": multiple specifications of the same option
//! are allowed, so long as they are compatible.

use super::prelude::*;

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
    /// In the driver's application
    ///
    /// `#[derive_adhoc(Template[OPTIONS])]`
    DriverApplicationCapture,
    /// Driver's application options as they appear in the ultimate expansion
    ///
    /// With the versioning, so `1 1 AOPTIONS`.
    DriverApplicationPassed,
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
    pub driver_kind: Option<DaOptVal<ExpectedDriverKind>>,
    pub expect_target: Option<DaOptVal<check::Target>>,
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
    dbg,
    For(DaOptVal<ExpectedDriverKind>),
    // TODO should this be `producing` or something?
    expect(DaOptVal<check::Target>),
}

/// Value for an option
///
/// If `V` is `FromStr` and `DaOptValDescribable`,
/// this is `Parse`, taking a single keyword.
#[derive(Debug, Clone, Copy)]
pub struct DaOptVal<V> {
    pub value: V,
    pub span: Span,
}

/// Things that go into a `DaOptVal`
pub trait DaOptValDescribable {
    const DESCRIPTION: &'static str;
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

#[derive(Debug, Copy, Clone)]
pub struct OpCompatVersions {
    /// Increased when we make a wholly-incompatible change
    ///
    /// Bumping this will cause rejection of AOPTIONS by old d-a engines.
    /// Hopefully a newer d-a will able to cope with both.
    major: OpCompatVersionNumber,

    /// Increased when we make a more subtle change
    ///
    /// Current d-a versions will ignore this.
    /// Bumping it can be used to psas information
    /// from a newer capturing d-a to a newer template/driver d-a.
    minor: OpCompatVersionNumber,

    /// Span for error reporting
    span: Span,
}
type OpCompatVersionNumber = u32;
impl OpCompatVersions {
    pub fn ours() -> Self {
        OpCompatVersions {
            major: 1,
            minor: 0,
            span: Span::call_site(),
        }
    }
}

impl DaOptValDescribable for ExpectedDriverKind {
    const DESCRIPTION: &'static str = "expected driver kind (in `for` option)";
}

//---------- parsing ----------

impl OpContext {
    fn allowed(self, option: &DaOption) -> syn::Result<()> {
        use OpContext as OC;
        match &option.od {
            OD::dbg | //.
            OD::expect(..) => return Ok(()),
            OD::For(..) => {}
        }
        match self {
            OC::Template => Ok(()),
            OC::DriverApplicationCapture | OC::DriverApplicationPassed => {
                Err(option.kw_span.error(
                    "this derive-adhoc option is only supported in templates",
                ))
            }
        }
    }

    fn parse_versions(
        self,
        input: ParseStream,
    ) -> syn::Result<OpCompatVersions> {
        use OpContext as OC;
        let ours = OpCompatVersions::ours();
        let got = match self {
            OC::Template | OC::DriverApplicationCapture => ours,
            OC::DriverApplicationPassed => input.parse()?,
        };
        if got.major != ours.major {
            return Err(got.error(format_args!(
 "Incompatible major version for AOPTIONS (driver {}, template/engine {})",
                got.major,
                ours.major,
            )));
        }
        Ok(got)
    }
}

impl ToTokens for OpCompatVersions {
    fn to_tokens(&self, out: &mut TokenStream) {
        let OpCompatVersions { major, minor, span } = OpCompatVersions::ours();
        out.extend(quote_spanned! {span=> #major #minor });
    }
}

impl Parse for OpCompatVersions {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let number = move || {
            let lit: syn::LitInt = input.parse()?;
            Ok::<_, syn::Error>((lit.span(), lit.base10_parse()?))
        };
        let (span, major) = number()?;
        let (_, minor) = number()?;
        Ok(OpCompatVersions { major, minor, span })
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
        DaOption::parse_several(&input.fork(), opcontext, |_| Ok(()))?;

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
        DaOption::parse_several(input, opcontext, |option| {
            self.update_from_option(option)
        })
    }
}

impl DaOption {
    /// Parses zero or more options, in `opcontext`
    ///
    /// With `OpContext::DriverApplicationPassed`,
    /// expects to find the version information too.
    fn parse_several(
        input: ParseStream,
        opcontext: OpContext,
        mut each: impl FnMut(DaOption) -> syn::Result<()>,
    ) -> syn::Result<()> {
        let _versions = opcontext
            .parse_versions(input)
            .map_err(advise_incompatibility)?;

        while let Some(la) = continue_options(input) {
            if !la.peek(Ident::peek_any) {
                return Err(la.error());
            }
            let option = input.parse()?;
            opcontext.allowed(&option)?;
            each(option)?;

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
        let kw: IdentAny = input.parse()?;

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
        keyword! { "for": For(input.parse()?) }
        keyword! { expect(input.parse()?) }

        Err(kw.error("unknown derive-adhoc option"))
    }
}

impl<V: FromStr + DaOptValDescribable> Parse for DaOptVal<V> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let kw: IdentAny = input.parse()?;
        let value = kw.to_string().parse().map_err(|_| {
            kw.error(format_args!("unknown value for {}", V::DESCRIPTION))
        })?;
        let span = kw.span();
        Ok(DaOptVal { value, span })
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
        fn store<V>(
            already: &mut Option<DaOptVal<V>>,
            new: DaOptVal<V>,
        ) -> syn::Result<()>
        where
            V: PartialEq + DaOptValDescribable,
        {
            match already {
                Some(already) if already.value == new.value => Ok(()),
                Some(already) => {
                    Err([(already.span, "first"), (new.span, "second")].error(
                        format_args!(
                            "contradictory values for {}",
                            V::DESCRIPTION,
                        ),
                    ))
                }
                None => {
                    *already = Some(new);
                    Ok(())
                }
            }
        }

        Ok(match option.od {
            OD::dbg => self.dbg = true,
            OD::expect(spec) => store(&mut self.expect_target, spec)?,
            OD::For(spec) => store(&mut self.driver_kind, spec)?,
        })
    }
}
