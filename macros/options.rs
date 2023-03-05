//! "Options", keyword modifiers for an expansion
//!
//! These "combine": multiple specifications of the same option
//! are allowed, so long as they are compatible.

#![allow(unused_macros, unused_imports, unused_variables, dead_code)]

use crate::prelude::*;

use OptionDetails as OD;

//---------- types, ordered from general to specific ----------

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
            Ok::<_, syn::Error>(DaOption {
                kw_span: kw.span(),
                od,
            })
        };

        // See keyword_general! in utils.rs
        macro_rules! keyword { { $($args:tt)* } => {
            keyword_general! { kw from_od OD; $($args)* }
        } }

        // Keyword parsing will go here

        Err(kw.error("unknown derive-adhoc option"))
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
        match option.od {
        }
    }
}
