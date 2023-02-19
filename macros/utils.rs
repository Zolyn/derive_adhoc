//! Utilities for proc macro implementation

//---------- misc ----------

/// Construct a braced group from a token expansion
///
/// Calls `f` to expand tokens, and returns a braced group containing
/// its output.
pub fn braced_group(
    brace_span: Span,
    f: impl FnOnce(&mut TokenAccumulator) -> syn::Result<()>,
) -> syn::Result<proc_macro2::Group> {
    delimit_token_group(Delimiter::Brace, brace_span, f)
}

pub fn delimit_token_group(
    delim: Delimiter,
    delim_span: Span,
    f: impl FnOnce(&mut TokenAccumulator) -> syn::Result<()>,
) -> syn::Result<proc_macro2::Group> {
    let mut out = TokenAccumulator::default();
    f(&mut out)?;
    let out = out.tokens()?;
    let mut out = proc_macro2::Group::new(delim, out);
    out.set_span(delim_span);
    Ok(out)
}

//---------- MakeErrorExt ----------

use crate::prelude::*;
use proc_macro_crate::{crate_name, FoundCrate};

pub trait MakeError {
    /// Convenience method to make an error
    fn error<M: Display>(&self, m: M) -> syn::Error;
}

impl<T: Spanned> MakeError for T {
    fn error<M: Display>(&self, m: M) -> syn::Error {
        syn::Error::new(self.span(), m)
    }
}

/// Error location: span and what role that span plays
///
/// Includes string indicating to the user
/// what kind of location this is.
/// For example, `(tspan, "template")`.
pub type ErrorLoc = (Span, &'static str);

/// Generates multiple copies of the error, for multiple places
///
/// # Panics
///
/// Panics if passed an empty slice.
impl MakeError for [ErrorLoc] {
    fn error<M: Display>(&self, m: M) -> syn::Error {
        let mut locs = self.into_iter().cloned();
        let mk = |(span, frag): (Span, _)| {
            span.error(format_args!("{} ({})", &m, frag))
        };
        let first = locs.next().expect("at least one span needed!");
        let mut build = mk(first);
        for rest in locs {
            build.combine(mk(rest))
        }
        build
    }
}

//---------- ToTokensPunctComposable ----------

/// Convert to a token stream in a way that composes nicely
pub trait ToTokensPunctComposable {
    fn to_tokens_punct_composable(&self, out: &mut TokenStream);
}
/// Ensure that there is a trailing punctuation if needed
impl<T, P> ToTokensPunctComposable for Punctuated<T, P>
where
    T: ToTokens,
    P: ToTokens + Default,
{
    fn to_tokens_punct_composable(&self, out: &mut TokenStream) {
        self.to_tokens(out);
        if !self.empty_or_trailing() {
            P::default().to_tokens(out)
        }
    }
}
/// Ensure that something is output, for punctuation `P`
///
/// Implemented for `Option<&&P>` because that's what you get from
/// `Punctuated::pairs().next().punct()`.
impl<P> ToTokensPunctComposable for Option<&&P>
where
    P: ToTokens,
    P: Default,
{
    fn to_tokens_punct_composable(&self, out: &mut TokenStream) {
        if let Some(self_) = self {
            self_.to_tokens(out)
        } else {
            P::default().to_tokens(out)
        }
    }
}

//---------- ErrorAccumulator ----------

/// Contains zero or more `syn::Error`
///
/// # Panics
///
/// Panics if dropped.
///
/// You must call one of the consuming methods, eg `finish`
#[derive(Debug, Default)]
pub struct ErrorAccumulator {
    bad: Option<syn::Error>,
    defused: bool,
}

impl ErrorAccumulator {
    /// Run `f`, accumulate any error, and return an `Ok`
    pub fn handle_in<T, F>(&mut self, f: F) -> Option<T>
    where
        F: FnOnce() -> syn::Result<T>,
    {
        self.handle(f())
    }

    /// Handle a `Result`: accumulate any error, and returni an `Ok`
    pub fn handle<T>(&mut self, result: syn::Result<T>) -> Option<T> {
        match result {
            Ok(y) => Some(y),
            Err(e) => {
                self.push(e);
                None
            }
        }
    }

    /// Accumulate an error
    pub fn push(&mut self, err: syn::Error) {
        if let Some(bad) = &mut self.bad {
            bad.combine(err)
        } else {
            self.bad = Some(err);
        }
    }

    /// If there were any errors, return a single error that combines them
    #[allow(dead_code)]
    pub fn finish(self) -> syn::Result<()> {
        self.finish_with(())
    }

    /// If there were any errors, return `Err`, otherwise `Ok(success)`
    pub fn finish_with<T>(self, success: T) -> syn::Result<T> {
        match self.into_inner() {
            None => Ok(success),
            Some(bad) => Err(bad),
        }
    }

    /// If there any errors, return a single error that combines them
    pub fn into_inner(mut self) -> Option<syn::Error> {
        self.defused = true;
        self.bad.take()
    }
}

impl Drop for ErrorAccumulator {
    fn drop(&mut self) {
        assert!(panicking() || self.defused);
    }
}

//---------- TokenAsIdent ----------

/// For use with `ExpansionOutput.push_identfrag_toks`
///
/// Will then write out `T` as its tokens.
/// In identifier pasting, converts the tokens to a string first
/// (so they had better be keywords).
pub struct TokenPastesAsIdent<T>(pub T);

impl<T: ToTokens> ToTokens for TokenPastesAsIdent<T> {
    fn to_tokens(&self, out: &mut TokenStream) {
        self.0.to_tokens(out)
    }
}
impl<T: ToTokens> quote::IdentFragment for TokenPastesAsIdent<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt(&self.0.to_token_stream(), f)
    }
}

//---------- expand_macro_name ----------

/// Return a full path to the location of `derive_adhoc_expand`.
pub fn expand_macro_name() -> Result<TokenStream, syn::Error> {
    let name = crate_name("derive-adhoc-macros")
        .or_else(|_| crate_name("derive-adhoc"));

    // See `tests/pub-export/pub-b/pub-b.rs`.  (The bizarre version
    // has a different crate name, which we must handle heree.)
    #[cfg(feature = "bizarre")]
    let name = name.or_else(|_| crate_name("bizarre-derive-adhoc"));

    match name {
        Ok(FoundCrate::Itself) => Ok(quote!( crate::derive_adhoc_expand )),
        Ok(FoundCrate::Name(name)) => {
            let ident = Ident::new(&name, Span::call_site());
            Ok(quote!( ::#ident::derive_adhoc_expand ))
        }
        Err(e) => Err(Span::call_site().error(
            format_args!("Expected derive-adhoc or derive-adhoc-macro to be present in Cargo.toml: {}", e)
        )),
    }
}
