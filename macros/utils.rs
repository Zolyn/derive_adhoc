use crate::prelude::*;
use proc_macro_crate::{crate_name, FoundCrate};

pub trait SpannedExt: Spanned {
    fn error<M: Display>(&self, m: M) -> syn::Error {
        syn::Error::new(self.span(), m)
    }
}

impl<T: Spanned> SpannedExt for T {}

pub trait TokenStreamExt: Extend<TokenStream> {
    fn write_error<S: Spanned, M: Display>(&mut self, s: &S, m: M) {
        self.extend([s.error(m).into_compile_error()])
    }
}

impl<T: Extend<TokenStream>> TokenStreamExt for T {}

#[derive(Debug, Default)]
pub struct ErrorAccumulator {
    bad: Option<syn::Error>,
    defused: bool,
}

impl ErrorAccumulator {
    pub fn handle_in<T, F>(&mut self, f: F) -> Option<T>
    where
        F: FnOnce() -> syn::Result<T>,
    {
        self.handle(f())
    }

    pub fn handle<T>(&mut self, result: syn::Result<T>) -> Option<T> {
        match result {
            Ok(y) => Some(y),
            Err(e) => {
                self.push(e);
                None
            }
        }
    }

    pub fn push(&mut self, err: syn::Error) {
        if let Some(bad) = &mut self.bad {
            bad.combine(err)
        } else {
            self.bad = Some(err);
        }
    }

    #[allow(dead_code)]
    pub fn finish(self) -> syn::Result<()> {
        self.finish_with(())
    }

    pub fn finish_with<T>(self, success: T) -> syn::Result<T> {
        match self.into_inner() {
            None => Ok(success),
            Some(bad) => Err(bad),
        }
    }

    pub fn into_inner(mut self) -> Option<syn::Error> {
        self.defused = true;
        self.bad.take()
    }
}

impl Drop for ErrorAccumulator {
    fn drop(&mut self) {
        assert!(self.defused);
    }
}

/// Return a full path to the location of `derive_adhoc_expand`.
pub fn expand_macro_name() -> Result<TokenStream, syn::Error> {
    match crate_name("derive-adhoc").or_else(|_| crate_name("derive-adhoc-macros")) {
        Ok(FoundCrate::Itself) => Ok(quote!( crate::derive_adhoc_expand )),
        Ok(FoundCrate::Name(name)) => {
            let ident = Ident::new(&name, Span::call_site());
            Ok(quote!( ::#ident::derive_adhoc_expand ))
        }
        Err(e) => {
            Err(syn::Error::new(
                Span::call_site(),
                format!("Expected derive-adhoc or derive-adhoc-macro to be present in Cargo.toml: {}", e)))
        }
    }
}
