use crate::prelude::*;

pub trait JustSpannedExt: JustSpanned {
    fn error<M: Display>(&self, m: M) -> syn::Error {
        syn::Error::new(self.jspan(), m)
    }
}

impl<T: JustSpanned> JustSpannedExt for T {}

pub trait TokenStreamExt: Extend<TokenStream> {
    fn write_error<S: Spanned, M: Display>(&mut self, s: &S, m: M) {
        self.extend([s.error(m).into_compile_error()])
    }
}

impl<T: Extend<TokenStream>> TokenStreamExt for T {}

/// Like Spanned but not ToTokens
pub trait JustSpanned {
    fn jspan(&self) -> Span;
}

impl<T: Spanned> JustSpanned for T {
    fn jspan(&self) -> Span {
        Spanned::span(self)
    }
}
