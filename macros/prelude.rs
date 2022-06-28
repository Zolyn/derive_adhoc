//! private prelude for proc macro stuff

pub use proc_macro2::{Span, TokenStream, TokenTree};
pub use proc_macro2::{Delimiter, Ident, Literal, Punct};
pub use quote::{format_ident, quote, ToTokens};
pub use syn::Token;
pub use syn::braced;
pub use syn::ext::IdentExt;
pub use syn::parse::{Parse, ParseStream};
pub use syn::{parse_macro_input, token};

pub use TokenTree as TT;
