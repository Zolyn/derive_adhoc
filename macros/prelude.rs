//! private prelude for proc macro stuff

pub use proc_macro2::{Delimiter, TokenStream, TokenTree};
pub use quote::quote;
pub use syn::Token;
pub use syn::parse::{Parse, ParseStream};
pub use syn::parse_macro_input;
