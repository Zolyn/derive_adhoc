//! private prelude for proc macro stuff

pub use std::convert::{TryFrom, TryInto};
pub use std::default::Default;
pub use std::fmt::Display;

pub use proc_macro2::{Span, TokenStream, TokenTree};
pub use proc_macro2::{Delimiter, Ident, Literal, Punct};
pub use quote::{format_ident, quote, ToTokens};
pub use strum::Display;
pub use syn::Token;
pub use syn::{braced, parenthesized};
pub use syn::ext::IdentExt;
pub use syn::parse::{Parse, ParseStream};
pub use syn::spanned::Spanned;
pub use syn::{parse_macro_input, token};

pub use TokenTree as TT;

pub use crate::utils::{SpannedExt as _, TokenStreamExt as _};
