//! private prelude for proc macro stuff

pub use std::convert::{TryFrom, TryInto};
pub use std::default::Default;
pub use std::fmt::{self, Display};
pub use std::mem;

pub use itertools::{izip, Itertools};
pub use proc_macro2::{Delimiter, Ident, Literal, Punct};
pub use proc_macro2::{Span, TokenStream, TokenTree};
pub use quote::{format_ident, quote, quote_spanned, ToTokens};
pub use strum::IntoEnumIterator as _;
pub use strum::{AsRefStr, Display, EnumIter, EnumString};
pub use syn::ext::IdentExt;
pub use syn::parse::{Parse, ParseStream};
pub use syn::punctuated::Punctuated;
pub use syn::spanned::Spanned;
pub use syn::Token;
pub use syn::{braced, parenthesized};
pub use syn::{parse_macro_input, token};
pub use void::{ResultVoidErrExt as _, ResultVoidExt as _, Void};

pub use TokenTree as TT;

pub use crate::utils::expand_macro_name;
pub use crate::utils::ErrorAccumulator;
pub use crate::utils::SpannedExt;
pub use crate::utils::ToTokensPunctComposable;

pub use crate::expand::*;
