//! private prelude for proc macro stuff
//!
//! Should be included with `use super::prelude::*`, not `crate::`,
//! so that it works with `tests/directly.rs` too.

pub use std::borrow::Cow;
pub use std::cell::RefCell;
pub use std::collections::{HashSet};
pub use std::convert::{TryFrom, TryInto};
pub use std::default::Default;
pub use std::fmt::{self, Debug, Display, Write as _};
pub use std::iter;
pub use std::marker::PhantomData;
pub use std::mem;
pub use std::ops::Deref;
pub use std::str::FromStr;
pub use std::thread::panicking;

pub use itertools::{chain, izip, Either, Itertools};
pub use proc_macro2::{Delimiter, Ident, Literal, Punct};
pub use proc_macro2::{Span, TokenStream, TokenTree};
pub use quote::{format_ident, quote, quote_spanned, ToTokens};
pub use strum::IntoEnumIterator as _;
pub use strum::{AsRefStr, Display, EnumCount, EnumDiscriminants};
pub use strum::{EnumIter, EnumString};
pub use syn::ext::IdentExt;
pub use syn::parse::{Lookahead1, Parse, ParseBuffer, ParseStream, Parser};
pub use syn::punctuated::Punctuated;
pub use syn::spanned::Spanned;
pub use syn::Token;
pub use syn::{braced, bracketed, parenthesized};
pub use syn::{parse_macro_input, token};
pub use void::{ResultVoidErrExt as _, ResultVoidExt as _, Void};

pub use TokenTree as TT;

pub use super::definition::escape_dollars;
pub use super::framework::TokenAccumulator;
pub use super::paste::TokenPastesAsIdent;
pub use super::utils::advise_incompatibility;
pub use super::utils::expand_macro_name;
pub use super::utils::ErrorAccumulator;
pub use super::utils::ToTokensPunctComposable;
pub use super::utils::{braced_group, delimit_token_group};
pub use super::utils::{Concatenated, Discard};
pub use super::utils::{ErrorLoc, IdentAny, MakeError};
pub(super) use super::{check, dbg_allkw, framework};

pub use super::expand::*;
pub use super::options::*;
