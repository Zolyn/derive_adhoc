//! Macros for `derive_adhoc`
//
// This is the actual proc-macro crate.
//
// All it exports (or can export) are the proc macros themselves.
// Everything else that is `pub` could be written `pub(crate)`.

mod prelude;

use prelude::*;

// modules containing the actual implementations of our proc-macros
mod capture;
mod definition;
mod invocation;

// Implementation - common parts
mod framework;
mod utils;

// Implementation - specific areas
mod boolean;
mod expand;
mod paste;
mod repeat;
mod syntax;

// This calls the actual template engine.
//
// It should only get invoked by other macros; the user shouldn't need to
// touch it.
#[proc_macro]
#[doc(hidden)]
pub fn derive_adhoc_expand(
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let input: proc_macro2::TokenStream = input.into();
    let output = expand::derive_adhoc_expand_func_macro(input)
        .unwrap_or_else(|e| e.into_compile_error());
    output.into()
}

/// Expand an ad-hoc template, on a data structure decorated `#[derive(Adhoc)]`
///
/// ```
/// # use derive_adhoc_macros::{Adhoc, derive_adhoc, derive_adhoc_expand};
/// # #[derive(Adhoc)] struct DataStructureType;
/// # fn main() {
/// # const TEMPLATE: () = ();
/// derive_adhoc! {
///     DataStructureType:
///     TEMPLATE
/// }
/// # ;
/// # }
/// ```
///
/// Expands the template `TEMPLATE` for the type `DataStructureType`.
///
/// The definition of `DataStructureType` must have been decorated
/// with [`#[derive(Adhoc)]`](crate::Adhoc),
/// and the resulting `derive_adhoc_apply_TYPE` macro must be
/// available in scope.
///
/// The workhorse macro [`derive_adhoc_expand!`]
/// must be in scope in the crate root.
#[proc_macro]
pub fn derive_adhoc(
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let input = TokenStream::from(input);
    let output = invocation::derive_adhoc_func_macro(input)
        .unwrap_or_else(|e| e.into_compile_error());
    output.into()
}

/// Define a reuseable template
#[proc_macro]
pub fn define_derive_adhoc(
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let input = TokenStream::from(input);
    let output = definition::define_derive_adhoc_func_macro(input)
        .unwrap_or_else(|e| e.into_compile_error());
    output.into()
}

/// Perform ad-hoc templating driven by a data structure
///
/// This macro does two things:
///
///  1. It captures the data structure definition,
///     so that it can be used with calls to `derive_adhoc!`.
///
///  2. If `#[derive_adhoc(MyMacro)]` attributes are also specified,
///     they are taken to refer to reuseable templates
///     defined with `define_derive_adhoc!`.
///     Each such `MyMacro` is invoked on the data structure.
///
/// ## Captured data structure definition `derive_adhoc_apply_TYPE`
///
/// The data structure is captured by defining
/// a `macro_rules` macro called `derive_adhoc_apply_TYPE`,
/// where `TYPE` is the name of the type
/// that `#[derive(Adhoc)]` is applied to.
///
/// Like all macro_rules macros,
/// this lives at the top level of your crate.
/// So, sadly, its scoping and importing doesn't follow
/// that of your actual data type `TYPE`.
/// If you want to use it outside the containing module,
/// it must be explicitly imported.
/// For complicated reasons to do with Rust's macro name resolution,
/// a `use crate::*` won't necessarily work.
/// You may need to mention it by name explicitly.
///
/// There are also ordering requirements:
/// within a crate, the data structure definition
/// may need to precede its uses, in crate and module lexical order.
/// (This can even be affected by the order of *modules* within a crate.)
///
/// ## `#[adhoc]` attribute
///
/// The contents of `#[adhoc]` attributes are made available
/// to templates via the
/// [`${Xmeta}`](doc_template_syntax/index.html#derive_adhoc_syntax_Xmeta)
/// expansions.
///
/// If the template(s) don't use them, they are ignored.
/// `derive-adhoc` does not impose any namespacing within `#[adhoc]`:
/// all templates see the same adhoc meta attributes.
//
// This is the implementation of #[derive(Adhoc)]
//
// It should parse the struct name out of its input.
//
// The expansion should be
//   macro_rules! derive_adhoc_apply_ChannelsParams ...
// as per NOTES.txt
//
// For the MVP it does not need to have any attributes, but
// later it will want to be
//   #[proc_macro_derive(Adhoc, attributes(adhoc))]
// and then it will perhaps want to do *something* with the attributes?
// Although maybe just ignoring them and letting them get to the expander
// is right.
#[proc_macro_derive(Adhoc, attributes(adhoc, derive_adhoc))]
pub fn derive_adhoc_derive_macro(
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let input = TokenStream::from(input);
    let output = capture::derive_adhoc_derive_macro(input)
        .unwrap_or_else(|e| e.into_compile_error());
    output.into()
}
