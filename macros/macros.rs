//! Macros for `derive_adhoc`
//!
//! Import `derive_adhoc` instead.
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

#[doc=include_str!("../NOTES.md")]
mod notes {}

/// Template expansion engine, internal
///
/// Normally you do not need to mention this macro.
///
/// derive-adhoc does its work by
/// (defining and then) invoking various interrelated macros
/// including `macro_rules` macros and proc macros.
/// These ultimately end up calling this macro,
/// which takes a template and a data structure,
/// and expands the template for that data structure.
///
/// This macro's behvaiour is not currently stable or documented.
/// If you invoke it yourself, you get to keep all the pieces.
#[proc_macro]
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
/// and the resulting `derive_adhoc_driver_TYPE` macro must be
/// available in scope.
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
///
/// ```
/// # use derive_adhoc_macros::define_derive_adhoc;
/// define_derive_adhoc! {
///     MyMacro =
///     TEMPLATE
/// }
/// ```
///
/// Then, `MyMacro` can be used with
/// `#[derive(Adhoc)] #[derive_adhoc(MyMacro)]`.
///
/// ## Captured template macro `derive_adhoc_template_MyMacro`
///
/// The template is captured as a `macro_rules` macro
/// named `derive_adhoc_template_MyMacro`.
/// This macro must be in scope when you try to use it
/// with `#[derive_adhoc(MyMacro)]`.
///
/// The rules for scoping and ordering of `macro_rules` macros
/// are subtle.
/// For more information, see the
/// [documentation for `#[derive(Adhoc)]`](derive.Adhoc.html#captured-data-structure-definition-derive_adhoc_driver_type).
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
/// ## Captured data structure definition `derive_adhoc_driver_TYPE`
///
/// The data structure is captured by defining
/// a `macro_rules` macro called `derive_adhoc_driver_TYPE`,
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
//   macro_rules! derive_adhoc_driver_ChannelsParams ...
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
