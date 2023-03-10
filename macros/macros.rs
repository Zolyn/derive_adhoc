//! Macros for `derive_adhoc`
//!
//! **Import `derive_adhoc` instead.**
//
// This is the actual proc-macro crate.
//
// All it exports (or can export) are the proc macros themselves.
// Everything else that is `pub` could be written `pub(crate)`.

mod prelude;

use prelude::*;

// Implementation - common parts
#[macro_use]
mod utils;
mod framework;

// modules containing the actual implementations of our proc-macros
mod capture;
mod definition;
mod invocation;

// Implementation - specific areas
mod boolean;
mod expand;
mod options;
mod paste;
mod repeat;
mod syntax;

#[doc=include_str!("HACKING.md")]
mod _doc_hacking {}

#[doc=include_str!("NOTES.md")]
mod _doc_notes {}

//========== `expect`, the `check` module (or dummy version) ==========

// "expect" feature; module named check.rs for tab completion reasons
#[cfg(feature = "expect")]
mod check;
#[cfg(not(feature = "expect"))]
mod check {
    use crate::prelude::*;
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub struct Target(Void);

    impl FromStr for Target {
        type Err = Void;
        fn from_str(_: &str) -> Result<Self, Void> {
            panic!("output syntax checking not supported, enable `expect` feature of `derive-adhoc`")
        }
    }

    pub fn check_expected_target_syntax(
        _ctx: &framework::Context,
        _output: &mut TokenStream,
        target: DaOptVal<Target>,
    ) {
        void::unreachable(target.value.0)
    }
}
impl DaOptValDescribable for check::Target {
    const DESCRIPTION: &'static str =
        "expected output syntax (`expect` option)";
}

//========== actual macro entrypoints ==========

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
/// ```ignore
/// # use derive_adhoc_macros::{Adhoc, derive_adhoc, derive_adhoc_expand};
/// # #[derive(Adhoc)] struct DataStructureType;
/// # fn main() {
/// # const TEMPLATE: () = ();
/// derive_adhoc! {
///     DataStructureType OPTIONS,..:
///     TEMPLATE
/// }
/// # ;
/// # }
/// ```
///
/// Expands the template `TEMPLATE` for the type `DataStructureType`,
///
/// `OPTIONS,..` is an optional comma-separated list of
/// [expansion options](doc_template_syntax/index.html#expansion-options).
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
/// ```ignore
/// define_derive_adhoc! {
///     [pub] MyMacro OPTIONS,.. =
///     TEMPLATE
/// }
/// ```
///
/// Then, `MyMacro` can be used with
/// [`#[derive(Adhoc)]`](Adhoc)
/// `#[derive_adhoc(MyMacro)]`.
///
/// `OPTIONS,..` is an optional comma-separated list of
/// [expansion options](doc_template_syntax/index.html#expansion-options),
/// which will be applied whenever this template is expanded.
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
///
/// ## Exporting a template for use by other crates
///
/// With `pub MyMacro`, `define_derive_adhoc!` exports the macro
/// for use by other crates.
/// It is referred to in other crates
/// using `#[derive_ahdoc(this_crate::MyMacro)]`.
///
/// I.e., `pub MyMacro` causes the `derive_adhoc_template_MyMacro`
/// pattern macro to be exported with `#[macro_export]`.
///
/// Note that a template can only be exported at the crate top level,
/// not in a sub-module,
/// even if it is *defined* in a sub-module.
///
/// ### You must re-export `derive_adhoc`; (usually no) semver implications
///
/// When exporting a template to other crates, you must also
/// re-export `derive_adhoc`,
/// at the top level of your crate:
///
/// ```ignore
/// #[doc(hidden)]
/// pub use derive_adhoc;
/// ```
/// This is used to find the template expansion engine,
/// and will arrange that your template is expanded
/// by the right version of derive-adhoc.
/// The template syntax is that for *your* version of `derive-adhoc`,
/// even if the depending crate uses a different version of derive-adhoc.
///
/// You should *not* treat a breaking change
/// to derive-adhoc's template syntax
/// (which is a major change to derive-adhoc),
/// nor a requirement to use a newer template feature,
/// as a breaking changes in the API of your crate.
/// (You *should* use `#[doc(hidden)]`, or other approaches,
/// to discourage downstream crates from using
/// the derive-adhoc version you re-export.
/// Such use would be outside the semver guarantees.)
///
/// Changes that would require a semver bump
/// for all libraries that export templates,
/// will be rare, and specially marked in the derive-adhoc
/// changelog.
///
/// ### Namespacing within an exported template
///
/// Within the template,
/// items within your crate can be referred to with `$crate`.
///
/// For other items,
/// including from the standard library e.g., `std::option::Option`,
/// you may rely on the crate which uses the template
/// to have a reasonable namespace,
/// or use an explicit path starting with `std`
/// or `$crate` (perhaps naming a re-export).
///
/// Overall, the situation is similar to defining
/// an exported `macro_rules` macro.
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
///     so that it can be used with calls to [`derive_adhoc!`].
///
///  2. If `#[derive_adhoc(MyMacro)]` attributes are also specified,
///     they are taken to refer to reuseable templates
///     defined with [`define_derive_adhoc!`].
///     Each such `MyMacro` is applied to the data structure.
///
///     You can specify
///     [expansion options](doc_template_syntax/index.html#expansion-options)
///     for each such template application, by writing
///     `#[derive_adhoc(MyMacro[OPTIONS,..])]`, where
///     `[OPTIONS,..]` is a comma-separated list of expansion options
///     contained within `[ ]`.
///
///
/// ## Applying a template (derive-adhoc macro) from another crate
///
/// `#[derive_adhoc(some_crate::MyMacro)]`
/// applies an exported (`pub`) template
/// defined and exported by `some_crate`.
///
/// You can import a template from another crate,
/// so you can apply it with an unqualified name,
/// with `use`,
/// but you have to refer to
/// the actual pattern macro name `derive_adhoc_template_MyMacro`:
/// ```ignore
/// use other_crate::derive_adhoc_template_TheirMacro;
/// #[derive(Adhoc)]
/// #[derive_Adhoc(TheirMacro)]
/// struct MyStruct { // ...
/// # }
/// ```
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
/// [`${Xmeta}`](doc_template_syntax/index.html#tmeta-vmeta-fmeta---adhoc-attributes)
/// expansions.
///
/// If the template(s) don't use them, they are ignored.
/// `derive-adhoc` does not impose any namespacing within `#[adhoc]`:
/// all templates see the same adhoc meta attributes.
///
/// ## Exporting the driver for downstream crates' templates
///
// Really, the documentation about this in `pub-a.rs` and `pub-b.rs`,
// should be somewhere in our rustdoc output.
// But I don't want to put it *here* because it would completely
// dominate this macro documentation.
// So for now just reference the source tree docs.
// (We can't really easily provide even a link.)
// I think this is such a minority feature,
// that hiding the docs like this is OK.
//
/// To cause the macro embodying the driver struct to be exported,
/// write:
/// `#[derive_adhoc(pub)]`.
/// The driver can then be derived from in other crates,
/// with `derive_adhoc! { exporting_crate::DriverStruct: ... }`.
///
/// This is a tricky feature,
/// which should only be used by experts
/// who fully understand the implications.
/// It effectively turns the body of the struct into a macro,
/// with a brittle API
/// and very limited support for namespacing or hygiene.
///
/// See `pub mod a_driver` in the example file `pub-a.rs`,
/// in the source tree,
/// for a fuller discussion of the implications,
/// and some advice.
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
