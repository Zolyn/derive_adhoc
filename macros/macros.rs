//!

mod prelude;
use prelude::*;

mod capture;
mod definition;
mod invocation;
mod utils;

mod framework;

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

// This is derive_adhoc!, the invocation macro
#[proc_macro]
pub fn derive_adhoc(
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let input = TokenStream::from(input);
    let output = invocation::derive_adhoc_func_macro(input)
        .unwrap_or_else(|e| e.into_compile_error());
    output.into()
}

// This is define_derive_adhoc!, the invocation macro
#[proc_macro]
pub fn define_derive_adhoc(
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let input = TokenStream::from(input);
    let output = definition::define_derive_adhoc_func_macro(input)
        .unwrap_or_else(|e| e.into_compile_error());
    output.into()
}

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
