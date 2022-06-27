//!

// All these functions' actual contents should be in a library module,
// which talks only about proc_macro2::TokenStream.
//
// This file, containing the invocations of #[proc_macro],
// should contain simply stubs (which convert back and froth
// between proc_macro::TokenStream and proc_macros::TokenStream.
//
// That way that library module can be tested, separately, not
// only in a proc macro context.

use quote::quote;
use proc_macro2::TokenStream as TokenStream;

// This should implement the actual template engine
//
// In my design, the input contains, firstly, literally the definition
// that #[derive(Adhoc)] was applied to (see NOTES.txt).
// Using the literal input, rather than some pre-parsed version, is
// slower, but means that we aren't inventing a nontrivial data format which
// potentially crosses crate boundaries with semver implications.
//
// We should start with a POC where the template engine does something
// totally trivial, but which does:
//   - depend on parsing the original derive macro input (struct def'n)
//   - treat $ in the template specially
//   - make output that replicates mostly the template
// Eg, how about making a thing where the templater just replaces
//   $ Struct
// with the original struct ident.
#[proc_macro]
pub fn derive_adhoc_expand(input: proc_macro::TokenStream)
                           -> proc_macro::TokenStream {
    // obviously nothing should print to stderr
    dbg!(&input);
    eprintln!("---------- derive_adhoc_expand got start ----------");
    eprintln!("{}", &input);
    eprintln!("---------- derive_adhoc_expand got end ----------");
    quote!{ }.into()
}

// This is called by #[derive_adhoc]
//
// It should parse the struct name out of attr using darling
// and insert it into the output.
#[proc_macro_attribute]
pub fn derive_adhoc(attr: proc_macro::TokenStream,
                    input: proc_macro::TokenStream)
                    -> proc_macro::TokenStream {
    let input = TokenStream::from(input);
    quote!{
        derive_adhoc_apply_ChannelsParams!{
            #input
        }
    }.into()
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
#[proc_macro_derive(Adhoc)]
pub fn derive_answer_fn(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    todo!()
}
