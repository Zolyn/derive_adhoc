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

use proc_macro2::{TokenTree, Delimiter};

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
    let input: proc_macro2::TokenStream = input.into();
    let mut input = input.into_iter();

    let derive_input = input.next().unwrap();
    let derive_input = match derive_input {
        TokenTree::Group(g) => g,
        _ => panic!(),
    };
    match derive_input.delimiter() {
        Delimiter::Brace => { },
        _ => panic!(),
    }
    // Should parse it into a syn::DeriveInput, really
    let ident = derive_input.stream().into_iter().find_map(|tt| match tt {
        TokenTree::Ident(i) if i.to_string().starts_with('C')  => Some(i),
        _ => None,
    }).unwrap_or_else(|| panic!());
    dbg!(&ident);

    // maybe we should be using syn::buffer::TokenBuffer ?
    let mut output = TokenStream::new();
    while let Some(token) = input.next() {
        match token {
            TokenTree::Punct(p) if p.as_char() == '$' => {
                match input.next() {
                    Some(TokenTree::Ident(i)) if i.to_string() == "Struct" => {
                        output.extend([TokenTree::Ident(ident.clone())])
                    }
                    _ => panic!(),
                }
            }
            other => output.extend([other]),
        }
    }

    // obviously nothing should print to stderr
    dbg!(&&output);
    eprintln!("---------- derive_adhoc_expand got start ----------");
    eprintln!("{}", &output);
    eprintln!("---------- derive_adhoc_expand got end ----------");
    output.into()
}

// This is called by #[derive_adhoc]
//
// It should parse the struct name out of attr using darling
// and insert it into the output.
#[proc_macro_attribute]
pub fn derive_adhoc(_attr: proc_macro::TokenStream,
                    input: proc_macro::TokenStream)
                    -> proc_macro::TokenStream {
    let input = TokenStream::from(input);
    dbg!(&input);
    eprintln!("---------- derive_adhoc got start ----------");
    eprintln!("{}", &input);
    eprintln!("---------- derive_adhoc got end ----------");
    
/*
    quote!{
        derive_adhoc_apply_ChannelsParams!{
            #input
        }
    }.into()
     */
    quote!{ }.into()
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
pub fn derive_answer_fn(_item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    todo!()
}
