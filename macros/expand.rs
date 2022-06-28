
use crate::prelude::*;

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
pub fn derive_adhoc_expand_func_macro(input: TokenStream)
                                      -> syn::Result<TokenStream> {
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
    // or Vec<TokenTree>, which we parse into a tree of our own full
    // of [TokenTree] ?
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
    Ok(output.into())
}
