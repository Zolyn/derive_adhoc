
use crate::prelude::*;

struct ExpansionInput {
    #[allow(dead_code)]
    brace_token: token::Brace,
    driver: syn::DeriveInput,
    template: TokenStream,
}

impl Parse for ExpansionInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let driver;
        let brace_token = braced!(driver in input);
        let driver = driver.parse()?;
        let template = input.parse()?;
        Ok(ExpansionInput { brace_token, driver, template })
    }
}

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
    let input: ExpansionInput = syn::parse2(input)?;
    let ident = &input.driver.ident;
    dbg!(&ident);

    // maybe we should be using syn::buffer::TokenBuffer ?
    // or Vec<TokenTree>, which we parse into a tree of our own full
    // of [TokenTree] ?
    let mut output = TokenStream::new();
    let mut input = input.template.into_iter();
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
