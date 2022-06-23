//!

use quote::quote;

#[proc_macro]
pub fn derive_adhoc_expand(input: proc_macro::TokenStream)
                           -> proc_macro::TokenStream {
    dbg!(&input);
    eprintln!("---------- derive_adhoc_expand got start ----------");
    eprintln!("{}", &input);
    eprintln!("---------- derive_adhoc_expand got end ----------");
    quote!{ }.into()
}
