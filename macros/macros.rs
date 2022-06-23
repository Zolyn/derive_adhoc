//!

#[proc_macro]
pub fn derive_adhoc_expand(input: proc_macro::TokenStream)
                           -> proc_macro::TokenStream {
    eprintln!("{:?}", &input);
    input
}
