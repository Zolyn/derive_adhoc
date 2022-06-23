//!

use quote::quote;
use proc_macro2::TokenStream as TokenStream;

#[proc_macro]
pub fn derive_adhoc_expand(input: proc_macro::TokenStream)
                           -> proc_macro::TokenStream {
    dbg!(&input);
    eprintln!("---------- derive_adhoc_expand got start ----------");
    eprintln!("{}", &input);
    eprintln!("---------- derive_adhoc_expand got end ----------");
    quote!{ }.into()
}

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
