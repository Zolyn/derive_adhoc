use crate::prelude::*;

// This is #[derive(Adhoc)]
pub fn derive_adhoc_derive_macro(
    input: TokenStream,
) -> Result<TokenStream, syn::Error> {
    // TODO optimisation: parse this in a way that doesn't actually
    // parse the input data, with some custom CaptureInput which is
    // a bit like syn::DeriveInput.
    let input: syn::DeriveInput = syn::parse2(input)?;

    let mac_name = format_ident!("derive_adhoc_apply_{}", &input.ident);
    let export = match &input.vis {
        syn::Visibility::Public(_) => Some(quote! { #[macro_export] }),
        _ => None,
    };

    // TODO use a longer name for derive_adhoc_expand so users only
    // have to import the `derive_adhoc` crate.
    let output = quote! {
        #export
        macro_rules! #mac_name {
            {
                $($template:tt)*
            } => {
                derive_adhoc_expand!{
                    { #input }
                    $($template)*
                }
            }
        }
    };

    Ok(output)
}
