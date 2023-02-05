//! Macro impl for capturing the driver `#[derive(Adhoc)]`

use crate::prelude::*;

/// Contents of an entry in a `#[derive_adhoc(..)]` attribute
enum InvocationEntry {
    Precanned(syn::Path),
}

// (CannedName, CannedName, ...)
struct InvocationAttr {
    entries: Punctuated<InvocationEntry, token::Comma>,
}

impl Parse for InvocationEntry {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(InvocationEntry::Precanned(syn::Path::parse_mod_style(
            input,
        )?))
    }
}

impl Parse for InvocationAttr {
    fn parse(outer: ParseStream) -> syn::Result<Self> {
        let input;
        let _paren = parenthesized!(input in outer);
        let entries = Punctuated::parse_terminated(&input)?;
        Ok(InvocationAttr { entries })
    }
}

/// This is #[derive(Adhoc)]
pub fn derive_adhoc_derive_macro(
    driver: TokenStream,
) -> Result<TokenStream, syn::Error> {
    // TODO optimisation: parse this in a way that doesn't actually
    // parse the input data, with some custom CaptureInput which is
    // a bit like syn::DeriveInput.
    let driver: syn::DeriveInput = syn::parse2(driver)?;

    let driver_mac_name =
        format_ident!("derive_adhoc_driver_{}", &driver.ident);

    let precanned_paths: Vec<syn::Path> = driver
        .attrs
        .iter()
        .map(|attr| {
            if !attr.path.is_ident("derive_adhoc") {
                return Ok(None);
            }
            let tokens = attr.tokens.clone();
            let InvocationAttr { entries } = syn::parse2(tokens)?;
            Ok(Some(entries))
        })
        .flatten_ok()
        .flatten_ok()
        .filter_map(|entry| match entry {
            Err(e) => Some(Err(e)),
            Ok(InvocationEntry::Precanned(path)) => Some(Ok(path)),
        })
        .collect::<syn::Result<Vec<_>>>()?;

    let expand_macro = expand_macro_name()?;

    let mut output = quote! {
        #[allow(unused_macros)]
        macro_rules! #driver_mac_name {
            {
                { $($template:tt)* }
                $($tpassthrough:tt)*
            } => {
                #expand_macro!{
                    { #driver }
                    { }
                    { $($template)* }
                    { $($tpassthrough)* }
                }
            }
        }
    };

    for mut templ_path in precanned_paths {
        if templ_path.segments.is_empty() {
            return Err(templ_path
                .leading_colon
                .as_ref()
                .expect("path with no tokens!")
                .error("cannot derive_adhoc the empty path!"));
        }
        let last = templ_path.segments.last_mut().expect("became empty!");
        last.ident = format_ident!("derive_adhoc_template_{}", last.ident);

        output.extend(quote! {
            #templ_path !{
                $
                { #driver }
            }
        });
    }

    // let ident = &input.ident;
    // eprintln!("---------- derive(Adhoc) start on {} ----------", ident);
    // eprintln!("{}", &output);
    // eprintln!("---------- derive(Adhoc) end on {} ----------", ident);

    Ok(output)
}
