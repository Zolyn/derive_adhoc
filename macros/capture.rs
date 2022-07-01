use crate::prelude::*;

// (CannedName, CannedName, ...)
struct PrecannedInvocationsAttr {
    paths: Punctuated<syn::Path, token::Comma>,
}

impl Parse for PrecannedInvocationsAttr {
    fn parse(outer: ParseStream) -> syn::Result<Self> {
        let input;
        let _paren = parenthesized!(input in outer);
        let paths = Punctuated::parse_terminated_with(
            &input,
            syn::Path::parse_mod_style,
        )?;
        Ok(PrecannedInvocationsAttr { paths })
    }
}

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

    let precanned_paths: Vec<syn::Path> = input
        .attrs
        .iter()
        .map(|attr| {
            if !attr.path.is_ident("derive_adhoc") {
                return Ok(None);
            }
            let tokens = attr.tokens.clone();
            let PrecannedInvocationsAttr { paths } = syn::parse2(tokens)?;
            Ok(Some(paths))
        })
        .flatten_ok()
        .flatten_ok()
        .collect::<syn::Result<Vec<_>>>()?;

    let expand_macro = expand_macro_name()?;

    let mut output = quote! {
        #[allow(unused_macros)]
        #export
        macro_rules! #mac_name {
            {
                $($template:tt)*
            } => {
                #expand_macro!{
                    { #input }
                    $($template)*
                }
            }
        }
    };

    for mut path in precanned_paths {
        if path.segments.is_empty() {
            return Err(path
                .leading_colon
                .as_ref()
                .expect("path with no tokens!")
                .error("cannot derive_adhoc the empty path!"));
        }
        let last = path.segments.last_mut().expect("became empty!");
        last.ident = format_ident!("derive_adhoc_call_{}", last.ident);

        output.extend(quote! {
            #path !{
                $
                #input
            }
        });
    }

    let ident = &input.ident;
    eprintln!("---------- derive(Adhoc) start on {} ----------", ident);
    eprintln!("{}", &output);
    eprintln!("---------- derive(Adhoc) end on {} ----------", ident);

    Ok(output)
}
