//! Macro impl for capturing the driver `#[derive(Adhoc)]`

use crate::prelude::*;

/// Contents of an entry in a `#[derive_adhoc(..)]` attribute
enum InvocationEntry {
    Precanned(syn::Path),
    Pub(syn::VisPublic),
}

// (CannedName, CannedName, ...)
struct InvocationAttr {
    entries: Punctuated<InvocationEntry, token::Comma>,
}

impl Parse for InvocationEntry {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let entry = if input.lookahead1().peek(Token![pub]) {
            // TODO DOCS
            let vis = match input.parse()? {
                syn::Visibility::Public(vis) => vis,
                other => {
                    return Err(other
                        .error("only `pub`, nor no visibility, is allowed"))
                }
            };
            InvocationEntry::Pub(vis)
        } else {
            let path = syn::Path::parse_mod_style(input)?;
            InvocationEntry::Precanned(path)
        };
        Ok(entry)
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

    let mut vis_pub = None;

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
            Ok(InvocationEntry::Pub(vis)) => {
                let this_span = vis.span();
                if let Some(prev) = mem::replace(&mut vis_pub, Some(vis)) {
                    return Some(Err([
                        (prev.span(), "first `pub`"),
                        (this_span, "second `pub`"),
                    ]
                    .error("`pub` specified multiple times")));
                };
                None
            }
        })
        .collect::<syn::Result<Vec<_>>>()?;

    let expand_macro = expand_macro_name()?;

    let macro_export = vis_pub
        .map(|vis_pub| {
            let macro_export =
                quote_spanned!(vis_pub.span()=> #[macro_export]);
            Ok::<_, syn::Error>(macro_export)
        })
        .transpose()?;

    let mut output = quote! {
        #[allow(unused_macros)]
        #macro_export
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
