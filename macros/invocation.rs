//! Macro impl for invoking a predefined template `derive_adhoc!`

use crate::prelude::*;

#[derive(Debug, Clone)]
struct TemplateInvocation {
    driver: syn::Path,
    colon: Token![:],
    template: TokenStream,
}

impl Parse for TemplateInvocation {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let driver = input.parse()?;
        let colon = input.parse()?;
        let template = input.parse()?;
        Ok(TemplateInvocation {
            driver,
            colon,
            template,
        })
    }
}

/// This is `derive_adhoc!`
///
/// It parses
/// ```rust,ignore
///    StructName:
///    SOME_TOKENS
/// ```
/// and expand it to an invocation of
/// ```rust,ignore
///    derive_adhoc_driver_StructName
/// ```
/// as per NOTES.txt.
pub fn derive_adhoc_func_macro(
    input: TokenStream,
) -> Result<TokenStream, syn::Error> {
    let TemplateInvocation {
        driver,
        colon,
        template,
    } = syn::parse2(input)?;

    // eprintln!("---------- derive_adhoc got start ----------");
    // eprintln!("{}\n{}", &driver.to_token_stream(), &template);
    // eprintln!("---------- derive_adhoc got end ----------");

    let driver_mac_name = {
        let mut name = driver;
        let last = name.segments.last_mut().ok_or_else(|| {
            syn::Error::new(
                colon.span,
                "expected non-empty path for driver struct name, found colon",
            )
        })?;
        last.ident = format_ident!("derive_adhoc_driver_{}", &last.ident);
        name
    };

    let output = quote! { #driver_mac_name !{ { #template } crate; } };

    // eprintln!("---------- derive_adhoc! output start ----------");
    // eprintln!("{}", &output);
    // eprintln!("---------- derive_adhoc! output end ----------");

    Ok(output)
}
