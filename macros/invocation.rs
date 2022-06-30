use crate::prelude::*;

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

// This is derive_adhoc!
//
// It parses
//    StructName:
//    SOME_TOKENS
// and expand it to an invocation of
//    derive_adhoc_apply_StructName
// as per NOTES.txt.
pub fn derive_adhoc_func_macro(
    input: TokenStream,
) -> Result<TokenStream, syn::Error> {
    let TemplateInvocation {
        driver,
        colon,
        template,
    } = syn::parse2(input)?;

    //eprintln!("---------- derive_adhoc got start ----------");
    //eprintln!("{}\n{}", &driver.to_token_stream(), &template);
    //eprintln!("---------- derive_adhoc got end ----------");

    let mac_name = {
        let mut name = driver;
        let last = name.segments.last_mut().ok_or_else(|| {
            syn::Error::new(
                colon.span,
                "expected non-empty path for driver struct name, found colon",
            )
        })?;
        last.ident = format_ident!("derive_adhoc_apply_{}", &last.ident);
        name
    };

    let output = quote! { #mac_name !{ #template } };

    Ok(output)
}
