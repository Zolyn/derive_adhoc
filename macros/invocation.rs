//! Macro impl for invoking a predefined template `derive_adhoc!`

use crate::prelude::*;

#[derive(Debug, Clone)]
struct TemplateInvocation {
    driver: syn::Path,
    // TODO DOCS note specifying template opts in doc for derive_adhoc!
    options: UnprocessedOptions,
    colon: Token![:],
    template: TokenStream,
}

impl Parse for TemplateInvocation {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let driver = input.parse()?;
        let options = input.parse()?;
        let colon = input.parse()?;
        let template = input.parse()?;
        Ok(TemplateInvocation {
            driver,
            options,
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
        options,
        colon,
        template,
    } = syn::parse2(input)?;

    // eprintln!("---------- derive_adhoc got start ----------");
    // eprintln!("{}\n{}", &driver.to_token_stream(), &template);
    // eprintln!("---------- derive_adhoc got end ----------");

    let driver_mac_name = {
        let mut name = driver;
        let last = name.segments.last_mut().ok_or_else(|| {
            colon.error(
                "expected non-empty path for driver struct name, found colon",
            )
        })?;
        last.ident = format_ident!("derive_adhoc_driver_{}", &last.ident);
        name
    };

    // We must output the extra template arguments.
    // But maybe the template engine we are speaking to is old.
    // So any of the arguments that aren't "blank", we leave off.
    // But we can only do that at the end.
    enum Extra {
        Required(TokenStream),
        Blank(TokenStream),
    }
    impl ToTokens for Extra {
        fn to_tokens(&self, out: &mut TokenStream) {
            match self {
                Extra::Required(v) => v.to_tokens(out),
                Extra::Blank(v) => v.to_tokens(out),
            }
        }
    }
    let mut extras = vec![if options.is_empty() {
        Extra::Blank(quote! { () })
    } else {
        Extra::Required(quote! { (#options) })
    }];
    while matches!(extras.last(), Some(Extra::Blank(_))) {
        extras.pop();
    }

    let output = quote! {
        #driver_mac_name !{
            { #template }
            { ($) }
            crate; #(#extras)*
        }
    };

    // eprintln!("---------- derive_adhoc! output start ----------");
    // eprintln!("{}", &output);
    // eprintln!("---------- derive_adhoc! output end ----------");

    Ok(output)
}
