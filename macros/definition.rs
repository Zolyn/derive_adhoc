use crate::prelude::*;

#[derive(Debug, Clone)]
struct TemplateDefinition {
    templ_name: syn::Ident,
    template: TokenStream,
}

impl Parse for TemplateDefinition {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let templ_name = input.parse()?;
        let _equals: syn::Token![=] = input.parse()?;
        let template = input.parse()?;
        Ok(TemplateDefinition {
            templ_name,
            template,
        })
    }
}

pub fn define_derive_adhoc_func_macro(
    input: TokenStream,
) -> Result<TokenStream, syn::Error> {
    let TemplateDefinition {
        templ_name,
        template,
    } = syn::parse2(input)?;

    //eprintln!("---------- derive_adhoc got start ----------");
    //eprintln!("{}\n{}", &driver.to_token_stream(), &template);
    //eprintln!("---------- derive_adhoc got end ----------");

    let mac_name = format_ident!("derive_adhoc_call_{}", &templ_name);
    let output = quote! {
        macro_rules! #mac_name {
            {
                $( $driver:tt )*
            } => {
                erive_adhoc_expand!{
                    { $( $driver:tt )* }
                    # template
                }
            }
        }
    };

    Ok(output)
}
