
use crate::prelude::*;

struct TemplateInvocation {
    struc: syn::Path,
    template: TokenStream,
}

impl Parse for TemplateInvocation {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let struc = input.parse()?;
        let _: Token![:] = input.parse()?;
        let template = input.parse()?;
        Ok(TemplateInvocation { struc, template })
    }
}

pub fn derive_adhoc_func_macro(input: TokenStream)
                               -> Result<TokenStream, syn::Error> {
    let input: TemplateInvocation = syn::parse2(input)?;
    
    eprintln!("---------- derive_adhoc got start ----------");
    eprintln!("{}\n{}", &input.struc.to_token_stream(), &input.template);
    eprintln!("---------- derive_adhoc got end ----------");
    
/*
    quote!{
        derive_adhoc_apply_ChannelsParams!{
            #input
        }
    }.into()
     */
    Ok(quote!{ })
}
