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

// Replaces every $ with ${dollar}
// SRSLY
fn escape_dollars(input: TokenStream) -> TokenStream {
    let mut out = TokenStream::new();
    for tt in input {
        out.extend([match tt {
            TT::Group(g) => {
                let delim = g.delimiter();
                let span = g.span_open();
                let stream = g.stream();
                let stream = escape_dollars(stream);
                let mut g = proc_macro2::Group::new(delim, stream);
                g.set_span(span);
                TT::Group(g)
            }
            TT::Punct(p) if p.as_char() == '$' => {
                out.extend(quote_spanned! {p.span()=> #p dollar });
                continue;
            }
            other => other,
        }])
    }
    out
}

pub fn define_derive_adhoc_func_macro(
    input: TokenStream,
) -> Result<TokenStream, syn::Error> {
    let TemplateDefinition {
        templ_name,
        template,
    } = syn::parse2(input)?;

    let template = escape_dollars(template);

    let mac_name = format_ident!("derive_adhoc_call_{}", &templ_name);

    // the macro must recent a dollar as its first argument because
    // it is hard to find a dollar otherwise!
    let output = quote! {
        macro_rules! #mac_name {
            {
                $dollar:tt
                $( $driver:tt )*
            } => {
                derive_adhoc_expand!{
                    { $( $driver )* }
                    # template
                }
            }
        }
    };

    eprintln!(
        "---------- derive_adhoc_define start for {} ----------",
        mac_name
    );
    eprintln!("{}", &output);
    eprintln!(
        "---------- derive_adhoc_define end for {} ----------",
        mac_name
    );

    Ok(output)
}
