//! Macro impl for defining a template `define_derive_adhoc!`

use super::prelude::*;

#[derive(Debug, Clone)]
struct TemplateDefinition {
    doc_attrs: Vec<syn::Attribute>,
    vis: Option<syn::VisPublic>,
    templ_name: syn::Ident,
    options: UnprocessedOptions,
    template: TokenStream,
}

impl Parse for TemplateDefinition {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // This rejects Rust keywords, which is good because
        // for example `#[derive_adhoc(pub)]` ought not to mean to apply
        // a template called `pub`.  See ticket #1.
        let doc_attrs = input.call(syn::Attribute::parse_outer)?;
        for attr in &doc_attrs {
            if !attr.path.is_ident("doc") {
                return Err(attr
                    .path
                    .error("only doc attributes are supported"));
            }
        }
        let vis: Option<_> = input.parse()?;
        let templ_name = input.parse()?;
        let options = UnprocessedOptions::parse(&input, OpContext::Template)?;
        let _equals: syn::Token![=] = input.parse()?;
        let template = input.parse()?;
        Ok(TemplateDefinition {
            doc_attrs,
            vis: vis.map(|pub_token| syn::VisPublic { pub_token }),
            templ_name,
            options,
            template,
        })
    }
}

/// Replaces every `$` with `$orig_dollar`
///
/// Eg, where the template says `$fname`, we emit `$orig_dollar fname`.
/// When this is found in the macro_rules expander part
/// of a precanned template,
/// macro_rules doesn't expand
/// it because `orig_dollar` isn't one of the arguments to the macro.
///
/// Then, we spot these when parsing the template, and disregard them.
/// That is done by
/// [`syntax::deescape_orig_dollar`](super::syntax::deescape_orig_dollar).
///
/// See `doc/implementation.md` for why this is needed.
///
/// This has the weird result that there's a sometimes
/// (namely, when using a truly-adhoc, rather than precanned template)
/// an undocumented `orig_dollar` expansion keyword,
/// with strange behaviour.
/// No-one is likely to notice this.
///
/// Other tactics:
///
///  * Pass a literal dollar sign `$` into the template pattern macro,
///    capture it with a macro rules parameter `$dollar:tt`,
///    and write `$dollar` in the template.
///    This gets the span wrong: the span is that of
///    the literal dollar, which came from the call site, not the template.
///
/// * Use a different syntax in precanned templates:
///   have `escape_dollars` convert to that syntax,
///   and the template parsing notice this case and
///   de-escape the whole template again at the start.
///   This involves processing the whole template twice for no reason.
///   (And it would involve inventing an additional, different,
///   and probably weird, syntax.)
///
/// * As above but do the de-escaping on the fly.
///   Currently, though, the information about the template context
///   is not available to the parser.
///   We'd have to pass it in as a thread local,
///   or as an extra generic on `SubstContext`
///   (producing two monomorphised copies of the whole template engine).
pub fn escape_dollars(input: TokenStream) -> TokenStream {
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
                out.extend(quote_spanned! {p.span()=> #p orig_dollar });
                continue;
            }
            other => other,
        }])
    }
    out
}

/// This is `define_derive_adhoc!`
pub fn define_derive_adhoc_func_macro(
    input: TokenStream,
) -> Result<TokenStream, syn::Error> {
    // eprintln!(
    //     "---------- derive_adhoc_define start input ----------",
    // );
    // eprintln!("{}", &input);
    // eprintln!(
    //     "---------- derive_adhoc_define end input ----------",
    // );

    let TemplateDefinition {
        doc_attrs,
        vis,
        templ_name,
        options,
        template,
    } = syn::parse2(input)?;

    let template = escape_dollars(template);

    let templ_mac_name =
        format_ident!("derive_adhoc_template_{}", &templ_name);

    let doc_addendum = (!doc_attrs.is_empty()).then(|| {
        let addendum = format!(
            r#"

This is a `derive_adhoc` template.  Do not invoke it directly.
To use it, write: `#[derive(Adhoc)] #[derive_adhoc({})]`."#,
            templ_name
        );
        quote!( #[doc = #addendum] )
    });

    let expand_macro;
    let vis_export;
    match vis {
        None => {
            vis_export = quote! {};
            expand_macro = expand_macro_name()?;
        }
        Some(syn::VisPublic { pub_token }) => {
            let span = pub_token.span();
            vis_export = quote_spanned!(span=> #[macro_export]);
            expand_macro = quote_spanned!(span=> $crate::derive_adhoc::derive_adhoc_expand);
        }
    }

    // the macro must recent a dollar as its first argument because
    // it is hard to find a dollar otherwise!
    let output = quote! {
        #( #doc_attrs )*
        #doc_addendum
        #vis_export
        macro_rules! #templ_mac_name {
            {
                { $($driver:tt)* }
             $( [ $($aoptions:tt)* ] )?
                { $($future:tt)* }
                $($dpassthrough:tt)*
            } => {
                #expand_macro! {
                    { $( $driver )* }
                 $( [ $($aoptions)* ] )?
                    { $($dpassthrough)* }
                    { # template }
                    { $crate; [#options] #templ_name; }
                }
            };
            { $($wrong:tt)* } => {
                compile_error!{concat!(
                    "wrong input to derive-adhoc template inner macro ",
                    stringify!(#templ_mac_name),
                    "; might be due to incompatible derive-adhoc versions(s)",
                )}
            };
        }
    };

    // eprintln!(
    //     "---------- derive_adhoc_define start for {} ----------",
    //     templ_mac_name
    // );
    // eprintln!("{}", &output);
    // eprintln!(
    //     "---------- derive_adhoc_define end for {} ----------",
    //     templ_mac_name
    // );

    Ok(output)
}
