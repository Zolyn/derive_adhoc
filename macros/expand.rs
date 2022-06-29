#![allow(dead_code)]

use crate::prelude::*;

struct ExpansionInput {
    brace_token: token::Brace,
    driver: syn::DeriveInput,
    template: Template,
}

impl Parse for ExpansionInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let driver;
        let brace_token = braced!(driver in input);
        let driver = driver.parse()?;
        let template = input.parse()?;
        Ok(ExpansionInput { brace_token, driver, template })
    }
}

struct Template {
    elements: Vec<TemplateElement>,
}

enum TemplateElement {
    Pass(TokenTree),
    Group {
        /// Sadly Group's constructors let us only set *both* delimiters
        delim_span: Span,
        delimiter: Delimiter,
        template: Template,
    },
    Expansion {
        #[allow(dead_code)]
        dollar: Punct,
        exp: Expansion,
    },
    Repeat(RepeatedTemplate),
    Errors(Vec<syn::Error>),
}

struct RepeatedTemplate {
    template: Template,
    over: RepeatOver,
}

use TemplateElement as TE;

#[allow(non_camel_case_types)] // clearer to use the exact ident
enum Expansion {
    tname,
}

use Expansion as Ex;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Display)]
enum RepeatOver {
}

//use RepeatOver as RO;

struct RepeatOverInference {
    over: RepeatOver,
    span: Span,
}

enum TemplateExpression {
}

//use TemplateExpression as Texpr;

#[derive(Default)]
struct RepeatAnalysisVisitor {
    over: Option<RepeatOverInference>,
    errors: Vec<syn::Error>,
}

impl RepeatAnalysisVisitor {
    fn set_over(&mut self, over: RepeatOverInference) {
        match &self.over {
            None => self.over = Some(over),
            Some(already) => if &already.over != &over.over {
                self.errors([
                    syn::Error::new(over.span, format!(
                        "inconsistent repetition depth: \
                         firstly, {} inferred here",
                        already.over,
                    )),
                    syn::Error::new(over.span, format!(
                        "inconsistent repetition depth: \
                         secondly, {} inferred here",
                        over.over,
                    )),
                ]);
            }
        }
    }

    fn errors<EL: IntoIterator<Item=syn::Error>>(&mut self, errors: EL) {
        if self.errors.is_empty() {
            self.errors.extend(errors)
        }
    }

    fn finish(self, start: Span) -> Result<RepeatOver, Vec<syn::Error>> {
        use RepeatAnalysisVisitor as RAV;
        match self {
            RAV { errors, .. } if !errors.is_empty() => Err(errors),
            RAV { over: Some(over), .. } => Ok(over.over),
            _ => Err(vec![syn::Error::new(
                start,
                "no contained expansion field determined what to repeat here",
            )]),
        }
    }
}

impl Parse for Template {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut elements = vec![];
        while ! input.is_empty() {
            elements.push(input.parse()?)
        }
        Ok(Template { elements })
    }
}

impl Parse for TemplateElement {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(match input.parse()? {
            TT::Group(group) => {
                let delim_span = group.span_open();
                let delimiter = group.delimiter();
                let template = syn::parse2(group.stream())?;
                TE::Group { delim_span, delimiter, template }
            },
            tt@ TT::Ident(_) |
            tt@ TT::Literal(_) => {
                TE::Pass(tt)
            },
            TT::Punct(tok) if tok.as_char() != '$' => {
                TE::Pass(TT::Punct(tok))
            },
            TT::Punct(dollar) => {
                let la = input.lookahead1();
                if la.peek(Token![$]) {
                    // $$
                    TE::Pass(input.parse()?)
                } else if la.peek(token::Brace) {
                    let exp;
                    let _brace = braced!(exp in input);
                    let exp = exp.parse()?;
                    TE::Expansion { dollar, exp }
                } else if la.peek(token::Paren) {
                    let template;
                    let paren = parenthesized!(template in input);
                    let template: Template = template.parse()?;
                    let mut visitor = RepeatAnalysisVisitor::default();
                    template.analyse_repeat(&mut visitor);
                    match visitor.finish(paren.span) {
                        Err(el) => TE::Errors(el),
                        Ok(over) => TE::Repeat(RepeatedTemplate {
                            over,
                            template,
                        }),
                    }
                } else if la.peek(syn::Ident::peek_any) {
                    let exp: TokenTree = input.parse()?; // get it as TT
                    let exp = syn::parse2(exp.to_token_stream())?;
                    TE::Expansion { dollar, exp }
                } else {
                    return Err(la.error())
                }
            },
        })
    }
}

impl Parse for Expansion {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident: Ident = input.parse()?;
        Ok(if ident == "tname" {
            Ex::tname
        } else {
            return Err(syn::Error::new(
                ident.span(),
                "unknown expansion item"
            ))
        })
    }
}

struct ExpansionContext<'c> {
    top: &'c syn::DeriveInput,
}

impl Template {
    fn expand(&self, ctx: &ExpansionContext, out: &mut TokenStream) {
        for element in &self.elements {
            element.expand(ctx, out);
        }
    }

    /// Analyses a template section to be repeated
    fn analyse_repeat(&self, visitor: &mut RepeatAnalysisVisitor) {
        for element in &self.elements {
            element.analyse_repeat(visitor);
        }
    }
}

impl TemplateElement {
    fn expand(&self, ctx: &ExpansionContext, out: &mut TokenStream) {
        match self {
            TE::Pass(tt) => out.extend([tt.clone()]),
            TE::Group { delim_span, delimiter, template } => {
                use proc_macro2::Group as Group;
                let mut content = TokenStream::new();
                template.expand(ctx, &mut content);
                let mut group = Group::new(delimiter.clone(), content);
                group.set_span(delim_span.clone());
                out.extend([TT::Group(group)]);
            },
            TE::Expansion { dollar:_, exp } => {
                exp.expand(ctx, out);
            },
            TE::Repeat(RepeatedTemplate { template:_, over }) => {
                match *over {
                }
            },
            TE::Errors(el) => {
                for e in el {
                    out.extend(e.to_compile_error())
                }
            }
        }
    }

    fn analyse_repeat(&self, visitor: &mut RepeatAnalysisVisitor) {
        match self {
            TE::Pass(_) => { }
            TE::Repeat(_) => { }
            TE::Group { template, .. } => template.analyse_repeat(visitor),
            TE::Expansion { exp, .. } => exp.analyse_repeat(visitor),
            TE::Errors(el) => visitor.errors(el.clone()),
        }
    }
}

impl Expansion {
    fn expand(&self, ctx: &ExpansionContext, out: &mut TokenStream) {
        match self {
            Ex::tname => ctx.top.ident.to_tokens(out),
        }
    }

    fn analyse_repeat(&self, visitor: &mut RepeatAnalysisVisitor) {
        let over = match self {
            Ex::tname => None,
        };
        if let Some(over) = over { visitor.set_over(over) }
    }
}

// This should implement the actual template engine
//
// In my design, the input contains, firstly, literally the definition
// that #[derive(Adhoc)] was applied to (see NOTES.txt).
// Using the literal input, rather than some pre-parsed version, is
// slower, but means that we aren't inventing a nontrivial data format which
// potentially crosses crate boundaries with semver implications.
//
// We should start with a POC where the template engine does something
// totally trivial, but which does:
//   - depend on parsing the original derive macro input (struct def'n)
//   - treat $ in the template specially
//   - make output that replicates mostly the template
// Eg, how about making a thing where the templater just replaces
//   $ Struct
// with the original struct ident.
pub fn derive_adhoc_expand_func_macro(input: TokenStream)
                                      -> syn::Result<TokenStream> {
    let input: ExpansionInput = syn::parse2(input)?;
    let ident = &input.driver.ident;
    dbg!(&ident);

    // maybe we should be using syn::buffer::TokenBuffer ?
    // or Vec<TokenTree>, which we parse into a tree of our own full
    // of [TokenTree] ?
    let ctx = ExpansionContext {
        top: &input.driver,
    };
    let mut output = TokenStream::new();
    input.template.expand(&ctx, &mut output);

    // obviously nothing should print to stderr
    dbg!(&&output);
    eprintln!("---------- derive_adhoc_expand got start ----------");
    eprintln!("{}", &output);
    eprintln!("---------- derive_adhoc_expand got end ----------");
    Ok(output.into())
}
