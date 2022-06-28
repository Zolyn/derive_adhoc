
use crate::prelude::*;

struct ExpansionInput {
    #[allow(dead_code)]
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
}

struct RepeatedTemplate {
    template: Template,
    over: RepetitionOver,
    when: Vec<TemplateExpression>,
}

use TemplateElement as TE;

enum Expansion {
    Struct,
}

use Expansion as Ex;

#[derive(Debug, Copy, Eq, PartialEq)]
enum RepetitionOver {
}

use RepetitionOver as RO;

struct RepetitionOverInference {
    over: RepetitionOver,
    why: Span,
}

enum TemplateExpression {
}

use TemplateExpression as Texpr;

struct ReptitionAnalysisVisitor {
    over: Option<RepetitionOver>,
    when: Option<Vec<TemplateExpression>>,
    error: Vec<syn::Error>,
}

impl ReptitionAnalysisVisitor {
    fn set_over(&mut self, over: RepetitionOverInference) {
        match self.over {
            None => self.over = over,
            Some(already) if &already.over == &self.over => { },
            Some(already) => self.errors([
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
            ]),
        }
    }
    fn add_when(&mut self, when: TemplateExpression) {
        match self.when {
            None => self.errors([
                syn::Error::new(when.span, format!(
                    "when clause not directly within repetition"
                        already.over,
                )),
    }
}

struct TemplateElementIsWhenClause(pub bool);

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
                    let _paren = parenthesized!(template in input);
                    let repeat = template.analyse_repetition();
                    TE::Repeat(repeat)
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
        Ok(if ident == "Struct" {
            Ex::Struct
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
    ///
    ///  1. Filters out any ${when } clauses
    
    fn analyse_repetition(&mut self, visitor: &mut ReptitionAnalysisVisitor) {
        let when = vec![];
        let over = None;
        self.elements.retain_mut(|elem| {
            match elem {
                TE::Pass(_) => { }
                TE::Repeat(_) => { }
                TE::Group { template, .. } => {
                    template.analyse_repetition(visitor);
                },
                TE::Expansion { exp, .. } => {
                    let was_when_clause = exp.analyse_repetition(visitor);
                    return !was_when_clause.0 // delete when clauses
                },
            }
            true // keep everything else
        });
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
        }
    }

    fn analyse_repetition(&self) -> (Option<RepetitionOver>, Option<
}

impl Expansion {
    fn expand(&self, ctx: &ExpansionContext, out: &mut TokenStream) {
        match self {
            Ex::Struct => ctx.top.ident.to_tokens(out),
        }
    }

    fn analyse_repetition(&mut self, visitor: &mut ReptitionAnalysisVisitor)
                          -> TemplateElementIsWhenClause {
        let over = match self {
            Struct => None,
            // when clause will do special stuff return early
        };
        if let Some(over) { visitor.set_over(over) }
        TemplateElementIsWhenClause(false)
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
