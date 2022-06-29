#![allow(dead_code)]

use crate::prelude::*;

struct SubstInput {
    brace_token: token::Brace,
    driver: syn::DeriveInput,
    template: Template,
}

impl Parse for SubstInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let driver;
        let brace_token = braced!(driver in input);
        let driver = driver.parse()?;
        let template = input.parse()?;
        Ok(SubstInput { brace_token, driver, template })
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
    Subst(Subst),
    Repeat(RepeatedTemplate),
    Errors(Vec<syn::Error>),
}

struct RepeatedTemplate {
    template: Template,
    over: RepeatOver,
}

use TemplateElement as TE;

struct Subst {
    kw: Ident,
    ed: SubstDetails,
}

#[allow(non_camel_case_types)] // clearer to use the exact ident
enum SubstDetails {
    tname,
    vname,
    fname,
}

use SubstDetails as ED;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Display)]
#[strum(serialize_all = "snake_case")]
enum RepeatOver {
    Variants,
    Fields,
}

use RepeatOver as RO;

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
            TT::Punct(_dollar) => {
                let la = input.lookahead1();
                if la.peek(Token![$]) {
                    // $$
                    TE::Pass(input.parse()?)
                } else if la.peek(token::Brace) {
                    let exp;
                    let _brace = braced!(exp in input);
                    let exp = exp.parse()?;
                    TE::Subst(exp)
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
                    TE::Subst(exp)
                } else {
                    return Err(la.error())
                }
            },
        })
    }
}

impl Parse for Subst {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let kw: Ident = input.parse()?;

        // todo: use a macro_rules macro?
        let ed = if kw == "tname" {
            ED::tname
        } else if kw == "vname" {
            ED::vname
        } else if kw == "fname" {
            ED::fname
        } else {
            return Err(syn::Error::new(
                kw.span(),
                "unknown expansion item"
            ))
        };
        Ok(Subst { ed, kw: kw.clone() })
    }
}

struct SubstContext<'c> {
    top: &'c syn::DeriveInput,
    variant: Option<&'c WithinVariant<'c>>,
    field: Option<&'c WithinField<'c>>,
}

struct WithinVariant<'c> {
    variant: Option<&'c syn::Variant>,
    fields: &'c syn::Fields,
}

struct WithinField<'c> {
    field: &'c syn::Field,
    index: u32,
}

impl Template {
    fn expand(&self, ctx: &SubstContext, out: &mut TokenStream) {
        for element in &self.elements {
            let () = element.expand(ctx, out)
                .unwrap_or_else(|err| out.extend(err.into_compile_error()));
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
    fn expand(&self, ctx: &SubstContext, out: &mut TokenStream)
        -> syn::Result<()>
    {
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
            TE::Subst(exp) => {
                exp.expand(ctx, out)?;
            },
            TE::Repeat(repeated_template) => {
                repeated_template.expand(ctx, out);
            },
            TE::Errors(el) => {
                for e in el {
                    out.extend(e.to_compile_error())
                }
            }
        }
        Ok(())
    }

    fn analyse_repeat(&self, visitor: &mut RepeatAnalysisVisitor) {
        match self {
            TE::Pass(_) => { }
            TE::Repeat(_) => { }
            TE::Group { template, .. } => template.analyse_repeat(visitor),
            TE::Subst(exp) => exp.analyse_repeat(visitor),
            TE::Errors(el) => visitor.errors(el.clone()),
        }
    }
}

impl Spanned for Subst {
    fn span(&self) -> Span {
        self.kw.span()
    }
}

impl Subst {
    fn expand(&self, ctx: &SubstContext, out: &mut TokenStream)
              -> syn::Result<()>
    {
        match self.ed {
            ED::tname => ctx.top.ident.to_tokens(out),
            ED::vname => ctx.syn_variant(self)?.ident.to_tokens(out),
            ED::fname => {
                let f = ctx.field(self)?;
                if let Some(fname) = &f.field.ident {
                    // todo is this the right span to emit?
                    fname.to_tokens(out);
                } else {
                    syn::Index { index: f.index, span: self.kw.span() }
                        .to_tokens(out);
                }
            },
        };
        Ok(())
    }

    fn analyse_repeat(&self, visitor: &mut RepeatAnalysisVisitor) {
        let over = match self.ed {
            ED::tname => None,
            ED::vname => Some(RO::Variants),
            ED::fname => Some(RO::Fields),
        };
        if let Some(over) = over {
            let over = RepeatOverInference { over, span: self.kw.span() };
            visitor.set_over(over);
        }
    }
}

impl<'c> SubstContext<'c> {
    fn for_variants<F>(&self, mut call: F)
    where F: FnMut(&SubstContext, &WithinVariant)
    {
        let ctx = self;
        let mut within_variant = |variant, fields| {
            let wv = WithinVariant { variant, fields };
            let wv = &wv;
            let ctx = SubstContext { variant: Some(wv), ..*ctx };
            call(&ctx, wv);
        };
        match &ctx.top.data {
            syn::Data::Struct(syn::DataStruct { fields, .. }) => {
                within_variant(None, fields);
            },
            syn::Data::Enum(syn::DataEnum { variants, .. }) => {
                for variant in variants {
                    within_variant(Some(variant), &variant.fields);
                }
            }
            syn::Data::Union(syn::DataUnion { fields, .. }) => {
                let fields = syn::Fields::Named(fields.clone());
                within_variant(None, &fields);
            }
        }
    }

    fn for_with_variant<F>(&self, mut call: F)
    where F: FnMut(&SubstContext, &WithinVariant)
    {
        let ctx = self;
        if let Some(wv) = &self.variant {
            call(ctx, wv);
        } else {
            ctx.for_variants(call);
        }
    }

    fn variant(&self, why: &dyn Spanned) -> syn::Result<&WithinVariant> {
        // TODO helper function, maybe ext trait on Spanned, for syn::Error
        let r = self.variant.as_ref().ok_or_else(|| syn::Error::new(
            why.span(),
            "expansion must be within a variant (so, in a repeat group)"
        ))?;
        Ok(r)
    }

    fn syn_variant(&self, why: &dyn Spanned) -> syn::Result<&syn::Variant> {
        let r = self.variant(why)?
            .variant.as_ref().ok_or_else(|| syn::Error::new(
                why.span(),
                "expansion only valid in enums"
            ))?;
        Ok(r)
    }


    fn for_fields<F>(&self, mut call: F)
    where F: FnMut(&SubstContext, &WithinField)
    {
        let ctx = self;
        ctx.for_with_variant(|ctx, variant| {
            for (index, field) in variant.fields.iter().enumerate() {
                let index = index.try_into().expect(">=2^32 fields!");
                let wf = WithinField { field, index };
                let wf = &wf;
                let ctx = SubstContext { field: Some(wf), ..*ctx };
                call(&ctx, wf);
            }
        })
    }

    fn field(&self, why: &dyn Spanned) -> syn::Result<&WithinField> {
        let r = self.field.as_ref().ok_or_else(|| syn::Error::new(
            why.span(),
            "expansion must be within a field (so, in a repeat group)"
        ))?;
        Ok(r)
    }

}

impl RepeatedTemplate {
    fn expand(&self, ctx: &SubstContext, out: &mut TokenStream) {
        match self.over {
            RO::Variants => ctx.for_variants(
                |ctx, _variant| self.template.expand(ctx, out)
            ),
            RO::Fields => ctx.for_fields(
                |ctx, _field| self.template.expand(ctx, out)
            ),
        }
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
    let input: SubstInput = syn::parse2(input)?;
    let ident = &input.driver.ident;
    dbg!(&ident);

    // maybe we should be using syn::buffer::TokenBuffer ?
    // or Vec<TokenTree>, which we parse into a tree of our own full
    // of [TokenTree] ?
    let ctx = SubstContext {
        top: &input.driver,
        field: None,
        variant: None,
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
