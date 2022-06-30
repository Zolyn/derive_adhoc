#![allow(dead_code)]

use crate::prelude::*;

#[derive(Debug)]
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
        Ok(SubstInput {
            brace_token,
            driver,
            template,
        })
    }
}

#[derive(Debug)]
struct Template {
    elements: Vec<TemplateElement>,
}

#[derive(Debug)]
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

#[derive(Debug)]
struct RepeatedTemplate {
    template: Template,
    whens: Vec<Box<Subst>>,
    over: RepeatOver,
}

use TemplateElement as TE;

#[derive(Debug)]
struct Subst {
    kw: Ident,
    sd: SubstDetails,
}

#[allow(non_camel_case_types)] // clearer to use the exact ident
#[derive(Debug)]
// TODO: it might be good to separate this into separate enums for
// conditions and substitutions? -nickm
// I don't think so.  I unified these because the following places
// wanted to treat them very similarly:
//   - parsing
//   - iteration inspection
//   - attribute recursive descent matching
// Keeping them a single type avoids us making weird syntactic
// wrinkles (and may help avoid semantic wrinkles).
enum SubstDetails {
    // variables
    tname,
    ttype,
    vname,
    fname,
    ftype,

    // attributes
    tattr(SubstAttr),
    vattr(SubstAttr),
    fattr(SubstAttr),

    // special
    when(Box<Subst>),

    // expressions
    False,
    True,
    not(Box<Subst>),
    any(Punctuated<Subst, token::Comma>),
    all(Punctuated<Subst, token::Comma>),
    If(SubstIf),
}

#[derive(Debug)]
struct SubstIf {
    // A series of test/result pairs.  The test that gives "true"
    // short-circuits the rest.
    tests: Vec<(Subst, TemplateElement)>,
    // A final element to expand if all tests fail.
    otherwise: Option<Box<TemplateElement>>,
}

use SubstDetails as SD;

#[derive(Debug, Clone)]
struct SubstAttr {
    path: syn::Path, // nonempty segments
    deeper: Option<Box<SubstAttr>>,
}

impl Spanned for SubstAttr {
    fn span(&self) -> Span {
        self.path.segments.first().expect("empty path!").span()
    }
}

// Parses (foo,bar(baz),zonk="value")
// Like NestedMeta but doesn't allow lit, since we forbid #[adhoc("some")]
// And discards the paren and the `ahoc` introducer
#[derive(Debug, Clone)]
struct AdhocAttrList {
    meta: Punctuated<syn::Meta, token::Comma>,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Display)]
#[strum(serialize_all = "snake_case")]
enum RepeatOver {
    Variants,
    Fields,
}

use RepeatOver as RO;

#[derive(Debug, Clone)]
struct RepeatOverInference {
    over: RepeatOver,
    span: Span,
}

#[derive(Default, Debug, Clone)]
struct RepeatAnalysisVisitor {
    over: Option<RepeatOverInference>,
    errors: Vec<syn::Error>,
}

impl RepeatAnalysisVisitor {
    fn set_over(&mut self, over: RepeatOverInference) {
        match &self.over {
            None => self.over = Some(over),
            Some(already) => {
                if &already.over != &over.over {
                    let already_over = already.over.clone();
                    self.errors([
                        syn::Error::new(
                            over.span,
                            format!(
                                "inconsistent repetition depth: \
                         firstly, {} inferred here",
                                already_over,
                            ),
                        ),
                        syn::Error::new(
                            over.span,
                            format!(
                                "inconsistent repetition depth: \
                         secondly, {} inferred here",
                                over.over,
                            ),
                        ),
                    ]);
                }
            }
        }
    }

    fn errors<EL: IntoIterator<Item = syn::Error>>(&mut self, errors: EL) {
        if self.errors.is_empty() {
            self.errors.extend(errors)
        }
    }

    fn finish(self, start: Span) -> Result<RepeatOver, Vec<syn::Error>> {
        use RepeatAnalysisVisitor as RAV;
        match self {
            RAV { errors, .. } if !errors.is_empty() => Err(errors),
            RAV {
                over: Some(over), ..
            } => Ok(over.over),
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
        while !input.is_empty() {
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
                TE::Group {
                    delim_span,
                    delimiter,
                    template,
                }
            }
            tt @ TT::Ident(_) | tt @ TT::Literal(_) => TE::Pass(tt),
            TT::Punct(tok) if tok.as_char() != '$' => TE::Pass(TT::Punct(tok)),
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
                    RepeatedTemplate::parse(input)?
                } else if la.peek(syn::Ident::peek_any) {
                    let exp: TokenTree = input.parse()?; // get it as TT
                    let exp = syn::parse2(exp.to_token_stream())?;
                    TE::Subst(exp)
                } else {
                    return Err(la.error());
                }
            }
        })
    }
}

impl Parse for AdhocAttrList {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let meta;
        let _paren = parenthesized!(meta in input);
        let meta = Punctuated::parse_terminated(&meta)?;
        Ok(AdhocAttrList { meta })
    }
}

impl Parse for SubstAttr {
    fn parse(outer: ParseStream) -> syn::Result<Self> {
        let input;
        let paren = parenthesized!(input in outer);
        let path = input.call(syn::Path::parse_mod_style)?;
        if path.segments.is_empty() {
            return Err(paren
                .span
                .error("adhoc attribute must have nonempty path"));
        }

        let deeper = if input.is_empty() {
            None
        } else {
            let deeper = input.parse()?;
            Some(Box::new(deeper))
        };

        Ok(SubstAttr { path, deeper })
    }
}

impl Parse for Subst {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let kw = input.call(syn::Ident::parse_any)?;
        let from_sd = |sd| Ok(Subst { sd, kw: kw.clone() });

        // keyword!{ KEYWORD [ {BLOCK WITH BINDINGS} ] [ CONSTRUCTOR-ARGS ] }
        // expands to   if ... { return ... }
        macro_rules! keyword {
            { $kw:ident $( $ca:tt )? } => { keyword!{ @ $kw { } $( $ca )? } };
            { $kw:ident { $( $bindings:tt )* } $ca:tt } => {
                keyword!{ @ $kw { $( $bindings )* } $ca }
            };
            { @ $kw:ident { $( $bindings:tt )* } $( $constr_args:tt )? } => {
                if kw == stringify!($kw) {
                    $( $bindings )*
                    return from_sd(SD::$kw $( $constr_args )*);
                }
            };
        }

        keyword! { tname }
        keyword! { ttype }
        keyword! { vname }
        keyword! { fname }
        keyword! { ftype }

        keyword! { tattr(input.parse()?) }
        keyword! { vattr(input.parse()?) }
        keyword! { fattr(input.parse()?) }

        keyword! { when(input.parse()?) }

        if kw == "false" {
            return from_sd(SD::False);
        }
        if kw == "true" {
            return from_sd(SD::True);
        }
        if kw == "if" {
            return from_sd(SD::If(input.parse()?));
        }

        keyword! {
            all {
                let inner;
                let _paren = parenthesized!(inner in input);
            }
            (Punctuated::parse_terminated(&inner)?)
        }
        keyword! {
            any {
                let inner;
                let _paren = parenthesized!(inner in input);
            }
            (Punctuated::parse_terminated(&inner)?)
        }
        keyword! {
            not {
                let inner;
                let _paren = parenthesized!(inner in input);
            }
            (inner.parse()?)
        }

        return Err(kw.error("unknown derive-adhoc keyword"));
    }
}

impl Parse for SubstIf {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut tests = Vec::new();
        let mut otherwise = None;

        loop {
            let condition = input.parse()?;
            let content;
            let _br = braced![ content in input ];
            let consequence = content.parse()?;
            tests.push((condition, consequence));

            // (I'd like to use a lookahead here too, but it doesn't
            // accept "Nothing")
            if input.is_empty() {
                // no more conditions if there is not an "else"
                break;
            } else {
                let lookahead = input.lookahead1();
                if lookahead.peek(Token![else]) {
                    let _else: Token![else] = input.parse()?;
                } else {
                    return Err(lookahead.error());
                }
            }

            let lookahead = input.lookahead1();
            if lookahead.peek(Token![if]) {
                let _if: Token![if] = input.parse()?;
                // got an "else if": continue to the next iteration.
                continue;
            } else if lookahead.peek(token::Brace) {
                let content;
                let _br = braced![ content in input ];
                otherwise = Some(content.parse()?);
                break; // no more input allowed.
            } else {
                return Err(lookahead.error());
            }
        }

        // Q: What ensures that there are no unhandled
        // tokens left for us?

        return Ok(SubstIf { tests, otherwise });
    }
}

impl SubstIf {
    fn expand(&self, ctx: &Context, out: &mut TokenStream) -> syn::Result<()> {
        for (condition, consequence) in &self.tests {
            dbg!(&condition);
            if condition.eval_bool(ctx)? {
                dbg!(&consequence);
                consequence.expand(ctx, out)?;
                return Ok(());
            }
        }
        if let Some(consequence) = &self.otherwise {
            dbg!(&consequence);
            consequence.expand(ctx, out)?;
        }
        Ok(())
    }

    fn analyse_repeat(&self, visitor: &mut RepeatAnalysisVisitor) {
        for (cond, _) in &self.tests {
            cond.analyse_repeat(visitor);
            // TODO: diziet says not to recurse into the consequent
            // bodies.  Not sure why; let's document.
        }
        if let Some(consequence) = &self.otherwise {
            consequence.analyse_repeat(visitor);
        }
    }
}

struct Found;

fn is_found(r: Result<(), Found>) -> bool {
    r.is_err()
}

impl Subst {
    fn eval_bool(&self, ctx: &Context) -> syn::Result<bool> {
        // TODO this is calling out for some generic stuff
        macro_rules! eval_attr { { $wa:expr, $for:ident, $($pattrs:tt)* } => {
            is_found(ctx.$for(|_ctx, within| {
                $wa.search_eval_bool(&within . $($pattrs)*)
            }))
        } }

        let r = match &self.sd {
            SD::tattr(wa) => is_found(wa.search_eval_bool(&&ctx.tattrs)),
            SD::vattr(wa) => eval_attr!{ wa, for_variants, pattrs },
            SD::fattr(wa) => eval_attr!{ wa, for_fields, pfield.pattrs },

            SD::False => false,
            SD::True => true,

            SD::not(v) => ! v.eval_bool(ctx)?,
            SD::any(vs) =>
                vs.iter().find_map(|v| match v.eval_bool(ctx) {
                    Ok(true) => Some(Ok(true)),
                    Err(e) => Some(Err(e)),
                    Ok(false) => None,
                }).unwrap_or(Ok(false))?,
            SD::all(vs) =>
                vs.iter().find_map(|v| match v.eval_bool(ctx) {
                    Ok(true) => None,
                    Err(e) => Some(Err(e)),
                    Ok(false) => Some(Ok(false)),
                }).unwrap_or(Ok(true))?,
            _ => return Err(self.kw.error(
     "derive-adhoc keyword is an expansion - not valid as a condition"
            )),
        };
        Ok(r)
    }
}

#[derive(Debug, Clone)]
struct Context<'c> {
    top: &'c syn::DeriveInput,
    tattrs: &'c PreprocessedAttrs,
    variant: Option<&'c WithinVariant<'c>>,
    field: Option<&'c WithinField<'c>>,
    pvariants: &'c [PreprocessedVariant<'c>],
}

#[derive(Debug, Clone)]
struct PreprocessedVariant<'f> {
    fields: &'f syn::Fields,
    pattrs: PreprocessedAttrs,
    pfields: Vec<PreprocessedField>,
}

#[derive(Debug, Clone)]
struct PreprocessedField {
    pattrs: PreprocessedAttrs,
}

type PreprocessedAttr = syn::Meta;
type PreprocessedAttrs = Vec<PreprocessedAttr>;

#[derive(Debug, Clone)]
struct WithinVariant<'c> {
    variant: Option<&'c syn::Variant>,
    fields: &'c syn::Fields,
    pattrs: &'c PreprocessedAttrs,
    pfields: &'c [PreprocessedField],
}

#[derive(Debug, Clone)]
struct WithinField<'c> {
    field: &'c syn::Field,
    pfield: &'c PreprocessedField,
    index: u32,
}

impl Template {
    fn expand(&self, ctx: &Context, out: &mut TokenStream) {
        for element in &self.elements {
            let () = element
                .expand(ctx, out)
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
    fn expand(&self, ctx: &Context, out: &mut TokenStream) -> syn::Result<()> {
        match self {
            TE::Pass(tt) => out.extend([tt.clone()]),
            TE::Group {
                delim_span,
                delimiter,
                template,
            } => {
                use proc_macro2::Group;
                let mut content = TokenStream::new();
                template.expand(ctx, &mut content);
                let mut group = Group::new(delimiter.clone(), content);
                group.set_span(delim_span.clone());
                out.extend([TT::Group(group)]);
            }
            TE::Subst(exp) => {
                exp.expand(ctx, out)?;
            }
            TE::Repeat(repeated_template) => {
                repeated_template.expand(ctx, out);
            }
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
            TE::Pass(_) => {}
            TE::Repeat(_) => {}
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
    fn expand(&self, ctx: &Context, out: &mut TokenStream) -> syn::Result<()> {
        match &self.sd {
            SD::tname => ctx.top.ident.to_tokens(out),
            SD::ttype => {
                ctx.top.ident.to_tokens(out);
                ctx.top.generics.to_tokens(out);
            }
            SD::vname => ctx.syn_variant(self)?.ident.to_tokens(out),
            SD::fname => {
                let f = ctx.field(self)?;
                if let Some(fname) = &f.field.ident {
                    // todo is this the right span to emit?
                    fname.to_tokens(out);
                } else {
                    syn::Index {
                        index: f.index,
                        span: self.kw.span(),
                    }
                    .to_tokens(out);
                }
            }
            SD::ftype => {
                let f = ctx.field(self)?;
                f.field.ty.to_tokens(out);
            }
            SD::tattr(wa) => wa.expand(ctx, out, &ctx.tattrs)?,
            SD::vattr(wa) => wa.expand(ctx, out, &ctx.variant(wa)?.pattrs)?,
            SD::fattr(wa) => {
                wa.expand(ctx, out, &ctx.field(wa)?.pfield.pattrs)?
            }

            SD::when(when) => when.unfiltered_when(out),
            SD::If(conds) => conds.expand(ctx, out)?,
            SD::False | SD::True | SD::not(_) | SD::any(_) | SD::all(_) => self.not_expansion(out),
        };
        Ok(())
    }

    fn analyse_repeat(&self, visitor: &mut RepeatAnalysisVisitor) {
        let over = match &self.sd {
            SD::tname => None,
            SD::tattr(_) => None,
            SD::ttype => None,
            SD::vname => Some(RO::Variants),
            SD::vattr(_) => Some(RO::Variants),
            SD::fname => Some(RO::Fields),
            SD::ftype => Some(RO::Fields),
            SD::fattr(_) => Some(RO::Fields),
            SD::when(_) => None, // out-of-place when, ignore it
            SD::not(cond) => {
                cond.analyse_repeat(visitor);
                None
            }
            SD::If(conds) => {
                conds.analyse_repeat(visitor);
                None
            }
            SD::any(conds) | SD::all(conds) => {
                conds.iter().for_each(|c| c.analyse_repeat(visitor));
                None
            }
            SD::False | SD::True => None, // condition: ignore.
        };
        if let Some(over) = over {
            let over = RepeatOverInference {
                over,
                span: self.kw.span(),
            };
            visitor.set_over(over);
        }
    }
}

impl Subst {
    fn unfiltered_when(&self, out: &mut TokenStream) {
        out.write_error(self, "${when } only allowed in toplevel of $( )");
    }
    fn not_expansion(&self, out: &mut TokenStream) {
        out.write_error(
            self,
            "derive-adhoc keyword is a condition - not valid as an expansion",
        )
    }
}

enum Todo {}

enum AttrValue<'l> {
    Unit,
    Deeper,
    Lit(&'l syn::Lit),
}

use AttrValue as AV;

impl SubstAttr {
    fn expand(
        &self,
        _ctx: &Context,
        out: &mut TokenStream,
        pattrs: &PreprocessedAttrs,
    ) -> syn::Result<()> {
        let mut found = None;

        self.search(pattrs, &mut |av: AttrValue| {
            if found.is_some() {
                return Err(self.error(
 "tried to expand just attribute value, but it was specified multiple times"
                ));
            }
            let mut buf = TokenStream::new();
            av.expand(self.span(), &mut buf)?;
            found = Some(buf);
            Ok(())
        })?;

        let found = found.ok_or_else(|| {
            self.error(
 "attribute value expanded, but no value in data structure definition"
        )
        })?;

        out.extend(found);
        Ok(())
    }

    fn search_eval_bool(
        &self,
        pattrs: &PreprocessedAttrs,
    ) -> Result<(), Found> {
        self.search(pattrs, &mut |_av| /* got it! */ Err(Found))
    }

    fn search<'a, A, F, E>(&self, pattrs: A, f: &mut F) -> Result<(), E>
    where
        F: FnMut(AttrValue<'a>) -> Result<(), E>,
        A: IntoIterator<Item = &'a PreprocessedAttr>,
    {
        for pattr in pattrs {
            self.search_1(pattr, &mut *f)?;
        }
        Ok(())
    }

    fn search_1<'a, E, F>(
        &self,
        pattr: &'a PreprocessedAttr,
        f: &mut F,
    ) -> Result<(), E>
    where
        F: FnMut(AttrValue<'a>) -> Result<(), E>,
    {
        #[allow(non_camel_case_types)]
        use syn::Meta as sM;

        if pattr.path() != &self.path {
            return Ok(());
        }

        match (&self.deeper, pattr) {
            (None, sM::Path(_)) => f(AV::Unit)?,
            (None, sM::List(_)) => f(AV::Deeper)?,
            (None, sM::NameValue(nv)) => f(AV::Lit(&nv.lit))?,
            (Some(_), sM::NameValue(_)) => {}
            (Some(_), sM::Path(_)) => {} // self is deeper than pattr
            (Some(d), sM::List(l)) => {
                for nm in &l.nested {
                    let m = match nm {
                        syn::NestedMeta::Meta(m) => m,
                        syn::NestedMeta::Lit(_) => continue,
                    };
                    d.search_1(m, &mut *f)?;
                }
            }
        }
        Ok(())
    }
}

impl<'l> AttrValue<'l> {
    fn expand(&self, span: Span, out: &mut TokenStream) -> syn::Result<()> {
        let lit = match self {
            AttrValue::Unit => return Err(span.error(
 "tried to expand attribute which is just a unit, not a literal"
            )),
            AttrValue::Deeper => return Err(span.error(
 "tried to expand attribute which is nested list, not a value",
            )),
            AttrValue::Lit(lit) => lit,
        };
        lit.to_tokens(out);
        Ok(())
    }
}

impl<'c> Context<'c> {
    fn for_variants<F, E>(&self, mut call: F) -> Result<(), E>
    where
        F: FnMut(&Context, &WithinVariant) -> Result<(), E>,
    {
        let ctx = self;
        let mut within_variant = |variant, pvariant: &PreprocessedVariant| {
            let fields = &pvariant.fields;
            let pattrs = &pvariant.pattrs;
            let pfields = &pvariant.pfields;
            let wv = WithinVariant {
                variant,
                fields,
                pattrs,
                pfields,
            };
            let wv = &wv;
            let ctx = Context {
                variant: Some(wv),
                ..*ctx
            };
            call(&ctx, wv)
        };
        match &ctx.top.data {
            syn::Data::Enum(syn::DataEnum { variants, .. }) => {
                for (variant, pvariant) in izip!(variants, ctx.pvariants) {
                    within_variant(Some(variant), pvariant)?;
                }
            }
            syn::Data::Struct(_) | syn::Data::Union(_) => {
                within_variant(None, &ctx.pvariants[0])?;
            }
        }
        Ok(())
    }

    fn for_with_variant<F, E>(&self, mut call: F) -> Result<(), E>
    where
        F: FnMut(&Context, &WithinVariant) -> Result<(), E>,
    {
        let ctx = self;
        if let Some(wv) = &self.variant {
            call(ctx, wv)?;
        } else {
            ctx.for_variants(call)?;
        }
        Ok(())
    }

    fn variant(&self, why: &dyn JustSpanned) -> syn::Result<&WithinVariant> {
        // TODO helper function, maybe ext trait on Spanned, for syn::Error
        let r = self.variant.as_ref().ok_or_else(|| {
            syn::Error::new(
                why.jspan(),
                "must be within a variant (so, in a repeat group)",
            )
        })?;
        Ok(r)
    }

    fn syn_variant(
        &self,
        why: &dyn JustSpanned,
    ) -> syn::Result<&syn::Variant> {
        let r = self.variant(why)?.variant.as_ref().ok_or_else(|| {
            syn::Error::new(why.jspan(), "expansion only valid in enums")
        })?;
        Ok(r)
    }

    fn for_fields<F, E>(&self, mut call: F) -> Result<(), E>
    where
        F: FnMut(&Context, &WithinField) -> Result<(), E>,
    {
        let ctx = self;
        ctx.for_with_variant(|ctx, variant| {
            for (index, (field, pfield)) in
                izip!(variant.fields, variant.pfields,).enumerate()
            {
                let index = index.try_into().expect(">=2^32 fields!");
                let wf = WithinField {
                    field,
                    index,
                    pfield,
                };
                let wf = &wf;
                let ctx = Context {
                    field: Some(wf),
                    ..*ctx
                };
                call(&ctx, wf)?;
            }
            Ok(())
        })
    }

    fn field(&self, why: &dyn Spanned) -> syn::Result<&WithinField> {
        let r = self.field.as_ref().ok_or_else(|| {
            syn::Error::new(
                why.span(),
                "expansion must be within a field (so, in a repeat group)",
            )
        })?;
        Ok(r)
    }
}

impl RepeatedTemplate {
    fn parse(input: ParseStream) -> syn::Result<TemplateElement> {
        let template;
        let paren = parenthesized!(template in input);
        let mut template: Template = template.parse()?;

        // split `when` (and [todo] `for`) off
        let mut whens = vec![];
        let mut elements = Vec::with_capacity(template.elements.len());
        let mut beginning = true;
        for elem in template.elements.drain(..) {
            let not_special = match elem {
                _ if !beginning => elem,
                TE::Pass(_) => elem,

                TE::Subst(subst) => match subst.sd {
                    SD::when(when) => {
                        whens.push(when);
                        continue;
                    }
                    _ => TE::Subst(subst),
                },

                _ => {
                    beginning = false;
                    elem
                }
            };
            elements.push(not_special);
        }
        template.elements = elements;

        let mut visitor = RepeatAnalysisVisitor::default();
        template.analyse_repeat(&mut visitor);
        let over = visitor.finish(paren.span);

        Ok(match over {
            Ok(over) => TE::Repeat(RepeatedTemplate {
                over,
                template,
                whens,
            }),
            Err(errs) => TE::Errors(errs),
        })
    }
}

impl RepeatedTemplate {
    fn expand(&self, ctx: &Context, out: &mut TokenStream) {
        match self.over {
            RO::Variants => ctx.for_variants(|ctx, _variant| {
                Ok::<_, Void>(self.expand_inner(ctx, out))
            }),
            RO::Fields => ctx.for_fields(|ctx, _field| {
                Ok::<_, Void>(self.expand_inner(ctx, out))
            }),
        }
        .void_unwrap()
    }

    /// private, does the condition
    fn expand_inner(&self, ctx: &Context, out: &mut TokenStream) {
        for when in &self.whens {
            match when.eval_bool(ctx) {
                Ok(true) => continue,
                Ok(false) => return,
                Err(e) => {
                    out.extend([e.into_compile_error()]);
                    return;
                }
            }
        }
        self.template.expand(ctx, out)
    }
}

fn preprocess_attrs(
    attrs: &[syn::Attribute],
) -> syn::Result<PreprocessedAttrs> {
    attrs
        .iter()
        .filter_map(|attr| {
            // infallible filtering for attributes we are interested in
            match attr.style {
                syn::AttrStyle::Outer => {}
                syn::AttrStyle::Inner(_) => return None,
            };
            if attr.path.leading_colon.is_some() {
                return None;
            }
            let segment = attr.path.segments.iter().exactly_one().ok()?;
            if segment.ident != "adhoc" {
                return None;
            }
            Some(attr)
        })
        .map(|attr| {
            let attr: AdhocAttrList = syn::parse2(attr.tokens.clone())?;
            Ok(attr.meta.into_iter())
        })
        .flatten_ok()
        .collect()
}

fn preprocess_fields(
    fields: &syn::Fields,
) -> syn::Result<Vec<PreprocessedField>> {
    let fields = match fields {
        syn::Fields::Named(f) => &f.named,
        syn::Fields::Unnamed(f) => &f.unnamed,
        syn::Fields::Unit => return Ok(vec![]),
    };
    fields
        .into_iter()
        .map(|field| {
            let pattrs = preprocess_attrs(&field.attrs)?;
            Ok(PreprocessedField { pattrs })
        })
        .collect()
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
pub fn derive_adhoc_expand_func_macro(
    input: TokenStream,
) -> syn::Result<TokenStream> {
    let input: SubstInput = syn::parse2(input)?;
    let ident = &input.driver.ident;
    dbg!(&ident);

    let tattrs = preprocess_attrs(&input.driver.attrs)?;

    let pvariants_one = |fields| {
        let pattrs = vec![];
        let pfields = preprocess_fields(fields)?;
        let pvariant = PreprocessedVariant {
            fields,
            pattrs,
            pfields,
        };
        syn::Result::Ok(vec![pvariant])
    };

    let union_fields;

    let pvariants = match &input.driver.data {
        syn::Data::Struct(ds) => pvariants_one(&ds.fields)?,
        syn::Data::Union(du) => {
            union_fields = syn::Fields::Named(du.fields.clone());
            pvariants_one(&union_fields)?
        }
        syn::Data::Enum(de) => de
            .variants
            .iter()
            .map(|variant| {
                let fields = &variant.fields;
                let pattrs = preprocess_attrs(&variant.attrs)?;
                let pfields = preprocess_fields(&variant.fields)?;
                Ok(PreprocessedVariant {
                    fields,
                    pattrs,
                    pfields,
                })
            })
            .collect::<Result<Vec<_>, syn::Error>>()?,
    };

    // maybe we should be using syn::buffer::TokenBuffer ?
    // or Vec<TokenTree>, which we parse into a tree of our own full
    // of [TokenTree] ?
    let ctx = Context {
        top: &input.driver,
        tattrs: &tattrs,
        field: None,
        variant: None,
        pvariants: &pvariants,
    };
    let mut output = TokenStream::new();
    input.template.expand(&ctx, &mut output);

    // obviously nothing should print to stderr
    //    dbg!(&&output);
    eprintln!("---------- derive_adhoc_expand got start ----------");
    eprintln!("{}", &output);
    eprintln!("---------- derive_adhoc_expand got end ----------");
    Ok(output.into())
}
