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
}

#[derive(Debug)]
struct RepeatedTemplate {
    template: Template,
    #[allow(clippy::vec_box)]
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
// wrinkles (and may help avoid semantic wrinkles). -Diziet
//
// Hrm, actually, compare
//     ${if fmeta(foo) { ...
//     ${fmeta(foo) as ty}
// and this demonstrates different parsing.  We want to forbid
//     ${if fmeta(foo) as ty { ...
// and right now that is a bit funny.

enum SubstDetails {
    // variables
    tname,
    ttype,
    vname,
    fname,
    ftype,

    // attributes
    tmeta(SubstAttr),
    vmeta(SubstAttr),
    fmeta(SubstAttr),
    tattrs(RawAttr),
    vattrs(RawAttr),
    fattrs(RawAttr),

    // generics
    tgens,
    tgnames,
    twheres,

    // special
    when(Box<Subst>),

    // expressions
    False,
    True,
    not(Box<Subst>),
    any(Punctuated<Subst, token::Comma>),
    all(Punctuated<Subst, token::Comma>),
    is_enum,

    // Explicit iteration
    For(RepeatedTemplate),
    // Conditional substitution.
    If(SubstIf),
}

#[derive(Debug)]
struct SubstIf {
    // A series of test/result pairs.  The test that gives "true"
    // short-circuits the rest.
    tests: Vec<(Subst, Template)>,
    // A final element to expand if all tests fail.
    otherwise: Option<Box<Template>>,
}

use SubstDetails as SD;

#[derive(Debug, Clone)]
struct SubstAttr {
    path: SubstAttrPath,
    as_: SubstAttrAs,
    as_span: Span,
}

#[derive(Debug, Clone, AsRefStr, Display, EnumIter)]
#[allow(non_camel_case_types)] // clearer to use the exact ident
enum SubstAttrAs {
    lit,
    ty,
}

#[derive(Debug, Clone)]
struct SubstAttrPath {
    path: syn::Path, // nonempty segments
    deeper: Option<Box<SubstAttrPath>>,
}

impl Spanned for SubstAttr {
    fn span(&self) -> Span {
        self.path.span()
    }
}

impl Spanned for SubstAttrPath {
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
}

impl RepeatAnalysisVisitor {
    fn set_over(&mut self, over: RepeatOverInference) -> syn::Result<()> {
        match &self.over {
            None => self.over = Some(over),
            Some(already) => {
                if already.over != over.over {
                    let mut e1 = already.span.error(format!(
 "inconsistent repetition depth: firstly, {} inferred here",
                            already.over,
                        ));
                    let e2 = over.span.error(format!(
 "inconsistent repetition depth: secondly, {} inferred here",
                            over.over,
                        ));
                    e1.combine(e2);
                    return Err(e1);
                }
            }
        }
        Ok(())
    }

    fn finish(self, start: Span) -> Result<RepeatOver, syn::Error> {
        Ok(self
            .over
            .ok_or_else(|| {
                start.error(
            "no contained expansion field determined what to repeat here"
        )
            })?
            .over)
    }
}

impl Parse for Template {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // eprintln!("@@@@@@@@@@ PARSE {}", &input);
        let mut good = vec![];
        let mut errors = ErrorAccumulator::default();
        while !input.is_empty() {
            errors.handle_in(|| {
                let elem = input.parse()?;
                good.push(elem);
                Ok(())
            });
        }
        errors.finish_with(Template { elements: good })
    }
}

impl Parse for TemplateElement {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(match input.parse()? {
            TT::Group(group) => {
                let delim_span = group.span_open();
                let delimiter = group.delimiter();
                let template: Template = syn::parse2(group.stream())?;
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
                    RepeatedTemplate::parse_in_parens(input)?
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
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let path: SubstAttrPath = input.parse()?;

        let as_;
        let as_span;

        if input.peek(Token![as]) {
            let _: Token![as] = input.parse()?;
            let kw = input.call(syn::Ident::parse_any)?;
            as_span = kw.span();
            as_ = SubstAttrAs::iter().find(|as_| kw == as_).ok_or_else(
                || kw.error("unknown derive-adhoc 'as' syntax type keyword"),
            )?;
        } else {
            as_ = SubstAttrAs::lit;
            as_span = path.span();
        }

        Ok(SubstAttr { path, as_, as_span })
    }
}

impl Parse for SubstAttrPath {
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

        Ok(SubstAttrPath { path, deeper })
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
        keyword! { is_enum }

        keyword! { tgens }
        keyword! { tgnames }
        keyword! { twheres }

        keyword! { tmeta(input.parse()?) }
        keyword! { vmeta(input.parse()?) }
        keyword! { fmeta(input.parse()?) }

        keyword! { tattrs(input.parse()?) }
        keyword! { vattrs(input.parse()?) }
        keyword! { fattrs(input.parse()?) }

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
        if kw == "for" {
            return from_sd(SD::For(RepeatedTemplate::parse_for(input)?));
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

        Err(kw.error("unknown derive-adhoc keyword"))
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
            }
            let _else: Token![else] = input.parse()?;

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

        Ok(SubstIf { tests, otherwise })
    }
}

impl SubstIf {
    fn expand(&self, ctx: &Context, out: &mut TokenStream) -> syn::Result<()> {
        for (condition, consequence) in &self.tests {
            //dbg!(&condition);
            if condition.eval_bool(ctx)? {
                //dbg!(&consequence);
                consequence.expand(ctx, out);
                return Ok(());
            }
        }
        if let Some(consequence) = &self.otherwise {
            //dbg!(&consequence);
            consequence.expand(ctx, out);
        }
        Ok(())
    }

    fn analyse_repeat(
        &self,
        visitor: &mut RepeatAnalysisVisitor,
    ) -> syn::Result<()> {
        for (cond, _) in &self.tests {
            cond.analyse_repeat(visitor)?;
            // TODO: diziet says not to recurse into the consequent
            // bodies.  Not sure why; let's document.
        }
        if let Some(consequence) = &self.otherwise {
            consequence.analyse_repeat(visitor)?;
        }
        Ok(())
    }
}

struct Found;

fn is_found(r: Result<(), Found>) -> bool {
    r.is_err()
}

impl Subst {
    fn eval_bool(&self, ctx: &Context) -> syn::Result<bool> {
        // TODO this is calling out for some generic stuff
        // eprintln!("@@@@@@@@@@@@@@@@@@@@ EVAL {:?}", self);

        macro_rules! eval_attr { { $wa:expr, $lev:ident, $($pattrs:tt)* } => {
            is_found(ctx.for_with_within::<$lev,_,_>(|_ctx, within| {
                $wa.path.search_eval_bool(&within . $($pattrs)*)
            }))
        } }

        let r = match &self.sd {
            SD::tmeta(wa) => is_found(wa.path.search_eval_bool(ctx.tattrs)),
            SD::vmeta(wa) => eval_attr!{ wa, WithinVariant, pattrs },
            SD::fmeta(wa) => eval_attr!{ wa, WithinField, pfield.pattrs },
            SD::is_enum => matches!(ctx.top.data, syn::Data::Enum(_)),

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
    fn analyse_repeat(
        &self,
        visitor: &mut RepeatAnalysisVisitor,
    ) -> syn::Result<()> {
        for element in &self.elements {
            element.analyse_repeat(visitor)?;
        }
        Ok(())
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
                let mut group = Group::new(*delimiter, content);
                group.set_span(*delim_span);
                out.extend([TT::Group(group)]);
            }
            TE::Subst(exp) => {
                exp.expand(ctx, out)?;
            }
            TE::Repeat(repeated_template) => {
                repeated_template.expand(ctx, out);
            }
        }
        Ok(())
    }

    fn analyse_repeat(
        &self,
        visitor: &mut RepeatAnalysisVisitor,
    ) -> syn::Result<()> {
        match self {
            TE::Pass(_) => {}
            TE::Repeat(_) => {}
            TE::Group { template, .. } => template.analyse_repeat(visitor)?,
            TE::Subst(exp) => exp.analyse_repeat(visitor)?,
        }
        Ok(())
    }
}

impl Spanned for Subst {
    fn span(&self) -> Span {
        self.kw.span()
    }
}

impl Subst {
    fn expand(&self, ctx: &Context, out: &mut TokenStream) -> syn::Result<()> {
        // eprintln!("@@@@@@@@@@@@@@@@@@@@ EXPAND {:?}", self);

        let do_meta = |wa: &SubstAttr, out, meta| wa.expand(ctx, out, meta);
        let do_tgnames = |out: &mut _| {
            for pair in ctx.top.generics.params.pairs() {
                use syn::GenericParam as GP;
                match pair.value() {
                    GP::Type(t) => t.ident.to_tokens(out),
                    GP::Const(c) => c.ident.to_tokens(out),
                    GP::Lifetime(l) => l.lifetime.to_tokens(out),
                }
                pair.punct().to_tokens_punct_composable(out);
            }
        };

        match &self.sd {
            SD::tname => ctx.top.ident.to_tokens(out),
            SD::ttype => {
                ctx.top.ident.to_tokens(out);

                let gens = &ctx.top.generics;
                match (&gens.lt_token,&gens.gt_token) {
                    (None, None) => (),
                    (Some(lt), Some(rt)) => {
                        lt.to_tokens(out);
                        do_tgnames(out);
                        rt.to_tokens(out);
                    }
                    _ => panic!("unmatched < > in syn::Generics {:?}", gens),
                }
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
            SD::tmeta(wa) => do_meta(wa, out, ctx.tattrs)?,
            SD::vmeta(wa) => do_meta(wa, out, ctx.variant(wa)?.pattrs)?,
            SD::fmeta(wa) => do_meta(wa, out, &ctx.field(wa)?.pfield.pattrs)?,

            SD::tattrs(ra) => ra.expand(ctx, out, &ctx.top.attrs)?,
            SD::vattrs(ra) => {
                let variant = ctx.variant(self)?.variant;
                let attrs = variant.as_ref().map(|v| &*v.attrs);
                ra.expand(ctx, out, attrs.unwrap_or_default())?;
            }
            SD::fattrs(ra) => {
                ra.expand(ctx, out, &ctx.field(self)?.field.attrs)?
            }

            SD::tgens => ctx.top.generics.params.to_tokens(out),
            SD::tgnames => do_tgnames(out),
            SD::twheres => {
                if let Some(clause) = &ctx.top.generics.where_clause {
                    clause.predicates.to_tokens_punct_composable(out);
                }
            }

            SD::when(when) => when.unfiltered_when(out),
            SD::If(conds) => conds.expand(ctx, out)?,
            SD::is_enum
            | SD::False
            | SD::True
            | SD::not(_)
            | SD::any(_)
            | SD::all(_) => self.not_expansion(out),
            SD::For(repeat) => repeat.expand(ctx, out),
        };
        Ok(())
    }

    fn analyse_repeat(
        &self,
        visitor: &mut RepeatAnalysisVisitor,
    ) -> syn::Result<()> {
        let over = match &self.sd {
            SD::tname => None,
            SD::vname => Some(RO::Variants),
            SD::fname => Some(RO::Fields),
            SD::ttype => None,
            // TODO vtype
            SD::ftype => Some(RO::Fields),
            SD::tmeta(_) => None,
            SD::vmeta(_) => Some(RO::Variants),
            SD::fmeta(_) => Some(RO::Fields),
            SD::tattrs(_) => None,
            SD::vattrs(_) => Some(RO::Variants),
            SD::fattrs(_) => Some(RO::Fields),
            SD::tgens => None,
            SD::tgnames => None,
            SD::twheres => None,
            SD::is_enum => None,
            SD::when(_) => None, // out-of-place when, ignore it
            SD::not(cond) => {
                cond.analyse_repeat(visitor)?;
                None
            }
            SD::If(conds) => {
                conds.analyse_repeat(visitor)?;
                None
            }
            SD::any(conds) | SD::all(conds) => {
                for c in conds.iter() {
                    c.analyse_repeat(visitor)?;
                }
                None
            }
            // Has a RepeatOver, but does not imply anything about its context.
            SD::For(_) => None,
            SD::False | SD::True => None, // condition: ignore.
        };
        if let Some(over) = over {
            let over = RepeatOverInference {
                over,
                span: self.kw.span(),
            };
            visitor.set_over(over)?;
        }
        Ok(())
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

        self.path.search(pattrs, &mut |av: AttrValue| {
            if found.is_some() {
                return Err(self.error(
 "tried to expand just attribute value, but it was specified multiple times"
                ));
            }
            let mut buf = TokenStream::new();
            av.expand(self.span(), &self.as_, &mut buf)?;
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
}

impl SubstAttrPath {
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
        #![allow(non_camel_case_types)]
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
    fn expand(
        &self,
        span: Span,
        as_: &SubstAttrAs,
        out: &mut TokenStream,
    ) -> syn::Result<()> {
        let lit = match self {
            AttrValue::Unit => return Err(span.error(
 "tried to expand attribute which is just a unit, not a literal"
            )),
            AttrValue::Deeper => return Err(span.error(
 "tried to expand attribute which is nested list, not a value",
            )),
            AttrValue::Lit(lit) => lit,
        };

        fn lit_as<T>(
            lit: &syn::Lit,
            span: Span,
            as_: &SubstAttrAs,
            out: &mut TokenStream,
        ) -> syn::Result<()>
        where
            T: Parse + ToTokens,
        {
            let s: &syn::LitStr = match lit {
                syn::Lit::Str(s) => s,
                // having checked derive_builder, it doesn't handle
                // Lit::Verbatim so I guess we don't need to either.
                _ => {
                    return Err(span.error(format!(
                        "expected string literal, for conversion to {}",
                        as_,
                    )))
                }
            };

            let thing: T = s.parse()?;
            thing.to_tokens(out);
            Ok(())
        }

        use SubstAttrAs as SAS;
        match as_ {
            SAS::lit => lit.to_tokens(out),
            SAS::ty => lit_as::<syn::Type>(lit, span, as_, out)?,
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
enum RawAttr {
    Include {
        entries: Punctuated<RawAttrEntry, token::Comma>,
    },
    Exclude {
        exclusions: Punctuated<syn::Path, token::Comma>,
    },
}

#[derive(Debug, Clone)]
struct RawAttrEntry {
    path: syn::Path,
}

impl RawAttr {
    fn expand(
        &self,
        ctx: &Context,
        out: &mut TokenStream,
        attrs: &[syn::Attribute],
    ) -> syn::Result<()> {
        for attr in attrs {
            match self {
                RawAttr::Include { entries } => {
                    let ent = entries.iter().find(|ent| ent.matches(attr));
                    if let Some(ent) = ent {
                        ent.expand(ctx, out, attr)?;
                    }
                }
                RawAttr::Exclude { exclusions } => {
                    if !exclusions.iter().any(|excl| excl == &attr.path) {
                        attr.to_tokens(out);
                    }
                }
            }
        }
        Ok(())
    }
}

impl RawAttrEntry {
    fn matches(&self, attr: &syn::Attribute) -> bool {
        &self.path == &attr.path
    }

    fn expand(
        &self,
        _ctx: &Context,
        out: &mut TokenStream,
        attr: &syn::Attribute,
    ) -> syn::Result<()> {
        attr.to_tokens(out);
        Ok(())
    }

    fn simple(self, _negated: &Token![!]) -> syn::Result<syn::Path> {
        Ok(self.path)
    }
}

impl Parse for RawAttr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let la = input.lookahead1();
        let negated;
        if la.peek(Token![!]) {
            negated = Some(input.parse()?);
        } else if la.peek(Token![=]) {
            let _: Token![=] = input.parse()?;
            negated = None;
        } else {
            negated = None;
        }

        let entries: Punctuated<RawAttrEntry, _> =
            input.call(Punctuated::parse_terminated)?;

        if let Some(negated) = &negated {
            let exclusions = entries
                .into_iter()
                .map(|ent| ent.simple(negated))
                .try_collect()?;
            Ok(RawAttr::Exclude { exclusions })
        } else {
            Ok(RawAttr::Include { entries })
        }
    }
}

impl Parse for RawAttrEntry {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let path = input.parse()?;
        Ok(RawAttrEntry { path })
    }
}

/// Implemented for [`WithinVariant`] and [`WithinField`]
///
/// For combining code that applies similarly for different repeat levels.
trait WithinRepeatLevel<'w>: 'w {
    fn level_display_name() -> &'static str;

    fn current(ctx: &'w Context) -> Option<&'w Self>;

    /// Iterate over all the things at this level
    ///
    /// If it needs a current container of the next level up (eg, a
    /// field needing a variant) and there is none current, iterates
    /// over containing things too.
    fn for_each<'c, F, E>(ctx: &'c Context<'c>, call: F) -> Result<(), E>
    where
        'c: 'w,
        F: FnMut(&Context, &Self) -> Result<(), E>;
}

impl<'w> WithinRepeatLevel<'w> for WithinVariant<'w> {
    fn level_display_name() -> &'static str {
        "variant"
    }

    fn current(ctx: &'w Context) -> Option<&'w WithinVariant<'w>> {
        ctx.variant
    }

    fn for_each<'c, F, E>(ctx: &'c Context<'c>, mut call: F) -> Result<(), E>
    where
        'c: 'w,
        F: FnMut(&Context, &WithinVariant<'w>) -> Result<(), E>,
    {
        let mut within_variant = |variant, ppv: &'c PreprocessedVariant| {
            let fields = &ppv.fields;
            let pattrs = &ppv.pattrs;
            let pfields = &ppv.pfields;
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
}

impl<'w> WithinRepeatLevel<'w> for WithinField<'w> {
    fn level_display_name() -> &'static str {
        "field"
    }

    fn current(ctx: &'w Context) -> Option<&'w WithinField<'w>> {
        ctx.field
    }

    fn for_each<'c, F, E>(ctx: &'c Context<'c>, mut call: F) -> Result<(), E>
    where
        'c: 'w,
        F: FnMut(&Context, &WithinField<'w>) -> Result<(), E>,
    {
        ctx.for_with_within(|ctx, variant: &WithinVariant| {
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
}

impl<'c> Context<'c> {
    /// Obtains the relevant `Within`(s), and calls `call` for each one
    ///
    /// If there is a current `W`, simply calls `call`.
    /// Otherwise, iterates over all of them and calls `call` for each one.
    fn for_with_within<'w, W, F, E>(&'c self, mut call: F) -> Result<(), E>
    where
        'c: 'w,
        W: WithinRepeatLevel<'w>,
        F: FnMut(&Context, &W) -> Result<(), E>,
    {
        let ctx = self;
        if let Some(w) = W::current(ctx) {
            call(ctx, w)?;
        } else {
            W::for_each(ctx, call)?;
        }
        Ok(())
    }

    /// Obtains the current `Within` of type `W`
    ///
    /// Demands that there actually is a current container `W`.
    /// If we aren't, calls it an error.
    fn within_level<W>(&'c self, why: &dyn Spanned) -> syn::Result<&W>
    where
        W: WithinRepeatLevel<'c>,
    {
        // TODO helper function, maybe ext trait on Spanned, for syn::Error
        let r = W::current(self).ok_or_else(|| {
            syn::Error::new(
                why.span(),
                format_args!(
                    "must be within a {} (so, in a repeat group)",
                    W::level_display_name(),
                ),
            )
        })?;
        Ok(r)
    }

    /// Obtains the current field (or calls it an error)
    fn field(&self, why: &dyn Spanned) -> syn::Result<&WithinField> {
        self.within_level(why)
    }
    /// Obtains the current variant (or calls it an error)
    fn variant(&self, why: &dyn Spanned) -> syn::Result<&WithinVariant> {
        self.within_level(why)
    }
    /// Obtains the current variant as a `syn::Variant`
    fn syn_variant(&self, why: &dyn Spanned) -> syn::Result<&syn::Variant> {
        let r = self.variant(why)?.variant.as_ref().ok_or_else(|| {
            syn::Error::new(why.span(), "expansion only valid in enums")
        })?;
        Ok(r)
    }
}

impl RepeatedTemplate {
    fn parse_in_parens(input: ParseStream) -> syn::Result<TemplateElement> {
        let template;
        let paren = parenthesized!(template in input);
        let rt = RepeatedTemplate::parse(&template, paren.span, None)?;
        Ok(TE::Repeat(rt))
    }

    fn parse_for(input: ParseStream) -> syn::Result<RepeatedTemplate> {
        let over: Ident = input.parse()?;
        let over = if over == "fields" {
            RepeatOver::Fields
        } else if over == "variants" {
            RepeatOver::Variants
        } else {
            return Err(
                over.error("$for must be followed by 'fields' or 'variants'")
            );
        };
        let template;
        let brace = braced!(template in input);
        RepeatedTemplate::parse(&template, brace.span, Some(over))
    }

    fn parse(
        input: ParseStream,
        span: Span,
        over: Option<RepeatOver>,
    ) -> Result<RepeatedTemplate, syn::Error> {
        let mut template: Template = input.parse()?;

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

        let over = match over {
            Some(over) => Ok(over),
            None => {
                let mut visitor = RepeatAnalysisVisitor::default();
                template.analyse_repeat(&mut visitor)?;
                visitor.finish(span)
            }
        };

        match over {
            Ok(over) => Ok(RepeatedTemplate {
                over,
                template,
                whens,
            }),
            Err(errs) => Err(errs),
        }
    }
}

impl RepeatedTemplate {
    fn expand(&self, ctx: &Context, out: &mut TokenStream) {
        // TODO(nickm): Clippy thinks that this Void stuff is
        // gratuituous, but I don't understand it.
        #[allow(clippy::unit_arg)]
        match self.over {
            RO::Variants => ctx.for_with_within(|ctx, _: &WithinVariant| {
                Ok::<_, Void>(self.expand_inner(ctx, out))
            }),
            RO::Fields => ctx.for_with_within(|ctx, _: &WithinField| {
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
    // let ident = input.driver.ident;
    // eprintln!(
    //     "---------- derive_adhoc_expand start for {} ----------",
    //     ident
    // );
    // eprintln!("{}", &output);
    // eprintln!(
    //     "---------- derive_adhoc_expand end for {} ----------",
    //     ident
    // );

    Ok(output)
}
