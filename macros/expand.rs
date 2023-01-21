//! Expansion of a template into output tokens, plus `derive_adhoc_expand!()`
//!
//! Contains the implementations of `fn expand()`
//! for the various template types in [`crate::syntax`].
//!
//! Also contains the top-level "do the work" macro function -
//! the implementation of `derive_adhoc_expand!()`.

use crate::framework::*;

pub enum AttrValue<'l> {
    Unit(Span),
    Deeper(Span),
    Lit(&'l syn::Lit),
}

pub use AttrValue as AV;

impl Spanned for AttrValue<'_> {
    fn span(&self) -> Span {
        match self {
            AV::Unit(span) => *span,
            AV::Deeper(span) => *span,
            AV::Lit(lit) => lit.span(),
        }
    }
}

impl<O> Expand<O> for SubstIf<O>
where
    Template<O>: ExpandInfallible<O>,
    O: ExpansionOutput,
{
    fn expand(&self, ctx: &Context, out: &mut O) -> syn::Result<()> {
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
}

impl<O> SubstIf<O>
where
    Template<O>: ExpandInfallible<O>,
    O: ExpansionOutput,
{
    fn expand_select1(&self, ctx: &Context, out: &mut O) -> syn::Result<()> {
        let mut found: Result<Option<(Span, &Template<O>)>, Vec<ErrorLoc>> =
            Ok(None);

        for (condition, consequence) in &self.tests {
            if !condition.eval_bool(ctx)? {
                continue;
            }
            let cspan = condition.span();
            let error_loc = |span| (span, "true condition");
            match &mut found {
                Ok(None) => found = Ok(Some((cspan, consequence))),
                Ok(Some((span1, _))) => {
                    found = Err(vec![
                        ctx.error_loc(),
                        error_loc(*span1),
                        error_loc(cspan),
                    ])
                }
                Err(several) => several.push(error_loc(cspan)),
            }
        }
        let found = found
            .map_err(|several| several.error("multiple conditions matched"))?
            .map(|(_cspan, consequence)| consequence)
            .or(self.otherwise.as_deref())
            .ok_or_else(|| {
                [ctx.error_loc(), (self.kw_span, "select1 expansion")]
                    .error("no conditions matched, and no else clause")
            })?;
        found.expand(ctx, out);
        Ok(())
    }
}

impl<O> ExpandInfallible<O> for Template<O>
where
    TemplateElement<O>: Expand<O>,
    O: ExpansionOutput,
{
    fn expand(&self, ctx: &Context, out: &mut O) {
        for element in &self.elements {
            let () = element
                .expand(ctx, out)
                .unwrap_or_else(|err| out.record_error(err));
        }
    }
}

impl Expand<TokenAccumulator> for TemplateElement<TokenAccumulator> {
    fn expand(
        &self,
        ctx: &Context,
        out: &mut TokenAccumulator,
    ) -> syn::Result<()> {
        match self {
            TE::Ident(tt) => out.write_tokens(tt.clone()),
            TE::Literal(tt) => out.write_tokens(tt.clone()),
            TE::Punct(tt, _) => out.write_tokens(tt.clone()),
            TE::Group {
                delim_span,
                delimiter,
                template,
                no_paste: _,
            } => {
                use proc_macro2::Group;
                let mut content = TokenAccumulator::new();
                template.expand(ctx, &mut content);
                let mut group = Group::new(*delimiter, content.tokens()?);
                group.set_span(*delim_span);
                out.write_tokens(TT::Group(group));
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
}

impl<O> Expand<O> for Subst<O>
where
    O: ExpansionOutput,
    TemplateElement<O>: Expand<O>,
{
    fn expand(&self, ctx: &Context, out: &mut O) -> syn::Result<()> {
        // eprintln!("@@@@@@@@@@@@@@@@@@@@ EXPAND {:?}", self);

        let do_meta = |wa: &SubstAttr, out, meta| wa.expand(ctx, out, meta);
        let do_tgnames = |out: &mut TokenAccumulator| {
            for pair in ctx.top.generics.params.pairs() {
                use syn::GenericParam as GP;
                match pair.value() {
                    GP::Type(t) => out.write_tokens(&t.ident),
                    GP::Const(c) => out.write_tokens(&c.ident),
                    GP::Lifetime(l) => out.write_tokens(&l.lifetime),
                }
                out.with_tokens(|out| {
                    pair.punct().to_tokens_punct_composable(out);
                });
            }
        };

        match &self.sd {
            SD::tname(_) => out.push_ident(&ctx.top.ident),
            SD::ttype(_) => out.push_idpath(
                self.kw.span(),
                |_| {},
                &ctx.top.ident,
                |out| {
                    let gens = &ctx.top.generics;
                    match (&gens.lt_token, &gens.gt_token) {
                        (None, None) => (),
                        (Some(lt), Some(gt)) => {
                            out.write_tokens(lt);
                            do_tgnames(out);
                            out.write_tokens(gt);
                        }
                        _ => {
                            panic!("unmatched < > in syn::Generics {:?}", gens)
                        }
                    }
                },
            ),
            SD::vname(_) => out.push_ident(&ctx.syn_variant(self)?.ident),
            SD::fname(_) => {
                let f = ctx.field(self)?;
                if let Some(fname) = &f.field.ident {
                    // todo is this the right span to emit?
                    out.push_ident(fname);
                } else {
                    out.push_ident(&syn::Index {
                        index: f.index,
                        span: self.kw.span(),
                    });
                }
            }
            SD::ftype(_) => {
                let f = ctx.field(self)?;
                out.push_syn_type(self.kw.span(), &f.field.ty);
            }
            SD::tmeta(wa) => do_meta(wa, out, ctx.tattrs)?,
            SD::vmeta(wa) => do_meta(wa, out, ctx.variant(wa)?.pattrs)?,
            SD::fmeta(wa) => do_meta(wa, out, &ctx.field(wa)?.pfield.pattrs)?,

            SD::tattrs(ra, np, ..) => {
                out.push_other_subst(np, self, |out| {
                    ra.expand(ctx, out, &ctx.top.attrs)
                })?
            }
            SD::vattrs(ra, np, ..) => {
                out.push_other_subst(np, self, |out| {
                    let variant = ctx.variant(self)?.variant;
                    let attrs = variant.as_ref().map(|v| &*v.attrs);
                    ra.expand(ctx, out, attrs.unwrap_or_default())
                })?
            }
            SD::fattrs(ra, np, ..) => {
                out.push_other_subst(np, self, |out| {
                    ra.expand(ctx, out, &ctx.field(self)?.field.attrs)
                })?
            }

            SD::tgens(np, ..) => out.push_other_subst(np, self, |out| {
                out.write_tokens(&ctx.top.generics.params);
                Ok(())
            })?,
            SD::tgnames(np, ..) => out.push_other_subst(np, self, |out| {
                do_tgnames(out);
                Ok(())
            })?,
            SD::twheres(np, ..) => out.push_other_subst(np, self, |out| {
                if let Some(clause) = &ctx.top.generics.where_clause {
                    out.with_tokens(|out| {
                        clause.predicates.to_tokens_punct_composable(out);
                    });
                }
                Ok(())
            })?,

            SD::paste(content, np, ..) => {
                out.expand_paste(np, ctx, self.span(), content)?
            }
            SD::ChangeCase(content, case, nc, ..) => {
                out.expand_case(nc, *case, ctx, self.span(), content)?
            }

            SD::when(..) => out.write_error(
                self,
                "${when } only allowed in toplevel of $( )",
            ),
            SD::If(conds, ..) => conds.expand(ctx, out)?,
            SD::is_enum(bo)
            | SD::False(bo)
            | SD::True(bo)
            | SD::not(_, bo)
            | SD::any(_, bo)
            | SD::all(_, bo) => out.expand_bool_only(bo),
            SD::For(repeat, _) => repeat.expand(ctx, out),
            SD::select1(conds, ..) => conds.expand_select1(ctx, out)?,
        };
        Ok(())
    }
}

impl SubstAttr {
    fn expand<O>(
        &self,
        ctx: &Context,
        out: &mut O,
        pattrs: &PreprocessedAttrs,
    ) -> syn::Result<()>
    where
        O: ExpansionOutput,
    {
        let mut found = None;
        let error_loc = || [(self.span(), "expansion"), ctx.error_loc()];

        self.path.search(pattrs, &mut |av: AttrValue| {
            if found.is_some() {
                return Err(error_loc().error(
 "tried to expand just attribute value, but it was specified multiple times"
                ));
            }
            found = Some(av);
            Ok(())
        })?;

        let found = found.ok_or_else(|| {
            error_loc().error(
 "attribute value expanded, but no value in data structure definition"
            )
        })?;

        found.expand(self.span(), &self.as_, out)?;

        Ok(())
    }
}

impl<'l> AttrValue<'l> {
    fn expand<O>(
        &self,
        tspan: Span,
        as_: &SubstAttrAs,
        out: &mut O,
    ) -> syn::Result<()>
    where
        O: ExpansionOutput,
    {
        fn spans(tspan: Span, vspan: Span) -> [ErrorLoc; 2] {
            [(vspan, "attribute value"), (tspan, "template")]
        }

        let lit = match self {
            AttrValue::Unit(vspan) => return Err(spans(tspan, *vspan).error(
 "tried to expand attribute which is just a unit, not a literal"
            )),
            AttrValue::Deeper(vspan) => return Err(spans(tspan, *vspan).error(
 "tried to expand attribute which is nested list, not a value",
            )),
            AttrValue::Lit(lit) => lit,
        };

        fn lit_as<T>(
            lit: &syn::Lit,
            tspan: Span,
            as_: &SubstAttrAs,
        ) -> syn::Result<T>
        where
            T: Parse + ToTokens,
        {
            let s: &syn::LitStr = match lit {
                syn::Lit::Str(s) => s,
                // having checked derive_builder, it doesn't handle
                // Lit::Verbatim so I guess we don't need to either.
                _ => {
                    return Err(spans(tspan, lit.span()).error(format!(
                        "expected string literal, for conversion to {}",
                        as_,
                    )))
                }
            };

            let thing: T = s.parse()?;
            Ok(thing)
        }

        use SubstAttrAs as SAS;
        match as_ {
            SAS::lit => out.push_syn_lit(lit),
            SAS::ty => out.push_syn_type(tspan, &lit_as(lit, tspan, as_)?),
        }
        Ok(())
    }
}

impl RawAttr {
    fn expand(
        &self,
        ctx: &Context,
        out: &mut TokenAccumulator,
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
                        out.write_tokens(attr);
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
        out: &mut TokenAccumulator,
        attr: &syn::Attribute,
    ) -> syn::Result<()> {
        out.write_tokens(attr);
        Ok(())
    }
}

impl<O> ExpandInfallible<O> for RepeatedTemplate<O>
where
    Template<O>: ExpandInfallible<O>,
    O: ExpansionOutput,
{
    fn expand(&self, ctx: &Context, out: &mut O) {
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
}

impl<O: ExpansionOutput> RepeatedTemplate<O> {
    /// private, does the condition
    fn expand_inner(&self, ctx: &Context, out: &mut O)
    where
        Template<O>: ExpandInfallible<O>,
        O: ExpansionOutput,
    {
        for when in &self.whens {
            match when.eval_bool(ctx) {
                Ok(true) => continue,
                Ok(false) => return,
                Err(e) => {
                    out.record_error(e);
                    return;
                }
            }
        }
        self.template.expand(ctx, out)
    }
}

/// `derive_adhoc_expand!` -- implements the actual template engine
///
/// In my design, the input contains, firstly, literally the definition
/// that #[derive(Adhoc)] was applied to (see NOTES.txt).
/// Using the literal input, rather than some pre-parsed version, is
/// slower, but means that we aren't inventing a nontrivial data format which
/// potentially crosses crate boundaries with semver implications.
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
    let mut output = TokenAccumulator::new();
    input.template.expand(&ctx, &mut output);
    let output = output.tokens()?;

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
