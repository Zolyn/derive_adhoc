//! Expansion of a template into output tokens, plus `derive_adhoc_expand!()`
//!
//! Contains the implementations of `fn expand()`
//! for the various template types in [`crate::syntax`].
//!
//! Also contains the top-level "do the work" macro function -
//! the implementation of `derive_adhoc_expand!()`.

use crate::framework::*;

/// Input to `derive_adhoc_expand!`
#[derive(Debug)]
pub struct DeriveAdhocExpandInput {
    pub driver_brace: token::Brace,
    pub driver: syn::DeriveInput,
    pub template_brace: token::Brace,
    pub template_crate: syn::Path,
    pub template_options: DaOptions,
    pub template: Template<TokenAccumulator>,
}

pub enum AttrValue<'l> {
    Unit(Span),
    Deeper(Span),
    Lit(&'l syn::Lit),
}

/// What would `${fname}` expand to?  As type from [`syn`].
///
/// Implements [`quote::IdentFragment`] and [`ToTokens`].
#[derive(Debug)]
pub enum Fname<'r> {
    Name(&'r syn::Ident),
    Index(syn::Index),
}

pub use AttrValue as AV;

impl Parse for DeriveAdhocExpandInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // This strange structure is to add a note to most of the errors that
        // come out of syn parsing.  The braced! etc. macros insist that the
        // calling scope throws syn::Error.
        // See the match at the bottom for what the return values mean.
        match (|| {
            let driver;
            let driver_brace = braced!(driver in input);
            let driver = driver.parse()?;

            let driver_passed;
            let _ = braced!(driver_passed in input);
            let _: TokenStream = driver_passed.parse()?;

            let template;
            let template_brace = braced!(template in input);
            let template = Template::parse(&template, ());

            let template_passed;
            let _ = braced!(template_passed in input);
            let template_crate = template_passed.parse()?;
            let _: Token![;] = template_passed.parse()?;

            let template_options = {
                let tokens;
                let _ = bracketed!(tokens in template_passed);
                DaOptions::parse(&tokens)?
            };

            let _: TokenStream = template_passed.parse()?;

            let _: TokenStream = input.parse()?;

            let template = match template {
                Ok(template) => template,
                Err(err_return_raw) => return Ok(Err(err_return_raw)),
            };

            Ok(Ok(DeriveAdhocExpandInput {
                driver_brace,
                driver,
                template_brace,
                template,
                template_crate,
                template_options,
            }))
        })() {
            Ok(Ok(dae_input)) => Ok(dae_input),
            Ok(Err(err_to_return_directly)) => Err(err_to_return_directly),
            Err(err_needing_advice) => {
                let mut advice = Span::call_site().error(
 "bad input to derive_adhoc_expand inner template expansion proc macro; might be due to incompatible derive-adhoc versions(s)"
                );
                advice.combine(err_needing_advice);
                Err(advice)
            }
        }
    }
}

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

impl SubstVType {
    fn expand(
        &self,
        ctx: &Context,
        out: &mut TokenAccumulator,
        kw_span: Span,
        self_def: SubstDetails<TokenAccumulator>,
    ) -> syn::Result<()> {
        let expand_spec_or_sd =
            |out: &mut _,
             spec: &Option<Template<TokenAccumulator>>,
             sd: SubstDetails<TokenAccumulator>| {
                if let Some(spec) = spec {
                    spec.expand(ctx, out);
                    Ok(())
                } else {
                    sd.expand(ctx, out, kw_span)
                }
            };

        if !ctx.is_enum() {
            return expand_spec_or_sd(out, &self.self_, self_def);
        }
        // It's an enum.  We need to write the main type name,
        // and the variant.  Naively we might expect to just do
        //    TTYPE::VNAME
        // but that doesn't work, because if TTYPE has generics, that's
        //    TNAME::<TGENERICS>::VNAME
        // and this triggers bizarre (buggy) behaviour in rustc -
        // see rust-lang/rust/issues/108224.
        // So we need to emit
        //    TNAME::VNAME::<TGENERICS>
        //
        // The most convenient way to do that seems to be to re-parse
        // this bit of the expansion as a syn::Path.  That lets
        // us fish out the generics, for writing out later.

        let mut self_ty = TokenAccumulator::new();
        expand_spec_or_sd(&mut self_ty, &self.self_, self_def)?;
        let self_ty = self_ty.tokens()?;
        let mut self_ty: syn::Path =
            syn::parse2(self_ty).map_err(|mut e| {
                e.combine(kw_span.error(
                    "error re-parsing self type path for this expansion",
                ));
                e
            })?;

        let mut generics = mem::take(
            &mut self_ty
                .segments
                .last_mut()
                .ok_or_else(|| {
                    kw_span.error(
                        "self type path for this expansion is empty path!",
                    )
                })?
                .arguments,
        );

        out.write_tokens(self_ty);
        out.write_tokens(Token![::](kw_span));
        expand_spec_or_sd(out, &self.vname, SD::vname(Default::default()))?;
        let gen_content = match &mut generics {
            syn::PathArguments::AngleBracketed(content) => Some(content),
            syn::PathArguments::None => None,
            syn::PathArguments::Parenthesized(..) => {
                return Err([
                    (generics.span(), "generics"),
                    (kw_span, "template keyword"),
                ]
                .error("self type has parenthesised generics, not supported"))
            }
        };
        if let Some(gen_content) = gen_content {
            // Normalise `<GENERICS>` to `::<TGENERICS>`.
            gen_content
                .colon2_token
                .get_or_insert_with(|| Token![::](kw_span));
            out.write_tokens(&generics);
        }
        Ok(())
    }
}

impl SubstVPat {
    // $vpat      for struct    $tname         { $( $fname: $fpatname, ) }
    // $vpat      for enum      $tname::$vname { $( $fname: $fpatname, ) }
    fn expand(
        &self,
        ctx: &Context,
        out: &mut TokenAccumulator,
        kw_span: Span,
    ) -> syn::Result<()> {
        let self_def = SD::tname(Default::default());
        SubstVType::expand(&self.vtype, ctx, out, kw_span, self_def)?;

        let in_braces = braced_group(kw_span, |mut out| {
            WithinField::for_each(ctx, |ctx, field| {
                SD::fname::<TokenAccumulator>(())
                    .expand(ctx, &mut out, kw_span)?;
                out.push_other_tokens(&(), Token![:](kw_span))?;

                // Do the expansion with the paste machinery, since
                // that has a ready-made notion of what fprefix= might
                // allow, and how to use it.
                let mut paste = paste::Items::new(kw_span);
                if let Some(fprefix) = &self.fprefix {
                    fprefix.expand(ctx, &mut paste);
                } else {
                    paste.push_fixed_string("f_".into());
                }
                paste.push_identfrag_toks(&field.fname(kw_span));

                out.push_other_subst(&(), |out| paste.assemble(out))?;
                out.write_tokens(Token![,](kw_span));

                Ok::<_, syn::Error>(())
            })
        })?;
        out.write_tokens(in_braces);
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
                not_in_paste: _,
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
        self.sd.expand(ctx, out, self.kw_span)
    }
}

impl<O> SubstDetails<O>
where
    O: ExpansionOutput,
    TemplateElement<O>: Expand<O>,
{
    fn expand(
        &self,
        ctx: &Context,
        out: &mut O,
        kw_span: Span,
    ) -> syn::Result<()> {
        // eprintln!("@@@@@@@@@@@@@@@@@@@@ EXPAND {:?}", self);

        let do_meta = |wa: &SubstAttr<_>, out, meta| wa.expand(ctx, out, meta);
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
        let do_tgens_nodefs = |out: &mut TokenAccumulator| {
            for pair in ctx.top.generics.params.pairs() {
                use syn::GenericParam as GP;
                let out_attrs = |out: &mut TokenAccumulator, attrs: &[_]| {
                    attrs.iter().for_each(|attr| out.write_tokens(attr));
                };
                match pair.value() {
                    GP::Type(t) => {
                        out_attrs(out, &t.attrs);
                        out.write_tokens(&t.ident);
                        out.write_tokens(&t.colon_token);
                        out.write_tokens(&t.bounds);
                    }
                    GP::Const(c) => {
                        out_attrs(out, &c.attrs);
                        out.write_tokens(&c.const_token);
                        out.write_tokens(&c.ident);
                        out.write_tokens(&c.colon_token);
                        out.write_tokens(&c.ty);
                    }
                    GP::Lifetime(l) => out.write_tokens(&l.lifetime),
                }
                out.with_tokens(|out| {
                    pair.punct().to_tokens_punct_composable(out);
                });
            }
        };
        let do_tgens = |out: &mut TokenAccumulator| {
            out.write_tokens(&ctx.top.generics.params);
        };
        // There are three contexts where the top-level type
        // name might occur with generics, and two syntaxes:
        //   referring to the type    $ttype     Type::<G>
        //   impl'ing for the type    $ttype     Type::<G>
        //   defining a new type      $ttypedef  Type<G: bounds>
        let do_ttype = |out: &mut O, colons: Option<()>, do_some_gens| {
            let _: &dyn Fn(&mut _) = do_some_gens; // specify type
            let colons = colons.map(|()| Token![::](kw_span));
            out.push_idpath(
                kw_span,
                |_| {},
                &ctx.top.ident,
                |out| {
                    out.write_tokens(colons);
                    out.write_tokens(Token![<](kw_span));
                    do_some_gens(out);
                    out.write_tokens(Token![>](kw_span));
                },
            )
        };
        let do_maybe_delimited_group = |out, np, delim, content| {
            let _: &mut O = out;
            let _: &Template<TokenAccumulator> = content;
            out.push_other_subst(np, |out| {
                if let Some(delim) = delim {
                    out.write_tokens(delimit_token_group(
                        delim,
                        kw_span,
                        |inside: &mut TokenAccumulator| {
                            Ok(content.expand(ctx, inside))
                        },
                    )?);
                } else {
                    content.expand(ctx, out);
                }
                Ok(())
            })
        };

        match self {
            SD::tname(_) => out.push_identfrag_toks(&ctx.top.ident),
            SD::ttype(_) => do_ttype(out, Some(()), &do_tgnames),
            SD::tdeftype(_) => do_ttype(out, None, &do_tgens),
            SD::vname(_) => {
                out.push_identfrag_toks(&ctx.syn_variant(&kw_span)?.ident)
            }
            SD::fname(_) => {
                let fname = ctx.field(&kw_span)?.fname(kw_span);
                out.push_identfrag_toks(&fname);
            }
            SD::ftype(_) => {
                let f = ctx.field(&kw_span)?;
                out.push_syn_type(kw_span, &f.field.ty);
            }
            SD::fpatname(_) => {
                let f = ctx.field(&kw_span)?;
                let fpatname =
                    format_ident!("f_{}", f.fname(kw_span), span = kw_span);
                out.push_identfrag_toks(&fpatname);
            }
            SD::tmeta(wa) => do_meta(wa, out, ctx.tattrs)?,
            SD::vmeta(wa) => do_meta(wa, out, ctx.variant(wa)?.pattrs)?,
            SD::fmeta(wa) => do_meta(wa, out, &ctx.field(wa)?.pfield.pattrs)?,

            SD::Vis(vis, np) => {
                out.push_other_tokens(np, vis.syn_vis(ctx, kw_span)?)?
            }
            SD::tdefkwd(_) => {
                fn w<O>(out: &mut O, t: impl ToTokens)
                where
                    O: ExpansionOutput,
                {
                    out.push_identfrag_toks(&TokenPastesAsIdent(t))
                }
                use syn::Data::*;
                match &ctx.top.data {
                    Struct(d) => w(out, &d.struct_token),
                    Enum(d) => w(out, &d.enum_token),
                    Union(d) => w(out, &d.union_token),
                };
            }

            SD::tattrs(ra, np, ..) => out.push_other_subst(np, |out| {
                ra.expand(ctx, out, &ctx.top.attrs)
            })?,
            SD::vattrs(ra, np, ..) => out.push_other_subst(np, |out| {
                let variant = ctx.variant(&kw_span)?.variant;
                let attrs = variant.as_ref().map(|v| &*v.attrs);
                ra.expand(ctx, out, attrs.unwrap_or_default())
            })?,
            SD::fattrs(ra, np, ..) => out.push_other_subst(np, |out| {
                ra.expand(ctx, out, &ctx.field(&kw_span)?.field.attrs)
            })?,

            SD::tgens(np, ..) => out.push_other_subst(np, |out| {
                do_tgens_nodefs(out);
                Ok(())
            })?,
            SD::tdefgens(np, ..) => out.push_other_subst(np, |out| {
                do_tgens(out);
                Ok(())
            })?,
            SD::tgnames(np, ..) => out.push_other_subst(np, |out| {
                do_tgnames(out);
                Ok(())
            })?,
            SD::twheres(np, ..) => out.push_other_subst(np, |out| {
                if let Some(clause) = &ctx.top.generics.where_clause {
                    out.with_tokens(|out| {
                        clause.predicates.to_tokens_punct_composable(out);
                    });
                }
                Ok(())
            })?,

            SD::vpat(v, np, ..) => out.push_other_subst(np, |out| {
                // This comment prevents rustfmt making this unlike the others
                v.expand(ctx, out, kw_span)
            })?,
            SD::vtype(v, np, ..) => out.push_other_subst(np, |out| {
                v.expand(ctx, out, kw_span, SD::ttype(Default::default()))
            })?,

            SD::tdefvariants(content, np, ..) => {
                let delim = if ctx.is_enum() {
                    Some(Delimiter::Brace)
                } else {
                    None
                };
                do_maybe_delimited_group(out, np, delim, content)?;
            }
            SD::fdefine(spec_f, np, ..) => {
                out.push_other_subst(np, |out| {
                    let field = ctx.field(&kw_span)?.field;
                    if let Some(driver_f) = &field.ident {
                        if let Some(spec_f) = spec_f {
                            spec_f.expand(ctx, out);
                        } else {
                            out.write_tokens(driver_f);
                        }
                    }
                    out.write_tokens(&field.colon_token);
                    Ok(())
                })?
            }
            SD::vdefbody(vname, content, np, ..) => {
                use syn::Fields as SF;
                let variant = ctx.variant(&kw_span)?;
                let enum_variant: Option<&syn::Variant> = variant.variant;
                if enum_variant.is_some() {
                    vname.expand(ctx, out);
                }
                let delim = match variant.fields {
                    SF::Unit => None,
                    SF::Unnamed(..) => Some(Delimiter::Parenthesis),
                    SF::Named(..) => Some(Delimiter::Brace),
                };
                do_maybe_delimited_group(out, np, delim, content)?;
                match variant.fields {
                    SF::Unit => Some(()),
                    SF::Unnamed(..) => Some(()),
                    SF::Named(..) => None,
                }
                .map(|()| {
                    if enum_variant.is_some() {
                        out.push_other_tokens(np, Token![,](kw_span))
                    } else {
                        out.push_other_tokens(np, Token![;](kw_span))
                    }
                })
                .transpose()?;
            }

            SD::paste(content, np, ..) => {
                out.expand_paste(np, ctx, kw_span, content)?
            }
            SD::ChangeCase(content, case, nc, ..) => {
                out.expand_case(nc, *case, ctx, kw_span, content)?
            }

            SD::when(..) => out.write_error(
                &kw_span,
                "${when } only allowed in toplevel of $( )",
            ),
            SD::If(conds, ..) => conds.expand(ctx, out)?,
            SD::is_struct(bo)
            | SD::is_enum(bo)
            | SD::is_union(bo)
            | SD::v_is_unit(bo)
            | SD::v_is_tuple(bo)
            | SD::v_is_named(bo)
            | SD::False(bo)
            | SD::True(bo)
            | SD::not(_, bo)
            | SD::any(_, bo)
            | SD::all(_, bo) => out.expand_bool_only(bo),
            SD::For(repeat, _) => repeat.expand(ctx, out),
            SD::select1(conds, ..) => conds.expand_select1(ctx, out)?,

            SD::Crate(np, ..) => {
                out.push_other_tokens(np, &ctx.template_crate)?
            }
        };
        Ok(())
    }
}

impl<O> SubstAttr<O>
where
    O: ExpansionOutput,
{
    fn expand(
        &self,
        ctx: &Context,
        out: &mut O,
        pattrs: &PreprocessedAttrs,
    ) -> syn::Result<()> {
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

        let as_ = self.as_.as_ref().map(|(as_, _nb)| as_);

        found.expand(self.span(), as_, out)?;

        Ok(())
    }
}

fn attrvalue_spans(tspan: Span, vspan: Span) -> [ErrorLoc; 2] {
    [(vspan, "attribute value"), (tspan, "template")]
}

/// Convert a literal found in a meta item into `T`
///
/// `into_what` is used only for error reporting
pub fn attrvalue_lit_as<T>(
    lit: &syn::Lit,
    tspan: Span,
    into_what: &dyn Display,
) -> syn::Result<T>
where
    T: Parse + ToTokens,
{
    let s: &syn::LitStr = match lit {
        syn::Lit::Str(s) => Ok(s),
        // having checked derive_builder, it doesn't handle
        // Lit::Verbatim so I guess we don't need to either.
        _ => Err(attrvalue_spans(tspan, lit.span()).error(format_args!(
            "expected string literal, for conversion to {}",
            into_what,
        ))),
    }?;

    let thing: T = s.parse()?;
    Ok(thing)
}

impl<'l> AttrValue<'l> {
    fn expand<O>(
        &self,
        tspan: Span,
        as_: Option<&SubstAttrAs>,
        out: &mut O,
    ) -> syn::Result<()>
    where
        O: ExpansionOutput,
    {
        let spans = |vspan| attrvalue_spans(tspan, vspan);

        let lit = match self {
            AttrValue::Unit(vspan) => return Err(spans(*vspan).error(
 "tried to expand attribute which is just a unit, not a literal"
            )),
            AttrValue::Deeper(vspan) => return Err(spans(*vspan).error(
 "tried to expand attribute which is nested list, not a value",
            )),
            AttrValue::Lit(lit) => lit,
        };

        use SubstAttrAs as SAS;
        match as_ {
            Some(SAS::lit) => out.push_syn_lit(lit),
            Some(as_ @ SAS::ty) => {
                out.push_syn_type(tspan, &attrvalue_lit_as(lit, tspan, as_)?)
            }
            None => out.push_attr_value(tspan, lit)?,
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
                RawAttr::Default => {
                    if ["adhoc", "derive_adhoc"]
                        .iter()
                        .all(|exclude| !attr.path.is_ident(exclude))
                    {
                        out.write_tokens(attr);
                    }
                }
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
        // for_with_within expects a fallible closure, but we want to do
        // infallible work in our infallible context, so we use `Void`
        // as the error type and wrap each call in `Ok`.
        #[allow(clippy::unit_arg)] // clippy wants us to worsify the style
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

impl<'w> WithinField<'w> {
    /// What would `${fname}` expand to?
    pub fn fname(&self, tspan: Span) -> Fname {
        if let Some(fname) = &self.field.ident {
            // todo is this the right span to emit?
            Fname::Name(fname)
        } else {
            Fname::Index(syn::Index {
                index: self.index,
                span: tspan,
            })
        }
    }
}

impl quote::IdentFragment for Fname<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Fname::Name(v) => quote::IdentFragment::fmt(v, f),
            Fname::Index(v) => quote::IdentFragment::fmt(v, f),
        }
    }
    fn span(&self) -> Option<Span> {
        Some(match self {
            Fname::Name(v) => (*v).span(),
            Fname::Index(v) => v.span,
        })
    }
}
impl ToTokens for Fname<'_> {
    fn to_tokens(&self, out: &mut TokenStream) {
        match self {
            Fname::Name(v) => v.to_tokens(out),
            Fname::Index(v) => v.to_tokens(out),
        }
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
    // eprintln!("derive_adhoc_expand! {}", &input);
    let input: DeriveAdhocExpandInput = syn::parse2(input)?;
    // eprintln!("derive_adhoc_expand! crate = {:?}", &input.template_crate);

    let DaOptions {
        dbg,
        driver_kind,
    } = input.template_options;

    if let Some((exp_kind, exp_span)) = driver_kind {
        macro_rules! got_kind { { $($kind:ident)* } => {
            match &input.driver.data {
                $(
                    syn::Data::$kind(..) => ExpectedDriverKind::$kind,
                )*
            }
        } }

        let got_kind = got_kind!(Struct Enum Union);
        if got_kind != exp_kind {
            return Err([
                (exp_span, "expected kind"),
                (input.driver.span(), "actual kind"),
            ]
            .error(format_args!(
                "expected driver kind {}, but driver is {}",
                exp_kind, got_kind,
            )));
        }
    }

    let output = Context::call(&input.driver, &input.template_crate, |ctx| {
        let mut output = TokenAccumulator::new();
        input.template.expand(&ctx, &mut output);
        output.tokens()
    })?;

    //    dbg!(&&output);
    if dbg {
        let ident = input.driver.ident;
        // TODO print the template name too
        // TODO improve the output to not mention derive_adhoc_expand
        eprintln!(
            "---------- derive_adhoc_expand start for {} ----------",
            ident
        );
        eprintln!("{}", &output);
        eprintln!(
            "---------- derive_adhoc_expand end for {} ----------",
            ident
        );
    }

    Ok(output)
}
