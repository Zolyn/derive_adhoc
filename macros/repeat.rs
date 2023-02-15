//! Handling of repetition

use crate::framework::*;

pub use RepeatOver as RO;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Display)]
#[strum(serialize_all = "snake_case")]
pub enum RepeatOver {
    Variants,
    Fields,
}

#[derive(Debug, Clone)]
struct RepeatOverInference {
    over: RepeatOver,
    span: Span,
}

#[derive(Default, Debug, Clone)]
pub struct RepeatAnalysisVisitor {
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

    pub fn finish(self, start: Span) -> Result<RepeatOver, syn::Error> {
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

impl<O: SubstParseContext> SubstIf<O> {
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

impl<O: SubstParseContext> Template<O> {
    /// Analyses a template section to be repeated
    pub fn analyse_repeat(
        &self,
        visitor: &mut RepeatAnalysisVisitor,
    ) -> syn::Result<()> {
        for element in &self.elements {
            element.analyse_repeat(visitor)?;
        }
        Ok(())
    }
}

impl<O: SubstParseContext> TemplateElement<O> {
    fn analyse_repeat(
        &self,
        visitor: &mut RepeatAnalysisVisitor,
    ) -> syn::Result<()> {
        match self {
            TE::Ident(_) => {}
            TE::Literal(_) => {}
            TE::Punct(..) => {}
            TE::Repeat(_) => {}
            TE::Group { template, .. } => template.analyse_repeat(visitor)?,
            TE::Subst(exp) => exp.analyse_repeat(visitor)?,
        }
        Ok(())
    }
}

impl<O: SubstParseContext> Subst<O> {
    fn analyse_repeat(
        &self,
        visitor: &mut RepeatAnalysisVisitor,
    ) -> syn::Result<()> {
        let over = match &self.sd {
            SD::tname(..) => None,
            SD::vname(..) => Some(RO::Variants),
            SD::fname(..) => Some(RO::Fields),
            SD::ttype(..) => None,
            SD::ttypedef(..) => None,
            // TODO vtype
            SD::ftype(..) => Some(RO::Fields),
            SD::fpatname(_) => Some(RO::Fields),
            SD::Vis(SubstVis::T, ..) => None,
            SD::Vis(SubstVis::F, ..) => Some(RO::Fields),
            SD::tmeta(_) => None,
            SD::vmeta(_) => Some(RO::Variants),
            SD::fmeta(_) => Some(RO::Fields),
            SD::tattrs(..) => None,
            SD::vattrs(..) => Some(RO::Variants),
            SD::fattrs(..) => Some(RO::Fields),
            SD::tgens(..) => None,
            SD::tgnames(..) => None,
            SD::twheres(..) => None,
            SD::vpat(..) => Some(RO::Variants),
            SD::vtype(..) => Some(RO::Variants),
            SD::is_enum(..) => None,
            SD::paste(body, ..) => {
                body.analyse_repeat(visitor)?;
                None
            }
            SD::ChangeCase(body, ..) => {
                body.analyse_repeat(visitor)?;
                None
            }
            SD::when(..) => None, // out-of-place when, ignore it
            SD::not(cond, _) => {
                cond.analyse_repeat(visitor)?;
                None
            }
            SD::If(conds, ..) | SD::select1(conds, ..) => {
                conds.analyse_repeat(visitor)?;
                None
            }
            SD::any(conds, _) | SD::all(conds, _) => {
                for c in conds.iter() {
                    c.analyse_repeat(visitor)?;
                }
                None
            }
            // Has a RepeatOver, but does not imply anything about its context.
            SD::For(..) => None,
            SD::False(..) | SD::True(..) => None, // condition: ignore.
            SD::Crate(..) => None,
        };
        if let Some(over) = over {
            let over = RepeatOverInference {
                over,
                span: self.kw_span,
            };
            visitor.set_over(over)?;
        }
        Ok(())
    }
}

/// Implemented for [`WithinVariant`] and [`WithinField`]
///
/// For combining code that applies similarly for different repeat levels.
pub trait WithinRepeatLevel<'w>: 'w {
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
    /// Returns an [`ErrorLoc`] for the current part of the driver
    pub fn error_loc(&self) -> (Span, &'static str) {
        if let Some(field) = &self.field {
            (field.field.span(), "in this field")
        } else if let Some(variant) =
            &self.variant.and_then(|variant| variant.variant.as_ref())
        {
            (variant.span(), "in this variant")
        } else {
            (self.top.span(), "in this data structure")
        }
    }

    /// Obtains the relevant `Within`(s), and calls `call` for each one
    ///
    /// If there is a current `W`, simply calls `call`.
    /// Otherwise, iterates over all of them and calls `call` for each one.
    pub fn for_with_within<'w, W, F, E>(&'c self, mut call: F) -> Result<(), E>
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
                format!(
                    "must be within a {} (so, in a repeat group)",
                    W::level_display_name(),
                ),
            )
        })?;
        Ok(r)
    }

    /// Obtains the current field (or calls it an error)
    pub fn field(&self, why: &dyn Spanned) -> syn::Result<&WithinField> {
        self.within_level(why)
    }
    /// Obtains the current variant (or calls it an error)
    pub fn variant(&self, why: &dyn Spanned) -> syn::Result<&WithinVariant> {
        self.within_level(why)
    }
    /// Obtains the current variant as a `syn::Variant`
    pub fn syn_variant(
        &self,
        why: &dyn Spanned,
    ) -> syn::Result<&syn::Variant> {
        let r = self.variant(why)?.variant.as_ref().ok_or_else(|| {
            syn::Error::new(why.span(), "expansion only valid in enums")
        })?;
        Ok(r)
    }
}
