//! Contexts (drivers and parts) and contextual limits

use super::*;

pub fn for_every_example_context<E>(
    drivers: &[syn::DeriveInput],
    mut call: impl FnMut(&Context<'_>) -> Result<(), E>,
) -> Result<(), E> {
    for driver in drivers {
        Context::call(driver, &parse_quote!(crate), None, |ctx| {
            Ok::<_, syn::Error>((|| {
                call(&ctx)?;
                ctx.for_with_within::<WithinVariant, _, _>(|ctx, _| {
                    call(ctx)?;
                    ctx.for_with_within::<WithinField, _, _>(|ctx, _| {
                        call(ctx)
                    })
                })
            })())
        })
        .unwrap()?;
    }
    Ok(())
}

/// Limit on the contexts (in the example structs) that this test is for
///
/// This was (at an early stage) done with derive-adhoc conditions.
/// However, derive-adhoc conditions throw an error when applied
/// in contexts that lack all the information
/// (for example, `$fname` outside a field).
/// Also, they make it difficult to analyse what `Others` means.
#[derive(Educe, Clone)]
#[educe(Debug)]
pub enum Limit {
    True,
    DaCond(Rc<Subst<BooleanContext>>),
    Name(String),
    Field { f: String, n: String },
    Others(#[educe(Debug(ignore))] Vec<Limit>),
}

#[ext(ContextExt)]
pub impl Context<'_> {
    fn vname_s(&self) -> Option<String> {
        Some(self.variant?.variant?.ident.to_string())
    }
    fn fname_s(&self) -> Option<String> {
        let span = Span::call_site();
        Some(self.field?.fname(span).to_token_stream().to_string())
    }
    fn desc_for_tests(&self) -> String {
        let mut out = format!("{}", self.top.ident);
        if let Some(vname) = self.vname_s() {
            write!(out, "::{}", vname).unwrap();
        }
        if let Some(fname) = self.fname_s() {
            write!(out, ".{}", fname).unwrap();
        }
        out
    }
}

impl Limit {
    // Some tests aren't meaningful in some context.
    // Eg, we have "for" clauses that check the variant
    // kind (unit, tuple, named fields).  That isn't
    // meaningful if we're in an enum and not in a variant.
    // d-a gives an error there.  That makes this return `Err`.
    pub fn matches(&self, ctx: &Context<'_>) -> Result<bool, ()> {
        let tname = |n| &ctx.top.ident.to_string() == n;
        let vname = |n| ctx.vname_s().map(|s| &s == n) == Some(true);
        let fname = |n| ctx.fname_s().map(|s| &s == n) == Some(true);
        Ok(match self {
            Limit::True => true,
            Limit::DaCond(cond) => cond.eval_bool(ctx).map_err(|_| ())?,
            Limit::Name(n) => tname(n) || vname(n) || fname(n),
            Limit::Field { f, n } => fname(f) && (tname(n) || vname(n)),
            Limit::Others(v) => {
                // If we've had any `for field in Type` annotations,
                // we should skip any contexts that don't have a field.
                // Since, in that case, "other" means *other fields*
                if v.iter().any(|l| matches!(l, Limit::Field { .. })) {
                    if ctx.fname_s().is_none() {
                        return Ok(false);
                    }
                }
                !v.iter().any(|l| {
                    l.matches(ctx)
                        // Treat errors in the others as "no match"
                        // so if they might match "others"
                        .unwrap_or_default()
                })
            }
        })
    }
}

type LimitViaDaCond = dyn Fn(()) -> SubstDetails<BooleanContext>;

/// Table mapping limit regexp to derive-adhoc `SubstDetails`
// Breaking this out here allows us to format it nicely
#[rustfmt::skip]
const LIMIT_DA_COND_REGEXPS: &[(&str, &'static LimitViaDaCond)] = &[
    (r"^structs?$",                         &SD::is_struct  as _  ),
    (r"^enum( variant)?s?$",                &SD::is_enum    as _  ),
    (r"^braced (?:(?:struct|variant)s?)?$", &SD::v_is_named as _  ),
    (r"^tuple( variant)?s?$",               &SD::v_is_tuple as _  ),
    (r"^unit( variant)?s?$",                &SD::v_is_unit  as _  ),
];

impl Limit {
    pub fn parse(
        for_: &str,
        all_must_match: &mut bool,
        others: Option<&mut Vec<Limit>>,
    ) -> Result<Limit, String> {
        use Limit as L;

        let da_cond = |mk_sd: &dyn Fn(_) -> _| {
            L::DaCond(Rc::new(Subst {
                kw_span: Span::call_site(),
                sd: mk_sd(()),
                output_marker: PhantomData,
            }))
        };

        let limit = if let Some(mk_sd) = LIMIT_DA_COND_REGEXPS
            .iter()
            .cloned()
            .find_map(|(re, mk_sd)| m!(for_, re).then(|| mk_sd))
        {
            da_cond(mk_sd)
        } else if let Some((n,)) = mc!(for_, r"^`(\w+)`$") {
            L::Name(n.into())
        } else if let Some((f, n)) = mc!(for_, r"^`(\w+)` in `(\w+)`$") {
            *all_must_match = true;
            L::Field { f, n }
        } else if m!(for_, "^others$") {
            *all_must_match = true;
            let others = others
                .ok_or_else(|| format!(r#""for others" not allowed here"#))?;
            return Ok(L::Others(mem::take(others)));
        } else {
            return Err(format!(r#"unhandled for clause "{}""#, for_));
        };
        if let Some(others) = others {
            others.push(limit.clone());
        }
        Ok(limit)
    }
}
