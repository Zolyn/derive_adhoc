//! Examples involving searching over possible outputs for a particular input
//!
//! Where the input file doesn't specify
//! which driver (or part of driver) generates the output.
//! (Or maybe where it limits it according to `limit`).

use super::*;

pub struct PossibilitiesExample {
    loc: DocLoc,
    /// A derive-adhoc template fragment
    input: TokenStream,
    /// Limit on which contexts to consider
    limit: Limit,
    /// Expected output
    output: TokenStream,
    /// Are *all* the possibilities (subject to `limit`) supposed to match?
    all_must_match: bool,
}

struct Tracker {
    all_must_match: bool,
    matching_outputs: usize,
    other_outputs: Vec<Mismatch>,
    skipped_context_descs: Vec<String>,
}

struct Mismatch {
    got: String,
    context_desc: String,
    #[allow(dead_code)] // TODO EXTEST
    info: Option<DissimilarTokenStreams>,
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

#[ext]
impl Context<'_> {
    fn vname_s(&self) -> Option<String> {
        Some(self.variant?.variant?.ident.to_string())
    }
    fn fname_s(&self) -> Option<String> {
        let span = Span::call_site();
        Some(self.field?.fname(span).to_token_stream().to_string())
    }
}

impl Limit {
    fn matches(&self, ctx: &Context<'_>) -> bool {
        let tname = |n| &ctx.top.ident.to_string() == n;
        let vname = |n| ctx.vname_s().map(|s| &s == n) == Some(true);
        let fname = |n| ctx.fname_s().map(|s| &s == n) == Some(true);
        match self {
            Limit::True => true,
            Limit::DaCond(cond) => {
                cond.eval_bool(ctx)
                    // Some tests aren't meaningful in some context.
                    // Eg, we have "for" clauses that check the variant
                    // kind (unit, tuple, named fields).  That isn't
                    // meaningful if we're in an enum and not in a variant.
                    // d-a gives an error there.  We treat it as "skip".
                    .unwrap_or_default()
            }
            Limit::Name(n) => tname(n) || vname(n) || fname(n),
            Limit::Field { f, n } => fname(f) && (tname(n) || vname(n)),
            Limit::Others(v) => {
                // If we've had any `for field in Type` annotations,
                // we should skip any contexts that don't have a field.
                // Since, in that case, "other" means *other fields*
                if v.iter().any(|l| matches!(l, Limit::Field { .. })) {
                    if ctx.fname_s().is_none() {
                        return false;
                    }
                }
                !v.iter().any(|l| l.matches(ctx))
            }
        }
    }
}

impl Tracker {
    fn finish_ok(&self) -> Result<(), String> {
        if self.all_must_match {
            if !self.other_outputs.is_empty() {
                return Err(
 "at least one actual output doesn't match the documented output".into()
                );
            }
        }
        if self.matching_outputs == 0 {
            return Err(
                "documented output does not match any of the actual outputs"
                    .into(),
            );
        }
        Ok(())
    }

    fn should_continue(&self) -> bool {
        if !self.all_must_match && self.matching_outputs != 0 {
            return false;
        }
        true
    }

    fn note(&mut self, matched: Result<(), Mismatch>) {
        match matched {
            Ok(()) => self.matching_outputs += 1,
            Err(got) => self.other_outputs.push(got),
        }
    }

    fn note_skip(&mut self, context_desc: String) {
        self.skipped_context_descs.push(context_desc);
    }
}

impl Example for PossibilitiesExample {
    fn check(&self, errs: &mut Errors, drivers: &[syn::DeriveInput]) {
        let mut tracker = Tracker {
            all_must_match: self.all_must_match,
            matching_outputs: 0,
            other_outputs: vec![],
            skipped_context_descs: vec![],
        };
        println!("checking :{} {} => {}", self.loc, &self.input, &self.output);
        //println!("  LIMIT {:?}", &self.limit);

        for driver in drivers {
            Context::call(driver, &parse_quote!(crate), None, |ctx| {
                Ok(self.search_one_driver(&mut tracker, &ctx))
            })
            .unwrap();
        }

        match tracker.finish_ok() {
            Ok(()) => {}
            Err(m) => {
                eprintln!();
                errs.wrong(self.loc, "example mismatch");
                eprintln!(
                    r"{}
input: {}
limit: {:?}
documented: {}",
                    m, self.input, self.limit, self.output
                );
                for got in tracker.other_outputs {
                    eprintln!(
                        "mismatched: {} [{}]",
                        got.got, got.context_desc
                    );
                }
                eprintln!("matched: {}", tracker.matching_outputs);
                eprint!("skipped:");
                for skip in tracker.skipped_context_descs {
                    eprint!(" [{}]", skip);
                }
                eprintln!("\n");
            }
        }
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

impl PossibilitiesExample {
    pub fn new(
        loc: DocLoc,
        input: &str,
        limit: Limit,
        all_must_match: bool,
        output: &str,
    ) -> Result<Box<PossibilitiesExample>, String> {
        let parse = |s, what| {
            syn::parse_str(s).map_err(|e| {
                format!(r#"failed to parse {}: {:?}: {}"#, what, s, e)
            })
        };
        let input = parse(input, "input")?;
        let output = parse(output, "output")?;
        Ok(Box::new(PossibilitiesExample {
            loc,
            input,
            limit,
            all_must_match,
            output,
        }))
    }

    fn search_one_driver(&self, tracker: &mut Tracker, ctx: &Context<'_>) {
        self.compare_one_output(tracker, ctx);
        ctx.for_with_within::<WithinVariant, _, _>(|ctx, _| {
            self.compare_one_output(tracker, ctx);
            ctx.for_with_within::<WithinField, _, _>(|ctx, _| {
                self.compare_one_output(tracker, ctx);
                Ok::<_, Void>(())
            })
            .unwrap();
            Ok::<_, Void>(())
        })
        .void_unwrap()
    }

    fn compare_one_output(&self, tracker: &mut Tracker, ctx: &Context<'_>) {
        if !tracker.should_continue() {
            return;
        };
        let limit = &self.limit;

        let context_desc = {
            let mut out = format!("{}", ctx.top.ident);
            if let Some(vname) = ctx.vname_s() {
                write!(out, "::{}", vname).unwrap();
            }
            if let Some(fname) = ctx.fname_s() {
                write!(out, ".{}", fname).unwrap();
            }
            out
        };

        if !limit.matches(ctx) {
            //println!("  INAPPLICABLE {:?}", &context_desc);
            tracker.note_skip(context_desc);
            return;
        }

        let input = &self.input;

        let out = (|| {
            let mut out = TokenAccumulator::new();
            let template: Template<TokenAccumulator> =
                syn::parse2(input.clone())?;
            template.expand(ctx, &mut out);
            out.tokens()
        })();

        let matched = match out {
            Ok(got) => {
                match check_expected_actual_similar_tokens(
                    &self.output, &got
                ) {
                    Ok(y@ ()) => {
                        //println!("  MATCHED {}", &context_desc);
                        Ok(y)
                    },
                    Err(info) => {
                        //println!("  MISMATCH {}", &context_desc);
                        Err(Mismatch {
                            got: got.to_string(),
                            info: Some(info),
                            context_desc,
                        })
                    }
                }
            }
            Err(e) => {
                //println!("  ERROR {}", &context_desc);
                Err(Mismatch {
                    got: format!("error: {}", e),
                    info: None,
                    context_desc,
                })
            }
        };
        tracker.note(matched);
    }
}

#[test]
fn poc() {
    let driver: syn::DeriveInput = parse_quote! {
        pub(crate) enum Enum<'a, 'l: 'a, T: Display = usize,
                             const C: usize = 1>
        where T: 'l, T: TryInto<u8>
        {
            UnitVariant,
            TupleVariant(std::iter::Once::<T>),
            NamedVariant {
                field: &'l &'a T,
                field_b: String,
                field_e: <T as TryInto<u8>>::Error,
             },
        }
    };
    let input = quote! { $($vname,) };
    let limit = Limit::Name("Enum".into());
    let output = quote! { UnitVariant, TupleVariant, NamedVariant, };
    PossibilitiesExample {
        all_must_match: false,
        loc: 42,
        input: input,
        limit: limit,
        output: output,
    }
    .check(&mut Errors::new(), &[driver]);
}
