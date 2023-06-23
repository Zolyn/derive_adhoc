//! Examples involving searching over possible outputs for a particular input
//!
//! Where the input file doesn't specify
//! which driver (or part of driver) generates the output.
//! (Or maybe where it limits it according to `limit`).

use super::*;

pub struct PossibilitiesExample {
    pub loc: DocLoc,
    /// A derive-adhoc template fragment
    pub input: TokenStream,
    /// Limit on which contexts to consider
    pub limit: Limit,
    /// Expected output
    pub output: TokenStream,
    /// Are *all* the possibilities (subject to `limit`) supposed to match?
    pub all_must_match: bool,
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
}

#[derive(Educe, Clone)]
#[educe(Debug)]
pub enum Limit {
    True,
    IsStruct,
    IsEnum,
    Name(String),
    Field {
        f: String,
        n: String,
    },
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
            Limit::IsStruct => matches!(ctx.top.data, syn::Data::Struct(_)),
            Limit::IsEnum => matches!(ctx.top.data, syn::Data::Enum(_)),
            Limit::Name(n) => tname(n) || vname(n) || fname(n),
            Limit::Field { f, n} => fname(f) && (tname(n) || vname(n)),
            Limit::Others(v) => {
                if v.iter().any(|l| matches!(l, Limit::Field { .. })) {
                    if ctx.fname_s().is_none() {
                        return false;
                    }
                }
                !v.iter().any(|l| l.matches(ctx))
            },
        }
    }
}

impl Tracker {
    fn finish_ok(&self) -> Result<(), String> {
        if self.all_must_match {
            if !self.other_outputs.is_empty() {
                return Err(
 "at least one actual output doesn't match the documented output".into()
                )
            }
        }
        if self.matching_outputs == 0 {
            return Err(
 "documented output does not match any of the actual outputs".into()
            )
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
    fn check(
        &self,
        errs: &mut Errors,
        drivers: &[syn::DeriveInput],
    ) {
        let mut tracker = Tracker {
            all_must_match: self.all_must_match,
            matching_outputs: 0,
            other_outputs: vec![],
            skipped_context_descs: vec![],
        };
        println!("checking :{} {} => {}", self.loc, &self.input, &self.output);
        //println!("  LIMIT {:?}", &self.limit);

        for driver in drivers {
            Context::call(
                driver,
                &parse_quote!(crate),
                None,
                |ctx| Ok(self.search_one_driver(&mut tracker, &ctx))
            ).unwrap();
        }

	match tracker.finish_ok() {
            Ok(()) => {},
            Err(m) => {
                eprintln!();
                errs.wrong(self.loc, "example mismatch");
                eprintln!(r"{}
input: {}
limit: {:?}
documented: {}", m, self.input, self.limit, self.output);
                for got in tracker.other_outputs {
                    eprintln!("mismatched: {} [{}]",
                              got.got, got.context_desc);
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

impl PossibilitiesExample {
    fn search_one_driver(
        &self,
        tracker: &mut Tracker,
        ctx: &Context<'_>,
    ) {
        self.compare_one_output(tracker, ctx);
        ctx.for_with_within::<WithinVariant, _, _>(|ctx, _| {
            self.compare_one_output(tracker, ctx);
            ctx.for_with_within::<WithinField, _, _>(|ctx, _| {
                self.compare_one_output(tracker, ctx);
                Ok::<_, Void>(())
            }).unwrap();
            Ok::<_, Void>(())
        }).void_unwrap()
    }

    fn compare_one_output(
        &self,
        tracker: &mut Tracker,
        ctx: &Context<'_>,
    ) {
        if !tracker.should_continue() {
            return
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
            return
        }

        let input = &self.input;

        let out = (|| {
            let mut out = TokenAccumulator::new();
            let template: Template<TokenAccumulator> = syn::parse2(input.clone())?;
            template.expand(ctx, &mut out);
            out.tokens().map(|out| out.to_string())
        })();

        let matched = match out {
            Ok(got) if got == self.output.to_string() => {
                //println!("  MATCHED {}", &context_desc);
                Ok(())
            },
            Err(e) => {
                //println!("  ERROR {}", &context_desc);
                Err(format!("error: {}", e))
            },
            Ok(s) => {
                //println!("  MISMATCH {}", &context_desc);
                Err(s.to_string())
            },
        };
        let matched = matched.map_err(|got| Mismatch {
            got,
            context_desc,
        });
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
    }.check(&mut Errors::new(), &[driver]);
}

