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
    ///
    /// `Err` means we expected an error containing that string
    output: Result<TokenStream, String>,
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
    info: Option<DissimilarTokenStreams>,
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
    fn print_checking(&self) {
        println!(
            "checking :{} {} => {}",
            self.loc,
            &self.input,
            self.output_for_messages()
        );
    }

    fn check(&self, errs: &mut Errors, drivers: &[syn::DeriveInput]) {
        let mut tracker = Tracker {
            all_must_match: self.all_must_match,
            matching_outputs: 0,
            other_outputs: vec![],
            skipped_context_descs: vec![],
        };
        //println!("  LIMIT {:?}", &self.limit);

        for_every_example_context(drivers, |ctx| {
            self.compare_one_output(&mut tracker, &ctx);
            Ok::<_, Void>(())
        })
        .void_unwrap();

        match tracker.finish_ok() {
            Ok(()) => {}
            Err(m) => {
                eprintln!();
                eprintln!("========================================");
                errs.wrong(self.loc, "example mismatch");
                eprintln!(
                    r"{}
input: {}
limit: {:?}
documented: {}",
                    m,
                    self.input,
                    self.limit,
                    self.output_for_messages(),
                );
                for got in &tracker.other_outputs {
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
                eprintln!("");
                for got in &tracker.other_outputs {
                    let Some(info) = &got.info else { continue; };
                    info.eprintln(format!("[{}]", got.context_desc));
                }
                eprintln!("========================================");
            }
        }
    }
}

impl PossibilitiesExample {
    pub fn new(
        loc: DocLoc,
        input: &str,
        limit: Limit,
        all_must_match: bool,
        output: Result<&str, &str>,
    ) -> Result<Box<PossibilitiesExample>, String> {
        let parse = |s, what| {
            syn::parse_str(s).map_err(|e| {
                format!(r#"failed to parse {}: {:?}: {}"#, what, s, e)
            })
        };
        let input = parse(input, "input")?;
        let output = match output {
            Ok(exp) => Ok(parse(exp, "output")?),
            Err(msg) => Err(msg.to_string()),
        };
        Ok(Box::new(PossibilitiesExample {
            loc,
            input,
            limit,
            all_must_match,
            output,
        }))
    }

    fn output_for_messages(&self) -> impl Display {
        match &self.output {
            Ok(y) => y.to_string(),
            Err(e) => format!("error: {}", e),
        }
    }

    fn compare_one_output(&self, tracker: &mut Tracker, ctx: &Context<'_>) {
        if !tracker.should_continue() {
            return;
        };
        let limit = &self.limit;

        let context_desc = ctx.desc_for_tests();

        if !limit
            .matches(ctx)
            // Treat errors "skip".
            .unwrap_or_default()
        {
            //println!("  INAPPLICABLE {:?}", &context_desc);
            tracker.note_skip(context_desc);
            return;
        }

        let input = &self.input;

        let matched = (|| {
            let mut out = TokenAccumulator::new();

            let mk_mismatch = |info, got: &dyn Display| Mismatch {
                info,
                got: got.to_string(),
                context_desc: context_desc.clone(),
            };

            let handle_syn_error = |e: syn::Error| {
                //println!("  ERROR {}", &context_desc);
                mk_mismatch(None, &format_args!("error: {}", e))
            };

            let got = (|| {
                let template: Template<TokenAccumulator> =
                    syn::parse2(input.clone())?;

                template.expand(ctx, &mut out);
                let got = out.tokens()?;

                Ok(got)
            })();

            match &self.output {
                Ok(exp) => {
                    let got = got.map_err(handle_syn_error)?;
                    check_expected_actual_similar_tokens(
                        &exp, &got, //
                    )
                    .map_err(|info| {
                        //println!("  MISMATCH {}", &context_desc);
                        mk_mismatch(Some(info), &got)
                    })?;
                    //println!("  MATCHED {}", &context_desc);
                }
                Err(exp) => {
                    let got = match got {
                        Err(n) => Ok(n),
                        Ok(y) => Err(y),
                    };
                    let got = got
                        .map_err(|got| {
                            //println!("  UNEXPECTED-SUCCESS {}", &context_desc);
                            mk_mismatch(None, &got)
                        })?
                        .to_string();
                    if !got.contains(exp) {
                        //println!("  WRONG-ERROR {}", &context_desc);
                        return Err(mk_mismatch(None, &got));
                    }
                }
            }
            Ok(())
        })();

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
        output: Ok(output),
    }
    .check(&mut Errors::new(), &[driver]);
}
