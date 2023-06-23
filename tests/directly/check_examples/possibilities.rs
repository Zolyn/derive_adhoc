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
    /// A derive-adhoc condition
    pub limit: TokenStream,
    /// Expected output
    pub output: TokenStream,
    /// Are *all* the possibilities (subject to `limit`) supposed to match?
    pub all_must_match: bool,
}

struct Tracker {
    all_must_match: bool,
    matching_outputs: usize,
    other_outputs: Vec<Mismatch>,
}

struct Mismatch {
    got: String,
    context_desc: String,
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
        };
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
                errs.wrong(self.loc, "example mismatch");
                eprintln!(r"{}
input: {}
limit: {}
documented: {}", m, self.input, self.limit, self.output);
                for got in tracker.other_outputs {
                    eprintln!("mismatched: {} [{}]",
                              got.got, got.context_desc);
                }
                eprintln!("matched: {}", tracker.matching_outputs);
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
        let input = &self.input;
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
        println!("CHECKING :{} {} => {}", self.loc, &input, &self.output);

        if self.all_must_match {
            let template: Template<TokenAccumulator> =
                parse_quote!( ${if #limit {} else {}} );
            let mut out = TokenAccumulator::new();
            template.expand(ctx, &mut out);
            match out.tokens() {
                Ok(_) => {},
                Err(e) => {
                    assert!(e.to_string().contains("must be within"), "{:?}", e);
                    return;
                }
            }
        }

        let template = quote!(
            ${if #limit {
                #input
            } else {
                inapplicable: #limit
            }}
        );
        let out = (|| {
            let mut out = TokenAccumulator::new();
            let template: Template<TokenAccumulator> = syn::parse2(template)?;
            template.expand(ctx, &mut out);
            out.tokens().map(|out| out.to_string())
        })();

        let matched = match out {
            Ok(s) if s == self.output.to_string() => Ok(()),
            Err(e) => Err(format!("error: {}", e)),
            Ok(s) => Err(s),
        };
        let matched = matched.map_err(|got| Mismatch {
            got,
            context_desc,
        });
        tracker.note(matched);
    }
}

#[test]
#[should_panic] // XXXX panics due to lack of `equal` in this d-a branch
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
    let limit = quote! { any(equal($tname,Enum),equal($vname,Enum)) };
    let output = quote! { UnitVariant, TupleVariant, NamedVariant, };
    PossibilitiesExample {
        all_must_match: false,
        loc: 42,
        input: input,
        limit: limit,
        output: output,
    }.check(&mut Errors::new(), &[driver]);
}

