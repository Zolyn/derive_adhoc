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
}

enum Tracker {
    Found,
    NotFound {
        bad_outputs: Vec<String>
    },
}

impl Example for PossibilitiesExample {
    fn check(
        &self,
        errs: &mut Errors,
        drivers: &[syn::DeriveInput],
    ) {
        let mut tracker = Tracker::NotFound {
            bad_outputs: vec![],
        };
        for driver in drivers {
            Context::call(
                driver,
                &parse_quote!(crate),
                None,
                |ctx| Ok(self.search_one_driver(&mut tracker, &ctx))
            ).unwrap();
        }

        match tracker {
            Tracker::Found => {}
            Tracker::NotFound { bad_outputs } => {
                eprintln!(r"documented output does not match any of the actual outputs!
input: {}
limit: {}
documented: {}", self.input, self.limit, self.output);
                for got in bad_outputs {
                    eprintln!("    actual: {}", got);
                }
                errs.wrong(self.loc, "example mismatch");
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
        let bad_outputs = match tracker {
            Tracker::Found => return,
            Tracker::NotFound { bad_outputs } => bad_outputs,
        };

        let limit = &self.limit;
        let input = &self.input;
        let template: Template<TokenAccumulator> = parse_quote!(
            ${if #limit {
                #input
            } else {
                inapplicable: #limit
            }}
        );
        let out = {
            let mut out = TokenAccumulator::new();
            template.expand(ctx, &mut out);
            out.tokens().map(|out| out.to_string())
        };

        let bad = match out {
            Ok(s) if s == self.output.to_string() => {
                *tracker = Tracker::Found;
                return;
            }
            Err(e) => format!("error: {}", e),
            Ok(s) => s,
        };
        bad_outputs.push(bad);
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
        loc: 42,
        input: input,
        limit: limit,
        output,
    }.check(&mut Errors::new(), &[driver]);
}

