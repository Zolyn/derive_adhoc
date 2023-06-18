//! Check the examples in the reference manual

#![allow(dead_code, unused_imports, unused_variables)] // XXXX

use super::*;

mod reference_extract;

struct ExtractedExample<'i> {
    drivers: &'i [syn::DeriveInput],
    input: &'i TokenStream,
    limit: &'i TokenStream,
    output: &'i str,
}

enum Tracker {
    Found,
    NotFound {
        bad_outputs: Vec<String>
    },
}

impl ExtractedExample<'_> {
    fn check(
        &self,
    ) -> Result<(), Vec<String>> {
        let mut tracker = Tracker::NotFound {
            bad_outputs: vec![],
        };
        for driver in self.drivers {
            Context::call(
                driver,
                &parse_quote!(crate),
                None,
                |ctx| Ok(self.search_one_driver(&mut tracker, &ctx))
            ).unwrap();
        }

        match tracker {
            Tracker::Found => Ok(()),
            Tracker::NotFound { bad_outputs } => Err(bad_outputs),
        }
    }

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
            Ok(s) if s == self.output => {
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
    ExtractedExample {
        drivers: &[driver],
        input: &input,
        limit: &limit,
        output: &output.to_string(),
    }.check().unwrap();
}
