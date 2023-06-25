//! Blockquote examples involving expanding for certain toplevels

use super::*;

pub struct ForToplevelsConcatExample {
    pub loc: DocLoc,
    pub input: String,
    pub toplevels: Vec<String>,
    pub output: String,
}

impl Example for ForToplevelsConcatExample {
    fn print_checking(&self) {
        println!("checking :{} blockquotes input/output", self.loc);
    }
    fn check(&self, errs: &mut Errors, drivers: &[syn::DeriveInput]) {
        let (got, exp) = match (|| {
            let mut got = TokenStream::new();

            let template: Template<TokenAccumulator> =
                syn::parse_str(&self.input)
                    .map_err(|e| format!("parse template: {}", e))?;

            let exp: TokenStream = syn::parse_str(&self.output)
                .map_err(|e| format!("parse expected output: {}", e))?;

            for toplevel in &self.toplevels {
                (|| {
                    let driver = drivers
                        .iter()
                        .find(|d| d.ident == toplevel)
                        .ok_or_else(|| format!("driver not found"))?;

                    let crate_ = &parse_quote!(crate);
                    got.extend(
                        Context::call(driver, crate_, None, |ctx| {
                            let mut out = TokenAccumulator::new();
                            template.expand(&ctx, &mut out);
                            out.tokens()
                        })
                        .map_err(|e| format!("expansion failed: {}", e))?,
                    );
                    Ok::<_, String>(())
                })()
                .map_err(|e| format!("toplevel {}: {}", toplevel, e))?;
            }

            Ok::<_, String>((got, exp))
        })() {
            Ok(y) => y,
            Err(m) => {
                errs.wrong(self.loc, format_args!("example failed: {}", m));
                return;
            }
        };

        let err = match check_expected_actual_similar_tokens(&exp, &got) {
            Err(e) => e,
            Ok(()) => return,
        };
        eprintln!("==============================");
        errs.wrong(self.loc, format_args!("example expansion mismatch:"));
        eprintln!("expanded for: {}", self.toplevels.join(", "));
        err.eprintln("");
        eprintln!("----- input -----\n{}", self.input.trim_end());
        eprintln!("----- documented -----\n{}", self.output.trim_end());
        eprintln!("==============================");
    }
}
