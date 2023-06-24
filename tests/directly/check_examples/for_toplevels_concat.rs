//! Blockquote examples involving expanding for certain toplevels

use super::*;

pub struct ForToplevelsConcatExample {
    pub loc: DocLoc,
    pub input: String,
    pub toplevels: Vec<String>,
    pub output: String,
}

impl Example for ForToplevelsConcatExample {
    #[allow(dead_code, unused_variables)] // TODO EXTEST
    fn check(&self, errs: &mut Errors, drivers: &[syn::DeriveInput]) {
        eprintln!(
            "FOR TOPLEVELS CONCAT EXAMPLE {:?}\n{}\n-----\n{}\n-----",
            self.toplevels, self.input, self.output
        );
        // TODO EXTEST
    }
}
