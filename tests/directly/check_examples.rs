//! Check the examples in the reference manual

#![allow(dead_code, unused_imports, unused_variables)] // XXXX

use super::*;

mod possibilities;
mod reference_extract;

const INPUT_FILE: &str = "doc/reference.md";

pub type DocLoc = usize;

/// Something that can be checked
pub trait Example {
    fn check(&self, out: &mut Errors, drivers: &[syn::DeriveInput]);
}

/// Allows errors to be aggregated
pub struct Errors {
    ok: Result<(), ()>,
}
impl Errors {
    fn new() -> Self {
        Errors { ok: Ok(()) }
    }
    fn wrong(&mut self, loc: DocLoc, msg: impl Display) {
        eprintln!("{INPUT_FILE}:{loc}: {msg}");
        self.ok = Err(());
    }
}
impl Drop for Errors {
    fn drop(&mut self) {
        if !std::thread::panicking() && !self.ok.is_ok() {
            panic!("documentation examples check failed");
        }
    }
}

fn bail(loc: DocLoc, msg: impl Display) -> ! {
    Errors::new().wrong(loc, msg);
    panic!("Errors should have panicked already!");
}

#[test]
#[should_panic] // XXXX incomplete
fn check_examples() {
    let mut errs = Errors::new();
    let (structs, examples) = reference_extract::extract(&mut errs);
    for example in &examples {
        example.check(&mut errs, &structs);
    }
    eprintln!("checked {} examples", examples.len());
}
