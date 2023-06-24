//! Check the examples in the reference manual
//!
//! Looks for bullet points and blockquotes
//! in sections with title starting "Examples":
//!
//! ```text
//!  * `INPUT`: `OUTPUT`
//!  * `INPUT` for struct: `OUTPUT`
//!  * `INPUT` for structs: `OUTPUT`
//!  * `INPUT` for enum: `OUTPUT`
//!  * `INPUT` for enums: `OUTPUT`
//!  * `INPUT` for enum variant: `OUTPUT`
//!  * `INPUT` for enum variants: `OUTPUT`
//!  * `INPUT` for `TYPE-OR-VARIANT`: `OUTPUT`
//!  * `INPUT` for `FIELD` in `TYPE-OR-VARIANT`: `OUTPUT`
//!  * `INPUT` for others: `OUTPUT`
//! ```
//!
//! (Others means not any of the preceding contexts.)
//!
//! Blockquotes ` ```rust ` are tested separately via rustdoc, so ignored here.
//!
//! Otherwise, they should come in pairs, with, in between,
//! ```text
//!   <!--##examples-for-toplevels-concat TYPE TYPE...##-->
//! ```
//! And then the first is expanded for each TYPE;
//! the results (concatenated) must match the 2nd block.
//!
//! Special directives
//!
//!   * `<!--##examples-ignore##-->`:
//!
//!       Ignore until next blank line
//!
//!   * `<!--##examples-for FOO##-->`:
//!
//!       In bullet point(s), use this as if "for FOO" was written
//!       (ignoring any actual "for FOO")
//!       Applies until end of section (or next such directive)
//!
//! Preceding a ` ```...``` ` quote
//!
//!   * `<!--##examples-structs##-->`:
//!
//!       The quote has the example structs
//!

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

/// Tries to compare but disregarding spacing, which is unpredictable
///
/// What we really want to know is
/// whether the two `TokenStream`s mean the same.
/// This is not so straightforward.
/// Neither `TokenStream` nor `TokenTree` are `PartialEq`,
/// The string representation of a `TokenStream` has unpredictable spacing:
/// it can even inherit *some but not all* of the input spacing,
/// and, empirically, it can depend on whether the tokens went through `syn`
/// (and presumably which `syn` type(s)).
/// For example, output from `derive-adhoc`
/// that came via an expansion that used `syn::Type`
/// can have different spacing to
/// a string with the same meaning, converted to `TokenStream` and back.
///
/// The algorithm in this function isn't perfect but I think it will do.
fn similar_token_streams(a: &TokenStream, b: &TokenStream) -> bool {
    for eob in a.clone().into_iter().zip_longest(b.clone().into_iter()) {
        let (a, b) = match eob {
            itertools::EitherOrBoth::Both(a, b) => (a, b),
            _ => return false,
        };
        if !match (a, b) {
            (TT::Group(a), TT::Group(b)) => {
                a.delimiter() == b.delimiter() &&
                    similar_token_streams(&a.stream(), &b.stream())
            },
            (a, b) => a.to_string() == b.to_string()
        } {
            return false
        }
    }
    return true
}

#[test]
fn check_examples() {
    let mut errs = Errors::new();
    let (structs, examples) = reference_extract::extract(&mut errs);
    for example in &examples {
        example.check(&mut errs, &structs);
    }
    eprintln!("checked {} examples", examples.len());
}
