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
//! (which shuld be followed by introductory text for the reader).
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

use super::*;

mod for_toplevels_concat;
mod possibilities;
mod reference_extract;

use for_toplevels_concat::ForToplevelsConcatExample;

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

#[derive(Debug)]
#[allow(dead_code)] // TODO EX TEST
pub struct DissimilarTokenStreams {
    same: TokenStream,
    diff: itertools::EitherOrBoth<TokenTree, TokenTree>,
}

impl DissimilarTokenStreams {
    fn eprintln(&self) {
        use itertools::EitherOrBoth;
        use EitherOrBoth as EOB;
        eprintln!("----- difference report -----");
        eprintln!("same: {}", &self.same);
        let side = |s, getter: fn(_) -> _| {
            match getter(self.diff.as_ref()) {
                None => eprintln!("{} ended earlier", s),
                Some(t) => eprintln!("{}: {}", s, t),
            }
        };
        side("exp", EOB::left);
        side("got", EOB::right);
    }
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
fn check_expected_actual_similar_tokens(exp: &TokenStream, got: &TokenStream)
                         -> Result<(), DissimilarTokenStreams>
{
    use itertools::EitherOrBoth;
    use EitherOrBoth as EOB;

    fn recurse(
        a: &TokenStream,
        b: &TokenStream,
        same_out: &mut TokenStream,
    ) -> Result<(), EitherOrBoth<TokenTree, TokenTree>> {
        for eob in a.clone().into_iter().zip_longest(b.clone().into_iter()) {
            let (a, b) = match &eob {
                EOB::Both(a, b) => (a, b),
                _ => return Err(eob),
            };
            if !match (a, b) {
                (TT::Group(a), TT::Group(b)) => {
                    if a.delimiter() != b.delimiter() {
                        return Err(eob);
                    }
                    let () = recurse(&a.stream(), &b.stream(), same_out)?;
                    true
                }
                (a, b) => a.to_string() == b.to_string(),
            } {
                return Err(eob);
            }
            a.to_tokens(same_out);
        }
        Ok(())
    }

    let mut same = TokenStream::new();
    recurse(exp, got, &mut same).map_err(|diff| DissimilarTokenStreams {
        same, diff,
    })
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
