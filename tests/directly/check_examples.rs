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
//!  * `INPUT` for fields in `TYPE-OR-VARIANT`: `OUTPUT`
//!  * `INPUT` for others: `OUTPUT`
//!  * `INPUT` ...: ``OUTPUT``, ...
//!  * `INPUT` ...: nothing
//!  * `INPUT` ...: error, ``MESSAGE``
//!  * `CONDITION`: True for SOMETHING, SOMETHING, and ...`
//! ```
//!
//! ("others" means not any of the preceding contexts.
//! Note that double backquotes are required for "error,",
//! which allows individual backquotes in the messages themselves.
//! The MESSAGE must then be a substring of the actual error.)
//!
//! In an example of a `CONDITION`,
//! `SOMETHING` can be any of the syntaxes accepted in `for ...`
//! (but not "others", obviously).
//! All the contexts for which it returns true must be listed.
//!
//! In "for" clauses you can also write
//! a leading `struct ` or `Enum::`,
//! and a trailing `;`, `(...);`, or `{...}`.
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

mod conditions;
mod contexts;
mod for_toplevels_concat;
mod possibilities;
mod reference_extract;

use conditions::ConditionExample;
use contexts::{for_every_example_context, ContextExt as _, Limit};
use for_toplevels_concat::ForToplevelsConcatExample;
use possibilities::PossibilitiesExample;

const INPUT_FILE: &str = "doc/reference.md";

pub type DocLoc = usize;

/// Something that can be checked
pub trait Example {
    fn print_checking(&self);
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
pub struct DissimilarTokenStreams {
    exp: TokenStream,
    got: TokenStream,
    same: TokenStream,
    diff: itertools::EitherOrBoth<TokenTree, TokenTree>,
}

impl DissimilarTokenStreams {
    fn eprintln(&self, in_title: impl Display) {
        use itertools::EitherOrBoth;
        use EitherOrBoth as EOB;
        let in_title = in_title.to_string();
        eprintln!(
            "----- difference report {}{}-----",
            in_title,
            if in_title.is_empty() { "" } else { " " }
        );
        eprintln!(" expected:        {}", &self.exp);
        eprintln!(" actual:          {}", &self.got);
        eprintln!(" similar prefix:  {}", &self.same);

        let side = |s, getter: fn(_) -> Option<TokenTree>| {
            eprintln!(
                " {:16} {}",
                format!("{} token:", s),
                match getter(self.diff.clone()) {
                    None => format!("(none)"),
                    Some(TokenTree::Group(g)) => {
                        format!(
                            "{}",
                            proc_macro2::Group::new(
                                g.delimiter(),
                                quote!("..."),
                            )
                        )
                    }
                    Some(other) => format!("{}", other),
                }
            )
        };
        side("expected", EOB::left);
        side("actual", EOB::right);
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
fn check_expected_actual_similar_tokens(
    exp: &TokenStream,
    got: &TokenStream,
) -> Result<(), DissimilarTokenStreams> {
    use itertools::EitherOrBoth;
    use EitherOrBoth as EOB;

    /// Having `recurse` return this ensures that on error,
    /// we inserted precisely one placeholder message.
    struct ErrorPlaceholderInserted(EitherOrBoth<TokenTree, TokenTree>);

    fn recurse(
        a: &TokenStream,
        b: &TokenStream,
        same_out: &mut TokenStream,
    ) -> Result<(), ErrorPlaceholderInserted> {
        let mut input =
            a.clone().into_iter().zip_longest(b.clone().into_iter());
        loop {
            if input
                .clone()
                .filter_map(|eob| eob.left())
                .collect::<TokenStream>()
                .to_string()
                == "..."
            {
                // disregard rest of this group
                return Ok(());
            }
            let eob = match input.next() {
                Some(y) => y,
                None => break,
            };
            let mut mk_err = |tokens: TokenStream| {
                tokens.to_tokens(same_out);
                Err(ErrorPlaceholderInserted(eob.clone()))
            };
            let (a, b) = match &eob {
                EOB::Both(a, b) => (a, b),
                EOB::Left(_a) => {
                    return mk_err(quote!(MISSING_ACTUAL_TOKEN_HERE));
                }
                EOB::Right(_b) => {
                    return mk_err(quote!(UNEXPECTED_ACTUAL_TOKEN_HERE));
                }
            };
            if !match (a, b) {
                (TT::Group(a), TT::Group(b)) => {
                    if a.delimiter() != b.delimiter() {
                        return mk_err(quote!(
                            FOUND_DIFFERENT_DELIMITERS_HERE
                        ));
                    }
                    let mut sub = TokenStream::new();
                    let r = recurse(&a.stream(), &b.stream(), &mut sub);
                    proc_macro2::Group::new(a.delimiter(), sub)
                        .to_tokens(same_out);
                    let () = r?;
                    continue;
                }
                (a, b) => a.to_string() == b.to_string(),
            } {
                return mk_err(quote!(FOUND_DIFFERENCE_HERE));
            }
            a.to_tokens(same_out);
        }
        Ok(())
    }

    let mut same = TokenStream::new();
    recurse(exp, got, &mut same).map_err(|ErrorPlaceholderInserted(diff)| {
        DissimilarTokenStreams {
            same,
            diff,
            exp: exp.clone(),
            got: got.clone(),
        }
    })
}

#[test]
fn check_examples() {
    let mut errs = Errors::new();
    let (structs, examples) = reference_extract::extract(&mut errs);
    for example in &examples {
        example.print_checking();
        example.check(&mut errs, &structs);
    }
    eprintln!("checked {} examples", examples.len());
}
