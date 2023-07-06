# **Hacking on derive-adhoc (`HACKING.md`)**

Rust procedural macros are a somewhat awkward environment,
and, especially, testing them can be complex.

<!--##toc##-->
   * [Required reading](#required-reading)
   * [Generated and auto-updated files in the git tree](#generated-and-auto-updated-files-in-the-git-tree)
      * [`tests/pub-export/bizarre-facade/*` etc., updated by `maint/update-bizarre`](#testspub-exportbizarre-facade-etc-updated-by-maintupdate-bizarre)
      * [`Cargo.lock.example`, updated by `nailing-cargo update`.](#cargolockexample-updated-by-nailing-cargo-update)
      * [`Cargo.lock.minimal`, updated by `update-minimal-versions`.](#cargolockminimal-updated-by-update-minimal-versions)
      * [Tables of contents in various `*.md`, updated by `maint/update-tocs`.](#tables-of-contents-in-various-md-updated-by-maintupdate-tocs)
   * [Testing - see `tests/tests.rs`](#testing---see-teststestsrs)
   * [Reporting errors during template parsing and expansion](#reporting-errors-during-template-parsing-and-expansion)
   * [Adding an expansion keyword](#adding-an-expansion-keyword)
      * [Accessing the driver](#accessing-the-driver)
      * [Expansion keysords with content or arguments](#expansion-keysords-with-content-or-arguments)
      * [Adding a keyword that can appear in `${paste }` and/or `${CASE }`](#adding-a-keyword-that-can-appear-in-paste--andor-case-)
      * [Adding a boolean keyword](#adding-a-boolean-keyword)

## Required reading

derive-adhoc uses types and traits from [`syn`] and [`mod@quote`],
extensively.

It will be very helpful to run
```text
cargo doc --document-private-items --workspace
```
to get the rustdocs for the internal APIs.
That will also get a **rendering of this file with working links**,
as `target/doc/derive_adhoc_macros/_doc_hacking/index.html`.

[`NOTES.md`](_doc_notes) has some ideas for the future,
which we may or may not implement.
(Comments welcome!)

## Generated and auto-updated files in the git tree

The git tree contains some files which are actually
maintained by scripts in `maint/`.

### `tests/pub-export/bizarre-facade/*` etc., updated by `maint/update-bizarre`

"Bizarre" version of `derive-adhoc`,
used for [cross-crate compatibility testing](../../pub_b/index.html).

CI will check that these outputs are
up to date with
the normal top-level `Cargo.toml`s, and `pub-b.rs`,
from which they are generated.

### `Cargo.lock.example`, updated by `nailing-cargo update`.

Example lockfile.

Used in the CI tests,
which (in most tests) pin all of our dependencies.

If you're not a user of
[`nailing-cargo`](https://diziet.dreamwidth.org/tag/nailing-cargo)
you can update this
simply by copying a `Cargo.lock` made with `cargo update`.

### `Cargo.lock.minimal`, updated by `update-minimal-versions`.

Minimal versions of our dependencies,
used for CI testing of our MSRV, etc.

`update-minimal-versions` runs `cargo +nightly update ...`,
so you have to have a Rust Nightly installed.

### Tables of contents in various `*.md`, updated by `maint/update-tocs`.

These are inserted at the `<!--##toc##-->` marker.

Checked by CI, but it's only a warning if it's not up to date.

## Testing - see `tests/tests.rs`

derive-adhoc has comprehensive tests.

But, they are complicated
(mostly because testing proc macros is complicated).

You have to use **a particular version of Nightly Rust**.
**See [`tests/tests.rs`](../../derive_adhoc_tests/index.html)**
for information on how to run and update the tests.

## Reporting errors during template parsing and expansion

Generally, we use only `syn::Error` as the error type.
Use the [`MakeError`] convenience trait's
[`.error()`](MakeError::error) method
to construct errors.
Often, it is a good idea to generate an error
pointing at the relevant parts of both the driver and the template;
[`MakeError`]'s implementation on
[`[ErrorLoc]`](ErrorLoc) is good for this.

## Adding an expansion keyword

You need to decide if it should be useable in `${paste }`.
Generally, identifiers (or identifier-like things)
strings, 
and types should,
and other things shouldn't.
For now let's assume it shouldn't be useable in `${paste }`.

And, you need to decide if it should be useable
as a boolean expression, in `${if }` etc.
Again, for now, let's assume not.

Add the keyword to [`pub enum SubstDetails`](syntax::SubstDetails)
in `syntax.rs`.
If the keyword isn't a Rust keyword, use its name precisely,
in lowercase.
The enum variannt should contain:
 * Any arguments allowed and supplied, in their parsed form
 * Markers `O::NotInPaste` and `O::NotInBool`,
   as applicable.

Add the keyword to the parser in
`impl ... Parse for Subst`.
Use the provided `keyword!` macro.
For the markers, use `not_in_paste?` and `not_in_bool?`.

The compiler will now insist you add arms to various matches.
Most will be obvious.

The meat of the expansion - what your new keyword means -
is in `SubstDetails::expand`, in `expand.rs`.
For an expansion which isn't permitted in `${paste ..}`,
call 
[`out.append_tokens_with()`](framework::ExpansionOutput::append_tokens_with)
or
[`out.append_tokens()`](framework::ExpansionOutput::append_tokens).

You'll also want to add documentation to `doc/reference.md`,
arrangements for debug printing in `macros/dbg_allkw.rs`,
test cases in `tests/expand/` and maybe `tests/ui/`,
and possibly discussion in `doc/introduction.md`.

### Accessing the driver

Information about the driver (and the current variant and field)
is available via [`framework::Context`].

(Use the methods on `Context`, such as
[`field()`](framework::Context::field),
to get access to the per-field and per-variant details,
rather than using `Context.variant`
and open-coding the error handling for `None`.)

### Expansion keywords with content or arguments

Parse the content from `input`, in the `keyword!` invocation.
See `tmeta` et al for an example.

Usually it is best to make a Rust type to represent
the content or arguments,
if there isn't a suitable one already.

To parse a boolean expression, use `Subst<BooleanContext>`.
(Probably, in a `Box`, like in `when`).

Normally it is best to put the `O::Not...` markers
directly in the `SubstDetails` enum variant;
that makes it easier to extract them for use in the `match` arms.

It is fine to have thsee markers in an argument type *as well*.
For a sophisticated example of this, 
see `SubstMeta`,
which allows `... as ...`, except in boolean context.

For named arguments, use [`syntax::ParseUsingSubkeywords`].

### Adding a keyword that can appear in `${paste }` and/or `${CASE }`

Removing `O::NotInPaste` marker from a `SubstDetails` variant
will allow the template to contain that keyword
within `${paste}` and `${CASE}`.

You won't be able to call `out.append_tokens` any more.
Instead, you must use one of the more specific
[`framework::ExpansionOutput`] methods,
such as `append_identfrag` or `append_idpath`.

### Adding a boolean keyword

This is fairly straightforward.
Use `is_enum` (say) as an example.
