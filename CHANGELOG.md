# Changelog, MSRV policy and cargo features

## cargo features

Features are provided to allow for a build with reduced dependencies.

 * `full`: Metafeature.
   Enable all reasonable, non-experimental, features.
 * `case`: [Case conversions](../doc_template_syntax/index.html#case-changing),
   using `heck`.
 * `expect`: the [`expect`](../doc_template_syntax/index.html#expect-items-expect-expr--syntax-check-the-expansion)
   expansion option for syntax checking.
 * `minimal-1`: Minimal feature set.  Must be enabled.

All of the above features are enabled by default.

## MSRV and MSRV policy

The Minimum Supported Rust Version for derive-adhoc is 1.54.

We expect to increase it cautiously and only with good reason.
(However, MSRV increase would be a minor version bump.)

## Changelog

### UNRELEASED

#### Breaking

 * `${paste }` no longer allows non-string literals in its content.
 * Invalid literal content within `${paste }` is now rejected
   during parsing, so even if that part of the template isn't expanded.
 * `${Xmeta... as lit}` is abolished, in favour of `... as str`
   (which has more consistent semantics).
 * `${Xmeta}` without `as` is no longer allowed in `${paste ...}`.
 * In `#[adhoc(attr="VALUE")]`, now only *string* literals are allowed.

#### Added

 * `${paste }` and `${CASE_CHANGE ...}` can now be nested,
    either (or both) ways round.
 * `${Xmeta... as str}`, `${Xmeta... as tokens}`.

#### Improved

 * Better error messages when `${paste }` produces a bad identifier.
 * docs: Minor improvements to reference.
 * internal: CI tests improved and extended
 * internal: cleanups, and internal docs improved.

### 0.3.0

#### Breaking

 * cargo features introduced.
   Currently, all enabled by default -
   no breakage if default features enabled.
   - Case conversion (and `heck` dependency) now behind `case`.
   - `minimal-1` introduced; it must be enabled.

#### Added

 * Expansion options facility
 * `expect items`, `expect expr` option, for improved errors
   when template expands to invalid Rust syntax.
   (`expect` cargo feature.)
 * `for struct`, `for enum`, `for union` option,
   for improved errors due to misue of a template.
 * `dbg` option, for dumping template expansions to compiler stderr.
 * `$dbg_all_keywords` keyword, for dumping keyword expansions.
 * `full` and `minimal-1` cargo meta features.

#### Improved

 * docs: Much expanded and improved tutorial (still a work in progress).
 * docs: Various corrections to reference docs.
 * docs: Reference documentation retitled and module renamed.
 * error handling: Better messages from certain situations involving
   multiple (incompatible) derive-adhoc versions.
 * tests: Made less fragile (more pinning of test dependencies).
 * tests: Improved CI checks on documentation, TODOs, etc.
 * internal: new HACKING.md file for helping develop derive-adhoc.

### 0.2.2

#### Fixed (future compatibility)

 * Pinned dependency from `derive-adhoc` to `derive-adhoc-macros`.
 * Handling of certain supposedly-future-compatible options fixed.
   ("future driver options" argument to `d_a_t_T`).

#### Improved

 * Better error messages with usupported combinations of features
   with mixed derive-adhoc versions.  #10.
 * Compatibility with derive-adhoc 0.2.0 tested in CI.

### 0.2.1

#### Fixed

 * `$vpat` expansion includes necessary post-field comma.  #15.
 * Docs typo fixes.

#### Improved

 * docs: Much expanded tutorial (still a work in progress)

### 0.2.0

#### Breaking

 * `$tgens` no longer includes defaults for generics.
 * `$Xattrs` by default
   outputs all attributes except derive-adhoc ones,
   rather than nothing (breaking bugfix).
 * `$vmeta` for a struct (not enum) processes top-level attributes,
   rather than imagining that there are no variant/value attributes.
 * Fixed hygiene (span) for `${paste }`;
   now it's consistently that of the template
   (but disagrees with the hygiene span of `$fname`).

#### Added

 * `$fpatname` `$vpat` `$vtype`, for value matching and construction
 * `$fvis` `$tvis`, for visibility (also as booleans)
 * `is_struct` `is_union`
   `v_is_unit` `v_is_tuple` `v_is_named`,
   conditions for driver shape.
 * `$tdefkwd` `$tdeftype` `$tdefvariants` `$vdefbody`
   `$fdefine` `$tdefgens`,
   for defining derived types,
 * `$select1`, exactly-one conditional
 * Support exporting a template to other crates,
   and `$crate` expansion for referring to template crate items.
 * Support exporting a driver to other crates
   (rather hazardous).

#### Fixed

 * Do not claim that `$ttype` includes any leading path elements.
 * `$` in templates always has proper span for error reporting
 * `$` is properly escaped in drivers
 * `$Xmeta` can expand to arbitrary tokens from an attribute value

#### Improved

 * docs: New tutorial (still a work in progress)
 * docs: Template syntax reference overhauled
 * Many other minor improvements
 * New tests/examples

### 0.1.0

 * First publicly advertised release.
   Much important documentation and
   many important features still missing.

### 0.0.1

 * Initial release to crates.io, not widely advertised.
