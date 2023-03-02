# Changelog and MSRV policy

## MSRV and MSRV policy

The Minimum Supported Rust Version for derive-adhoc is 1.54.

We expect to increase it cautiously and only with good reason.
(However, MSRV increase would be a minor version bump.)

## Changelog

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