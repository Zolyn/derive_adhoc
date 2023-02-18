//! Test/demonstrate within-crate cross-module scoping
//!
//! In practice, one would probably put the "ought to be in scope"
//! names into an internal prelude module, and `use internal_prelude::*`.
//! That would allow natural use of the identifiers from the prelude
//! everywhere and avoid having to write qualified paths in templates
//! or worry about precisely which things should be imported where.
//!
//! Sadly `macro_rules!` macro scoping doesn't work that way, so one
//! would still need the `#[macro_use]` annotations.  And, the ordering
//! of the code is significant - so it can be necessary to have a particular
//! ordering of `mod` statements.

// Reminder, this is a *module* inside a test crate.
// The name of this module is `crate::modules`
// and it is within `#[cfg(test)]`.

#![allow(dead_code)]

struct EnumMarker;
trait IsEnum {
    fn is_enum() -> Option<EnumMarker>;
}
trait NumFields {
    fn num_fields() -> usize;
}

/// When defining a template that is to be used outside its defining
/// module, but inside the same crate, `#[macro_use]` is needed, on the
/// containing module(s).  The template is then available *textually after*
/// its definition.
///
/// (Exporting of a template to other crates is demonstrated in
/// `tests/pub-export/pub-b/pub-a.rs` and `pub-b.rs`.)
///
/// The remaining principles about use of names in a derive-adhoc template
/// are basically the same as those for a `macro_rules!` macro.
#[macro_use]
mod has_template {
    use derive_adhoc::define_derive_adhoc;

    /// Demonstrates use of a local name in a template.
    /// We must make it visible everywhere the template will be expanded.
    pub(super) const ENUM_MARKER: Option<super::EnumMarker> =
        Some(super::EnumMarker);

    define_derive_adhoc! {
        IsEnum =

        // When the template wants to refer to things, it is usually most
        // convenient to expect the template user to bring the needed parts
        // into scope at the template invocation site.  We do that here
        // for IsEnum and EnumMarker;
        impl<$tgens> IsEnum for $ttype {
            fn is_enum() -> Option<EnumMarker> {
                // Alternatively, the template can refer to items by
                // absolute crate path.  The items must still be visible
                // at the template invocation site.
                //
                // We do that here with `ENUM_MARKER`.
                //
                // (`super::` isn't adviseable in a macro, including in a
                // derive-adhoc template, because its meaning at the
                // expansion site would depend on the scope context there
                // (eg, module depth.)
                ${if is_enum {
                    crate::modules::has_template::ENUM_MARKER
                } else {
                    None
                } }
            }
        }
    }
}

/// When definining a driver that is to be used as a derive input
/// outside its defining module, but inside the same crate,
/// `#[macro_use]` is needed, on the containing module(s).  The driver
/// is then available for `derive_adhoc!` *textually after* its
/// definition.
#[macro_use]
pub mod has_driver {
    use derive_adhoc::Adhoc;

    // Additionally, for a driver to be useable outside its module, the
    // driver type name and types of its fields etc., need to be visible,
    // for the use of expansions in other modules.
    pub struct Field<T: Default>(T);

    // When applying a template that has expectations about the
    // invocation scope, we need to satisfy those, by bringing into
    // scope the things that the template expects.
    use super::{EnumMarker, IsEnum};

    #[derive(Adhoc)]
    #[derive_adhoc(IsEnum)]
    pub enum Driver<T: Default> {
        Variant(Field<T>),
    }
}

mod adhoc_template {
    use derive_adhoc::derive_adhoc;

    // When applying a template to a driver struct defined in another
    // module, the driver's own type name, and the names used for field
    // type=s etc.  that the macro might muse, must be brought into scope
    // manually.
    use super::has_driver::{Driver, Field};

    derive_adhoc! {
        // When expanding a template for a crate-local struct, pass
        // just the name of the struct to `derive_adhoc!`.
        //
        // (Exporting of the driver so that other crates can derive from it
        // is supported via `#[derive_adhoc(pub)]` but it brings
        // namespacing awkwardness and should be used only with crate.)
        //
        // (Passing a path doesn't work for a struct with an unexported
        // derive-adhoc driver, because the name here is turned into
        // the name of a `macro_rules!` macro, and a crate-local one
        // of those those doesn't have any path scope.)
        Driver:

        impl<$tgens> super::NumFields for $ttype {
            fn num_fields() -> usize {
                $( let _: $ftype; )

                0 + ${for fields { 1 }}
            }
        }
    }
}

#[test]
fn invoke() {
    assert!(has_driver::Driver::<()>::is_enum().is_some());
    assert_eq!(has_driver::Driver::<()>::num_fields(), 1);
}
