//! Test / demo of cross-crate and compatibility
//!
//! Our test has two crates.  This is the lower-layer one, which
//! provides a trait derivable with derive_adhoc (ie, a trait
//! and corresponding reuseable template).
//!
//! It also provides a struct exported as a derive-adhoc driver.

// Any crate exposing a derive_adhoc template must re-export derive_adhoc
// itself at the top level.  This will be used during template invocation
// to find the right version of the template expansion machinery.
pub use derive_adhoc;

pub use a_driver::ADriver;

// For local use of the template, within the same crate,
// `#[macro_use]` is needed to extend the textual scope of the macro.
// This is because, as the compiler says,
//    macro-expanded `macro_export` macros from the current crate
//    cannot be referred to by absolute paths
#[macro_use]
pub mod a_trait {
    use derive_adhoc::define_derive_adhoc;

    /// Demonstrates use of a local name in a template.
    /// We must make it visible everywhere the template will be expanded.
    pub trait IsEnum {
        fn is_enum() -> Option<()>;
    }

    define_derive_adhoc! {
        pub IsEnum =

        // When the template wants to refer to things in its own
        // crate, it must use fully qualified names, starting with
        // `$crate`, which works the similarly in `macro_rules!`.
        impl<$tgens> $crate::a_trait::IsEnum for $ttype {
            // For a name like `Option` it is a matter of taste
            // whether to refer explicitly to `std`, or just rely on
            // the caller not having messed up their namespace.
            // (Again, this is similar to macro_rules!)
            fn is_enum() -> Option<()> {
                ${if is_enum {
                    Some(())
                } else {
                    None
                } }
            }
        }
    }
}

mod local_use {
    use derive_adhoc::Adhoc;

    // When we refer to a local template, we do so without a path,
    // even though it was an exported template.
    #[derive(Adhoc)]
    #[derive_adhoc(IsEnum)]
    pub(crate) struct Local;
}

pub mod a_driver {
    use derive_adhoc::Adhoc;

    /// A struct which is exported as a derive-adhoc driver (iw with
    /// `#[derive_adhoc(pub)]` must be *visilbe* outside the crate.
    ///
    /// Doing this effectively turns the body of the struct into a macro,
    /// with very limited support for namespacing and hygiene!
    /// (So this feature should be used with care and caution.)
    ///
    /// The *user* of such an exported driver will need not only access to
    /// the driver macro (which is exported at the top-level of the driver's
    /// crate), but also all of the types involved in the struct.
    ///
    /// That includes the struct type itself, of course, but also all the
    /// names it refers to (for example, the types of the fields).
    /// The most reasonable way to do this is probably to use unqualified
    /// names in the driver definition, and export all of those names.
    /// The user can then `use *` to obtain a suitable namespace for
    /// invoking the driver with their own template.
    ///
    /// Additionally, of course, this exposes all of the field names and
    /// types of the struct.  So it is probably not a good idea with a
    /// driver that isn't completely pub and exhaustive.  (This could be
    /// checked by derive_adhoc!, but then we would want a way to override
    /// the error, too.  All this additional complexity seems unwarranted
    /// for a feature which is so difficult to use for other reasons too.)
    ///
    /// So we will say that as a rule of thumb, we expect that any exported
    /// driver will have bespoke rules about how a depending crate might
    /// use it, and what would and wouldn't count as a semver major change.
    /// Anyone who *uses* an exported driver should refer to those docs,
    /// on pain of risking breakages due to `cargo update`.
    #[derive(Adhoc)]
    #[derive_adhoc(pub)]
    pub struct ADriver<T: Default> {
        pub field: AField<T>,
    }

    pub struct AField<T: Default>(T);
}

#[test]
fn invoke() {
    use a_trait::IsEnum;
    assert!(local_use::Local::is_enum().is_none());
}
