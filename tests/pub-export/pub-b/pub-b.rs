//! Test / demo of cross-crate and compatibility
//!
//! Our test has two crates.  This is the higher-layer one, which
//! invokes a reuseable template from pub-a, and invokes an ad-hoc
//! template of its own on a driver from pub-a.

pub mod adhoc_template {
    use derive_adhoc::derive_adhoc;

    pub trait NumFields {
        fn num_fields() -> usize;
    }

    /// When applying a template to a driver struct defined in another
    /// module, the driver's own type name, and the names used for field
    /// types etc., will; be *textually* substituted.
    ///
    /// See the note next to `pub mod a_driver` in `pub-b.rs`.
    ///
    /// Here we import the whole of the driver's module. so that the
    /// unqualified struct name and field names are useable here.
    use pub_a::a_driver::*;

    derive_adhoc! {
        // When expanding a template for a remote struct, pass
        // the crate name and struct name to `derive_adhoc!`.
        //
        // (Macro scoping rules mean that the driver is visible
        // only at the top-level, not in the module it was defined in.)
        //
        // The top-level struct name must *also* be in scope, here,
        // as an unqualified name: that's what `$ttype` expands to.
        //
        // TODO DOCS Currently the syntax reference says of `$ttype` that
        // it might contain a path.  This is not in fact ever true.
        pub_a::ADriver:

        impl<$tgens> NumFields for $ttype {
            fn num_fields() -> usize {
                $( let _: $ftype; )

                0 + ${for fields { 1 }}
            }
        }
    }
}

pub mod b_driver {
    use derive_adhoc::Adhoc;

    pub struct BField<T: Default>(T);

    /// Here we invoke a template from another crate.
    /// We can freely refer to local names in our struct definition,
    /// because the template's expansion appears here, in this scope.
    ///
    /// The template, in `pub-a.rs`, is referenced by its crate,
    /// but *not* its module (`pub_a::a_trait`).
    #[derive(Adhoc)]
    #[derive_adhoc(pub_a::IsEnum)]
    pub enum BDriver<T: Default> {
        Variant(BField<T>),
    }
}

#[test]
fn invoke() {
    use adhoc_template::NumFields;
    use pub_a::a_trait::IsEnum;
    assert_eq!(pub_a::a_driver::ADriver::<()>::num_fields(), 1);
    assert!(b_driver::BDriver::<()>::is_enum().is_some());
}
