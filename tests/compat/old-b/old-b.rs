/// Forward/backward compatibility tests
/// See tests/compat/README.md

pub mod adhoc_template {
    use derive_adhoc::derive_adhoc;

    pub trait NumFields {
        fn num_fields() -> usize;
    }

    use pub_a::a_driver::*;

    derive_adhoc! {
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
