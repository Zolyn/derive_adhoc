use derive_adhoc::{derive_adhoc, Adhoc};
use std::fmt::Debug;
struct Both {
    #[adhoc(left)]
    a: usize,
    #[adhoc(right)]
    b: usize,
    #[adhoc(right)]
    c: usize,
}
#[automatically_derived]
impl ::core::default::Default for Both {
    #[inline]
    fn default() -> Both {
        Both {
            a: ::core::default::Default::default(),
            b: ::core::default::Default::default(),
            c: ::core::default::Default::default(),
        }
    }
}
#[automatically_derived]
impl ::core::fmt::Debug for Both {
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_struct_field3_finish(
            f,
            "Both",
            "a",
            &&self.a,
            "b",
            &&self.b,
            "c",
            &&self.c,
        )
    }
}
struct Left {
    a: usize,
}
#[automatically_derived]
impl ::core::default::Default for Left {
    #[inline]
    fn default() -> Left {
        Left {
            a: ::core::default::Default::default(),
        }
    }
}
#[automatically_derived]
impl ::core::fmt::Debug for Left {
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_struct_field1_finish(f, "Left", "a", &&self.a)
    }
}
struct Right {
    b: usize,
    c: usize,
}
#[automatically_derived]
impl ::core::default::Default for Right {
    #[inline]
    fn default() -> Right {
        Right {
            b: ::core::default::Default::default(),
            c: ::core::default::Default::default(),
        }
    }
}
#[automatically_derived]
impl ::core::fmt::Debug for Right {
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_struct_field2_finish(
            f,
            "Right",
            "b",
            &&self.b,
            "c",
            &&self.c,
        )
    }
}
fn main() {
    match (
        &{
            let res = ::alloc::fmt::format(
                ::core::fmt::Arguments::new_v1(
                    &[""],
                    &[::core::fmt::ArgumentV1::new_debug(&&Left::default())],
                ),
            );
            res
        },
        &"Left { a: 0 }",
    ) {
        (left_val, right_val) => {
            if !(*left_val == *right_val) {
                let kind = ::core::panicking::AssertKind::Eq;
                ::core::panicking::assert_failed(
                    kind,
                    &*left_val,
                    &*right_val,
                    ::core::option::Option::None,
                );
            }
        }
    }
}
