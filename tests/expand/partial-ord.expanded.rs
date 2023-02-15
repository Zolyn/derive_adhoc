//! THIS IS A NON-WORKING CONCEPT, FOR FUTURE DEVELOPMENT
use std::cmp::Ordering::{self, *};
use derive_adhoc::{define_derive_adhoc, Adhoc};
#[derive_adhoc(VeryPartialOrd)]
enum Enum<F: PartialEq, G>
where
    G: PartialEq,
{
    Unit,
    Tuple(F),
    Struct { field: G },
}
impl<F: PartialEq, G> PartialOrd for Enum<F, G>
where
    F: PartialOrd,
    G: PartialOrd,
    G: PartialEq,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Enum::Unit {}, Enum::Unit {}) => {
                let ord = Equal;
                Some(ord)
            }
            (Enum::Tuple { 0: self_0 }, Enum::Tuple { 0: other_0 }) => {
                let ord = Equal;
                let ord = ord.then(PartialOrd::partial_cmp(self_0, other_0)?);
                Some(ord)
            }
            (
                Enum::Struct { field: self_field },
                Enum::Struct { field: other_field },
            ) => {
                let ord = Equal;
                let ord = ord.then(PartialOrd::partial_cmp(self_field, other_field)?);
                Some(ord)
            }
            _ => None,
        }
    }
}
#[automatically_derived]
impl<F: PartialEq, G> ::core::marker::StructuralPartialEq for Enum<F, G>
where
    G: PartialEq,
{}
#[automatically_derived]
impl<
    F: ::core::cmp::PartialEq + PartialEq,
    G: ::core::cmp::PartialEq,
> ::core::cmp::PartialEq for Enum<F, G>
where
    G: PartialEq,
{
    #[inline]
    fn eq(&self, other: &Enum<F, G>) -> bool {
        let __self_tag = ::core::intrinsics::discriminant_value(self);
        let __arg1_tag = ::core::intrinsics::discriminant_value(other);
        __self_tag == __arg1_tag
            && match (self, other) {
                (Enum::Tuple(__self_0), Enum::Tuple(__arg1_0)) => *__self_0 == *__arg1_0,
                (Enum::Struct { field: __self_0 }, Enum::Struct { field: __arg1_0 }) => {
                    *__self_0 == *__arg1_0
                }
                _ => true,
            }
    }
}
fn mk_t_struct<F: PartialEq>(field: &str) -> Enum<F, &str> {
    Enum::Struct { field }
}
fn main() {
    use Enum::*;
    expect_none(Unit::<_, ()>, Tuple(42));
    expect_none(Tuple(42), mk_t_struct(""));
    expect_none(Tuple::<_, ()>(Tuple::<_, ()>(42)), Tuple(Unit));
    expect_some(Unit::<(), ()>, Unit::<(), ()>, Equal);
    expect_some(Tuple::<_, ()>(0), Tuple(0), Equal);
    expect_some(Tuple::<_, ()>(1), Tuple(2), Less);
    expect_some(Tuple::<_, ()>(4), Tuple(3), Greater);
    expect_some(mk_t_struct::<()>("a"), mk_t_struct("a"), Equal);
    expect_some(mk_t_struct::<()>("b"), mk_t_struct("c"), Less);
    expect_some(mk_t_struct::<()>("e"), mk_t_struct("d"), Greater);
}
/// Versions of assert, basically
///
/// Without too many macros cluttering the expanded output
fn expect(ok: bool) {
    if !ok {
        { ::std::rt::begin_panic("explicit panic") };
    }
}
fn expect_none<T: PartialOrd>(a: T, b: T) {
    expect(a.partial_cmp(&b) == None);
}
fn expect_some<T: PartialOrd>(a: T, b: T, ord: Ordering) {
    expect(a.partial_cmp(&b) == Some(ord));
}
