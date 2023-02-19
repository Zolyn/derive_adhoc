//! Example which derives a new type containing references
//!
//! This demonstrates how to make a new type which mirrors the driver type,
//! including both structs and enums, with units, tuples or structs.
//!
//! It also demonstrates how to construct a new enum type
//! using `$vdefvariants`, and `$vdefbody` and `$fdefine}`,
//! and handling of visibility attributes.
#![allow(dead_code)]
use derive_adhoc::{define_derive_adhoc, Adhoc};
#[derive_adhoc(ReferenceVersion)]
struct Tuple<F = ()>(F);
struct TupleReference<'reference, F = ()>(&'reference F);
impl<'reference, F> From<&'reference Tuple<F>> for TupleReference<'reference, F> {
    fn from(ref_to_owned: &'reference Tuple<F>) -> Self {
        match ref_to_owned {
            Tuple { 0: f_0 } => TupleReference::<F> { 0: f_0 },
        }
    }
}
impl<'reference, F> TupleReference<'reference, F>
where
    F: Clone,
{
    fn cloned(&self) -> Tuple<F> {
        match self {
            Self { 0: f_0 } => Tuple::<F> { 0: (**f_0).clone() },
        }
    }
}
#[derive_adhoc(ReferenceVersion)]
struct Struct<F = ()> {
    field: F,
}
struct StructReference<'reference, F = ()> {
    field: &'reference F,
}
impl<'reference, F> From<&'reference Struct<F>> for StructReference<'reference, F> {
    fn from(ref_to_owned: &'reference Struct<F>) -> Self {
        match ref_to_owned {
            Struct { field: f_field } => {
                StructReference::<F> {
                    field: f_field,
                }
            }
        }
    }
}
impl<'reference, F> StructReference<'reference, F>
where
    F: Clone,
{
    fn cloned(&self) -> Struct<F> {
        match self {
            Self { field: f_field } => {
                Struct::<F> {
                    field: (**f_field).clone(),
                }
            }
        }
    }
}
#[derive_adhoc(ReferenceVersion)]
enum Enum<F = ()> {
    Unit,
    Tuple(F),
    Struct { field: F },
}
enum EnumReference<'reference, F = ()> {
    Unit,
    Tuple(&'reference F),
    Struct { field: &'reference F },
}
impl<'reference, F> From<&'reference Enum<F>> for EnumReference<'reference, F> {
    fn from(ref_to_owned: &'reference Enum<F>) -> Self {
        match ref_to_owned {
            Enum::Unit {} => EnumReference::Unit::<F> {},
            Enum::Tuple { 0: f_0 } => {
                EnumReference::Tuple::<F> {
                    0: f_0,
                }
            }
            Enum::Struct { field: f_field } => {
                EnumReference::Struct::<F> {
                    field: f_field,
                }
            }
        }
    }
}
impl<'reference, F> EnumReference<'reference, F>
where
    F: Clone,
    F: Clone,
{
    fn cloned(&self) -> Enum<F> {
        match self {
            Self::Unit {} => Enum::Unit::<F> {},
            Self::Tuple { 0: f_0 } => {
                Enum::Tuple::<F> {
                    0: (**f_0).clone(),
                }
            }
            Self::Struct { field: f_field } => {
                Enum::Struct::<F> {
                    field: (**f_field).clone(),
                }
            }
        }
    }
}
fn main() {
    let _: Option<EnumReference> = None;
    let _: Option<EnumReference<i32>> = None;
}
