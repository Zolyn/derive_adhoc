//! THIS IS A NON-WORKING CONCEPT, FOR FUTURE DEVELOPMENT
use derive_adhoc::{define_derive_adhoc, Adhoc};
#[derive_adhoc(PreciseClone)]
struct Unit;
impl Clone for Unit {
    fn clone(&self) -> Self {
        match self {
            Unit {} => Unit {},
        }
    }
}
#[derive_adhoc(PreciseClone)]
struct Tuple<F>(F);
impl<F> Clone for Tuple<F>
where
    F: Clone,
{
    fn clone(&self) -> Self {
        match self {
            Tuple { 0: f_0 } => Tuple::<F> { 0: f_0.clone() },
        }
    }
}
#[derive_adhoc(PreciseClone)]
struct Struct<F> {
    field: F,
}
impl<F> Clone for Struct<F>
where
    F: Clone,
{
    fn clone(&self) -> Self {
        match self {
            Struct { field: f_field } => {
                Struct::<F> {
                    field: f_field.clone(),
                }
            }
        }
    }
}
#[derive_adhoc(PreciseClone)]
enum Enum<F> {
    Unit,
    Tuple(F),
    Struct { field: F },
}
impl<F> Clone for Enum<F>
where
    F: Clone,
    F: Clone,
{
    fn clone(&self) -> Self {
        match self {
            Enum::Unit {} => Enum::Unit::<F> {},
            Enum::Tuple { 0: f_0 } => Enum::Tuple::<F> { 0: f_0.clone() },
            Enum::Struct { field: f_field } => {
                Enum::Struct::<F> {
                    field: f_field.clone(),
                }
            }
        }
    }
}
fn test<T: Clone>(value: &T) {
    let ours = value.clone();
    drop(ours);
}
fn main() {
    test(&Unit);
    test(&Tuple(String::new()));
    test(&Struct { field: 42 });
    test(&Enum::<()>::Unit);
    test(&Enum::Tuple(String::new()));
    test(&Enum::Struct { field: 66 });
}
