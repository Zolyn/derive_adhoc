//! Test `${define }` and `${defcond }`
#![allow(dead_code)]
use derive_adhoc::{derive_adhoc, Adhoc};
struct S {
    a: i32,
    b: i32,
}
const X: i32 = 0;
const L: &[(i32, i32)] = &[
    (901, 901),
    (141, 141),
    (902, 902),
    (102, 102),
    (902, 902),
    (202, 202),
    (903, 903),
    (203, 203),
    (343, 343),
    (902, 902),
    (202, 202),
    (903, 903),
    (203, 203),
    (343, 343),
];
enum T<'g> {
    /// Expected value
    E(i32),
    /// Generated values (all must match corresponding `E`
    G(&'g [i32]),
}
/// Interleaved expected and generated
const T_DATA: &[T] = &[
    T::G(&[900 + 1]),
    T::G(&[100 + 1, 100 + 1]),
    T::E(901),
    T::E(101),
    T::G(&[900 + 2]),
    T::G(&[100 + 2 + 40, 140 + 2]),
    T::E(902),
    T::E(142),
    T::G(&[900 + 3]),
    T::G(&[100 + 3, 100 + 3]),
    T::E(903),
    T::E(103),
    T::G(&[900 + 3]),
    T::G(&[100 + 3, 100 + 3]),
    T::E(903),
    T::E(103),
];
fn t_compare() {
    let mut e = T_DATA
        .iter()
        .filter_map(|t| match t {
            T::E { 0: f_0 } => Some(f_0),
            _ => None,
        });
    let mut g = T_DATA
        .iter()
        .filter_map(|t| match t {
            T::G { 0: f_0 } => Some(f_0),
            _ => None,
        });
    while let Some(e) = e.next() {
        let g = g.next().unwrap();
        for g in *g {
            if e != g {
                std::process::abort();
            }
        }
    }
    if e.next().is_some() {
        std::process::abort();
    }
}
fn main() {
    for (exp, got) in L {
        if exp != got {
            std::process::abort()
        }
    }
    t_compare();
}
