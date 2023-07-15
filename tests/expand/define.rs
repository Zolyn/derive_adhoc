//! Test `${define }` and `${defcond }`
#![allow(dead_code)]

use derive_adhoc::{derive_adhoc, Adhoc};

#[derive(Adhoc)]
struct S {
    a: i32,
    b: i32,
}

derive_adhoc! {
    S expect items:

    ${define X 0}
    const X: i32 = $X;

    const L: &[(i32, i32)] = &[

        ${define X 901}
        ${defcond C false}
        (901, $X),
        (141, ${if C { 101 } else { 141 }}),

        ${define X 902}
        ${defcond C true}
        (902, $X),
        (102, ${if C { 102 } else { 142 }}),

        ${for fields {
            (902, $X),
            (202, ${if C { 202 } else { 242 }}),

            ${define X 903}
            ${defcond C false}

            (903, $X),
            (203, ${if not(C) { 203 } else { 243 }}),
            (343, ${if    C   { 303 } else { 343 }}),
        }}
    ];
}

#[derive(Adhoc)]
enum T<'g> {
    /// Expected value
    E(i32),
    /// Generated values (all must match corresponding `E`
    G(&'g [i32]),
}

derive_adhoc! {
    S expect items:

    ${define CASE {
        T::G(&[ 900 + $N ]),
        T::G(&[ 100 + $N ${if not(C) { + 40 }},
                ${if C { 100 + $N } else { 140 + $N }} ]),
    }}

    /// Interleaved expected and generated
    const T_DATA: &[T] = &[

        ${define N 1}
        ${defcond C true}
        ${CASE}
        T::E(901), T::E(101),

        ${define N 2}
        ${defcond C false}
        ${CASE}
        T::E(902), T::E(142),

        ${for fields {
            ${define N 3}
            ${defcond C true}
            ${CASE}
            T::E(903), T::E(103),
        }}

    ];
}

derive_adhoc! {
    T expect items:

    fn t_compare() {
      $(
        let mut ${snake_case $vname} = T_DATA
            .iter()
            .filter_map(|t| match t {
                $vpat => Some($( $fpatname )),
                _ => None,
            });
      )
        // zip_longest without itertools
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
}

fn main() {
    for (exp, got) in L {
        if exp != got {
            std::process::abort()
        }
    }

    t_compare();
}
