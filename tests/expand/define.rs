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

fn main() {
    for (exp, got) in L {
        if exp != got {
            std::process::abort()
        }
    }
}
