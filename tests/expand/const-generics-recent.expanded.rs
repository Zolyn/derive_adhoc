//! Test/example for const generics, including default values
//!
//! This is a separate file rather than (say) integrated into
//! `partial-ord.rs` or `ref-version.rs` because this feature is new
//! in Rust and our MSRV can't compile it.
//!
//! `recent` in the filename arranges for it not to be run by the MSRV
//! compiler.
#![allow(dead_code)]
use derive_adhoc::{derive_adhoc, Adhoc};
enum Enum<T = (), const N: usize = 1> {
    Unit,
    Tuple([T; N]),
}
enum EnumCopy<T = (), const N: usize = 1> {
    Unit,
    Tuple([T; N]),
}
impl<T, const N: usize> From<Enum<T, N>> for EnumCopy<T, N> {
    fn from(orig: Enum<T, N>) -> Self {
        match orig {
            Enum::Unit {} => EnumCopy::Unit::<T, N> {},
            Enum::Tuple { 0: f_0 } => EnumCopy::Tuple::<T, N> { 0: f_0 },
        }
    }
}
fn main() {
    let _: Option<Enum> = None;
    let _ = Enum::Unit::<u8, 3>;
    let _ = Enum::Tuple([42; 2]);
}
