#![feature(macro_metavar_expr)]
#![allow(dead_code)]
use derive_adhoc::{define_derive_adhoc, derive_adhoc, Adhoc};
#[derive_adhoc(ReuseableTemplate)]
///one $dollar $template end
struct OneDollar;
struct ReusedWithOneDollar;
struct AdhocOneDollar;
#[derive_adhoc(ReuseableTemplate)]
///two $$dollars
struct TwoDollars;
struct ReusedWithTwoDollars;
struct AdhocTwoDollars;
fn main() {}
