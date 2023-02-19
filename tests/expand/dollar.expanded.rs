#![feature(macro_metavar_expr)]
#![allow(dead_code)]
use derive_adhoc::{define_derive_adhoc, derive_adhoc, Adhoc};
#[derive_adhoc(ReuseableTemplate)]
///one $dollar $template end
struct OneDollar;
///one $dollar $template end
struct ReusedWithOneDollar;
///one $dollar $template end
struct AdhocOneDollar;
#[derive_adhoc(ReuseableTemplate)]
///two $$dollars
struct TwoDollars;
///two $$dollars
struct ReusedWithTwoDollars;
///two $$dollars
struct AdhocTwoDollars;
fn main() {}
