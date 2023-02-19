
#![allow(dead_code)]

use derive_adhoc::{define_derive_adhoc, derive_adhoc, Adhoc};

define_derive_adhoc! {
    ReuseableTemplate =

    $tattrs struct ${paste ReusedWith $tname};
}

#[derive(Adhoc)]
#[derive_adhoc(ReuseableTemplate)]
#[doc=stringify!(one $ dollar $template end)]
struct OneDollar;

derive_adhoc! {
    OneDollar:

    $tattrs struct AdhocOneDollar;
}

#[derive(Adhoc)]
#[derive_adhoc(ReuseableTemplate)]
#[doc=stringify!(two $$ dollars)]
struct TwoDollars;

derive_adhoc! {
    TwoDollars:

    $tattrs struct AdhocTwoDollars;
}

fn main(){}
