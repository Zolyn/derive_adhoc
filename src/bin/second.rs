// This is a file full of random crap for experimentation.
//
// Instead, there should be a POC/MVP test case?, which maybe this is
// becoming?

use derive_adhoc_macros::{derive_adhoc, derive_adhoc_expand, Adhoc};

#[derive(Adhoc, Debug, Clone, Eq, Default, PartialEq)]
pub struct ChannelsParams {
    //    #[field educe(Default(expression = "interim_enable_by_env_var()"))]
    padding_enable: bool,

    #[adhoc(foo)]
    padding_parameters: usize,
}

/*
derive_adhoc_apply_ChannelsParams!{

    #[derive_adhoc(ChannelsParams)]
    #[derive(Debug, Default, Clone, Eq, PartialEq)]
    pub struct ChannelsParamsUpdates {
    $(
    /// New value, if it has changed.
    pub(crate) $field: Option<$ty>,
    )*

            thing: &'lifetime T,
            'char'
    }

}
*/

// Possible invocation ssyntaxes

/*
derive_adhoc!{
    #[derive_adhoc(ChannelsParams)]
    type Wombat1 = $ Struct;
}*/

derive_adhoc! {
    ChannelsParams:
    type Wombat = $ tname;
}

/*#[derive_adhoc(ChannelsParams)] x!{
    type Wombat3 = $ Struct;
}

derive_adhoc_apply_ChannelsParams!{
    type Wombat4 = $ Struct;
}*/

type K = Wombat;

/*
#[derive(Debug, Default, Clone, Eq, PartialEq)]
pub struct ChannelsParamsUpdates {
$(
    /// New value, if it has changed.
    pub(crate) $field: Option<$ty>,
)*
}
*/

fn main() {
    let _: K = ChannelsParams::default();

    derive_adhoc! {
        ChannelsParams:
        $(
//            ${when not(fattr(foo))}
            println!("field name {:?}", stringify!($fname));
        )
    }
}
