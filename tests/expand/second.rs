use derive_adhoc::{derive_adhoc, Adhoc};

#[derive(Adhoc, Default)]
/// Some docs
pub struct ChannelsParams {
    /// thing
    ///
    /// paragraph
    #[allow(dead_code)]
    padding_enable: bool,

    #[allow(dead_code)]
    padding_parameters: usize,
}

derive_adhoc! {
    ChannelsParams:

    #[derive(Debug, Default, Clone, Eq, PartialEq)]
    pub struct ChannelsParamsUpdates {
        $(
            // ${fattrs doc[0]}
            ${fattrs serde}
            ///
            /// New value, if it has changed.
            //
            // ${fattrs doc[+]}
            pub(crate) $fname: Option<$ftype>,
        )
    }
}

derive_adhoc! {
    ChannelsParams:

    #[allow(dead_code)]
    ${tattrs doc, serde}
    struct ChannelsParamsDupliate {
        $(
            ${fattrs ! serde}
            $fname: $ftype,
        )
    }
}

derive_adhoc! {
    ChannelsParams:
    type Wombat = $ tname;
}

type K = Wombat;

fn main() {
    let _: K = ChannelsParams::default();

    derive_adhoc! {
        ChannelsParams:
        $(
            println!("field name {:?}", stringify!($fname));
        )
    }

    let u = ChannelsParamsUpdates::default();
    println!("updates = {:?}", &u);
}
