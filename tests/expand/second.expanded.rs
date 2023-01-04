use derive_adhoc::{derive_adhoc, Adhoc};
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
#[automatically_derived]
impl ::core::default::Default for ChannelsParams {
    #[inline]
    fn default() -> ChannelsParams {
        ChannelsParams {
            padding_enable: ::core::default::Default::default(),
            padding_parameters: ::core::default::Default::default(),
        }
    }
}
pub struct ChannelsParamsUpdates {
    ///
    /// New value, if it has changed.
    pub(crate) padding_enable: Option<bool>,
    ///
    /// New value, if it has changed.
    pub(crate) padding_parameters: Option<usize>,
}
#[automatically_derived]
impl ::core::fmt::Debug for ChannelsParamsUpdates {
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_struct_field2_finish(
            f,
            "ChannelsParamsUpdates",
            "padding_enable",
            &&self.padding_enable,
            "padding_parameters",
            &&self.padding_parameters,
        )
    }
}
#[automatically_derived]
impl ::core::default::Default for ChannelsParamsUpdates {
    #[inline]
    fn default() -> ChannelsParamsUpdates {
        ChannelsParamsUpdates {
            padding_enable: ::core::default::Default::default(),
            padding_parameters: ::core::default::Default::default(),
        }
    }
}
#[automatically_derived]
impl ::core::clone::Clone for ChannelsParamsUpdates {
    #[inline]
    fn clone(&self) -> ChannelsParamsUpdates {
        ChannelsParamsUpdates {
            padding_enable: ::core::clone::Clone::clone(&self.padding_enable),
            padding_parameters: ::core::clone::Clone::clone(&self.padding_parameters),
        }
    }
}
#[automatically_derived]
impl ::core::marker::StructuralEq for ChannelsParamsUpdates {}
#[automatically_derived]
impl ::core::cmp::Eq for ChannelsParamsUpdates {
    #[inline]
    #[doc(hidden)]
    #[no_coverage]
    fn assert_receiver_is_total_eq(&self) -> () {
        let _: ::core::cmp::AssertParamIsEq<Option<bool>>;
        let _: ::core::cmp::AssertParamIsEq<Option<usize>>;
    }
}
#[automatically_derived]
impl ::core::marker::StructuralPartialEq for ChannelsParamsUpdates {}
#[automatically_derived]
impl ::core::cmp::PartialEq for ChannelsParamsUpdates {
    #[inline]
    fn eq(&self, other: &ChannelsParamsUpdates) -> bool {
        self.padding_enable == other.padding_enable
            && self.padding_parameters == other.padding_parameters
    }
}
#[allow(dead_code)]
/// Some docs
struct ChannelsParamsDupliate {
    /// thing
    ///
    /// paragraph
    #[allow(dead_code)]
    padding_enable: bool,
    #[allow(dead_code)]
    padding_parameters: usize,
}
type Wombat = ChannelsParams;
type K = Wombat;
fn main() {
    let _: K = ChannelsParams::default();
    {
        ::std::io::_print(
            ::core::fmt::Arguments::new_v1(
                &["field name ", "\n"],
                &[::core::fmt::ArgumentV1::new_debug(&"padding_enable")],
            ),
        );
    };
    {
        ::std::io::_print(
            ::core::fmt::Arguments::new_v1(
                &["field name ", "\n"],
                &[::core::fmt::ArgumentV1::new_debug(&"padding_parameters")],
            ),
        );
    };
    let u = ChannelsParamsUpdates::default();
    {
        ::std::io::_print(
            ::core::fmt::Arguments::new_v1(
                &["updates = ", "\n"],
                &[::core::fmt::ArgumentV1::new_debug(&&u)],
            ),
        );
    };
}
