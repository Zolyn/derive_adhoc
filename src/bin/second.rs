
use derive_adhoc_macros::derive_adhoc_expand;

macro_rules! derive_adhoc_apply_ChannelsParams {
 { $($template:tt)* } => {
   derive_adhoc_expand!{ 
     {

#[derive(Debug, Educe, Clone, Eq, PartialEq)]
#[educe(Default)]
pub struct ChannelsParams {
    #[field educe(Default(expression = "interim_enable_by_env_var()"))]
    padding_enable: bool,

    padding_parameters: padding::Parameters
}

     }
     $($template)*
   }
 }
}

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


#[test]
fn second(){
}
