
macro_rules! derive_adhoc_apply_ChannelsParams {
 { $($template:tt)* } => {
   derive_adhoc_expand!{ 
     {
         #[derive(Debug, Educe, Clone, Eq, PartialEq)]
         #[educe(Default)]
          pub struct ChannelsParams {
            $(
              $( #[doc $($doc_attr)*] )*
              $( #[$other_attr] )*
              pub(crate) $field: $ty,
            )*
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
    }
}


#[test]
fn second(){
}
