//! THIS IS A NON-WORKING CONCEPT, FOR FUTURE DEVELOPMENT

// Example which derives a new type containing references
//
// This demonstrates how to make a new type which mirrors the driver type,
// including both structs and enums, with units, tuples or structs.
//
// It also demonstrates how to construct an enum using $vconstr,
// and handling of visibility attributes.

define_derive_adhoc!{
    ReferenceVersion =

    // New expansions:
    //
    // $tkeyword                                      struct / enum / union
    //   TODO should this be $tkwd?
    //   We abbreviate many other things but this one is going to be
    //   more rarely used.
    //   https://gitlab.torproject.org/Diziet/rust-derive-adhoc/-/merge_requests/37#note_2877533
    //
    // $tvis                                          toplevel visibility
    // $fvis                    for enum              always nothing
    // $fvis                    otherwise             field visibility
    // // for effective vis., write ${if is_enum { $tvis } else { $fvis }}
    //
    // ${tdefvariants BLAH}        for enum              { BLAH }
    // ${tdefvariants BLAH}        otherwise               BLAH
    //
    // ${vdefbody VANME BLAH)}   for unit              nothing
    // ${vdefbody VANME BLAH)}   for tuple             ( BLAH )
    // ${vdefbody VANME BLAH)}   for struct            { BLAH }
    // ${vdefbody VANME BLAH)}   for unit variant      VNAME
    // ${vdefbody VANME BLAH)}   for tuple variant     VNAME ( BLAH )
    // ${vdefbody VANME BLAH)}   for struct variant    VNAME { BLAH }
    //    * BLAH will pretty much always have to be $( ... )
    //      since it will want to enumerate the fields.
    //    * ${vdefbody} cannot be emulated with ${if } without
    //      recapitulating the contents.  An alternative would be
    //        ${apply_possible_wrapping {WRAPPING} {CONTENTS}}
    //        ${apply_possible_wrapping {} X} => X
    //        ${apply_possible_wrapping {{}} X} => {X}
    //      omg wtf hope we don't have to go there?
    //      Or a local sub-macro feature
    //        ${define_subtemplate NAME EXPANSION}
    //        ${expand_subtemplate NAME}
    //
    // ${fdefine BLAH}            in unit [variant]     cannot occur
    // ${fdefine BLAH}            in tuple [variant]    nothing
    // ${fdefine BLAH}            in struct [variant]   BLAH:
    //    $fdefine can be emulated with
    //    ${if v_is_named {$fname:}}
    // which implies boolean tests:
    //    v_is_unit   // only unit syntax, not all with 0 fields, is this right
    //    v_is_tuple
    //    v_is_named  // named fields struct, "struct" variant ?
    // Or possibly like this
    //    vstyle_unit
    //    vstyle_tuple
    //    vstyle_struct
    // ?
    //
    // ${Xdefine BLAH} expands to either nothing, or BLAH-plus-framing
    $tvis $tkeyword ${paste $tname Reference}<'reference, $tgens>
    ${tdefvariants $(
    // Or maybe:
    ${t_body_define_variants $(
        ${vdefbody $vname $(
            $fvis ${fdefine $fname } &'r $ttype,
            // Tentatively rejected alternatives
            $fvis ${fdefine $fname:} &'r $ttype,
            $fvis ${fdefine $fname:  &'r $ttype},
        $) }
    ) }

    impl<'r> From<&'r $ttype>
    for ${paste $tname Reference}<'reference, $tgens> {
        fn from(ref_to_owned: &'r $ttype) -> Self {
            match ref_to_owned { $(
                $vpat => $vconstr { $(
                // Tentatively rejected alternatives
                // (see vpat in partial-ord.rs)
                $vpat => Self ${vspec $vname} { $(
                $vpat => Self $[:: $vname] { $(
                $vpat => Self ${if is_enum {:: $vname}} { $(
                    $fname: &ref_to_owned.$fpatname,
                ) }
            ) }
        }
    }

    impl<'r> ${paste $tname Reference}<'reference, $tgens> 
    where $( $ftype: Clone, )
    {
        fn cloned(&self) -> $ttype {
            match self { $(
                ${vpat self=Self} => $vconstr { $(
                    $fname: self.$fpatname.clone(),
                ) }
            ) }
        }
    }
}

//   We can't do this for a Unit because it would end up with
//   an unused lifetime.
// #[derive(Adhoc)]
// #[derive_adhoc(ReferenceVersion)]
// struct Unit;

#[derive(Adhoc)]
#[derive_adhoc(ReferenceVersion)]
struct Tuple<F>(F)

#[derive(Adhoc)]
#[derive_adhoc(ReferenceVersion)]
struct Struct<F> {
    field: F,
}

#[derive(Adhoc)]
#[derive_adhoc(ReferenceVersion)]
enum Enum<F> {
    Unit,
    Tuple(F),
    Struct { field: F },
}

fn main() {
}
