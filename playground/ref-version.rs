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
    // ${tvariants BLAH}        for enum              { BLAH }
    // ${tvariants BLAH}        otherwise               BLAH
    //
    // ${vdefine VANME BLAH)}   for unit              nothing
    // ${vdefine VANME BLAH)}   for tuple             ( BLAH )
    // ${vdefine VANME BLAH)}   for struct            { BLAH }
    // ${vdefine VANME BLAH)}   for unit variant      VNAME
    // ${vdefine VANME BLAH)}   for tuple variant     VNAME ( BLAH )
    // ${vdefine VANME BLAH)}   for struct variant    VNAME { BLAH }
    //
    // ${fspec BLAH}            in unit [variant]     cannot occur
    // ${fspec BLAH}            in tuple [variant]    nothing
    // ${fspec BLAH}            in struct [variant]   BLAH
    $tvis $tkeyword ${paste $tname Reference}<'reference, $tgens>
    ${tvariants $(
    // Or maybe:
    ${t_body_define_variants $(
        ${vdefine $vname $(
            // One of these, but which?
            $fvis ${fspec $fname:} &'r $ttype,
            $fvis ${fdefine $fname: &'r $ttype},
        $) }
    ) }

    impl<'r> From<&'r $ttype>
    for ${paste $tname Reference}<'reference, $tgens> {
        fn from(ref_to_owned: &'r $ttype) -> Self {
            match ref_to_owned { $(
                $vpat => $vconstr { $(
                    $fname: &ref_to_owned.$fpatname,
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
