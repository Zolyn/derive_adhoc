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
    // $[...]                                         ${paste ...}
    // //  Q: should this be $< > ?  That would lead to $<...><...>
    // //  Or to put it another way, which do we prefer?
    // //      $tkeyword $[$tname Reference]<'reference, $tgens> ...
    // //      $tkeyword $<$tname Reference><'reference, $tgens> ...
    // // clonelike.rs has this as [< >] (without the $) but I think
    // // that's wrong - we should introduce all our expansions with $.
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
    $tvis $tkeyword $[$tname Reference]<'reference, $tgens> ${tvariants $(
        ${vdefine $vname $(
            $fvis ${fspec $fname:} &'r $ttype,
        $) }
    ) }

    impl<'r> From<&'r $ttype> for $[$tname Reference]<'reference, $tgens> {
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
