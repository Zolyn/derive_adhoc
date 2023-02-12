//! THIS IS A NON-WORKING CONCEPT, FOR FUTURE DEVELOPMENT

// Example which fully and precisely derives Clone
//
// This gives a basic demonstration of how to handle an enum.

define_derive_adhoc!{
    PreciseClone =

    // New expansions:
    //
    // $vpat      for struct    $tname         { $( $fname: $fpatname ) }
    // $vpat      for enum      $tname::$vname { $( $fname: $fpatname ) }
    // $vconstr                 like $vpat, but with <$tgnames>
    //
    // $fpatname                ${paste f_ $fname}
    // // This is what hash2.rs calls ${pfname}.
    // // I think it must start with $f... not $p... since it is per-field.
    // // It needs to expand to a prefix so we can bind to variables
    // // which won't clash with fixed locals / other parameters.
    impl Clone for $ttype
    // We don't need to $( $( ) ) (ie, twice); it automatically descends.
    where $( $ftype: Clone, )
          $twheres
    {
        fn clone(&self) -> Self {
            match self { $(
                $vpat => $vconstr { $(
                    $fname: self.$fpatname.clone(),
                ) }
            ) }
        }
    }
}

#[derive(Adhoc)]
#[derive_adhoc(PreciseClone)]
struct Unit;

#[derive(Adhoc)]
#[derive_adhoc(PreciseClone)]
struct Tuple<F>(F)

#[derive(Adhoc)]
#[derive_adhoc(PreciseClone)]
struct Struct<F> {
    field: F,
}

#[derive(Adhoc)]
#[derive_adhoc(PreciseClone)]
enum Enum<F> {
    Unit,
    Tuple(F),
    Struct { field: F },
}

fn test<T: Clone>(value: &T) {
    let ours = value.clone();
    drop(ours);
}

fn main() {
    test(Unit);
    test(Tuple(String::new()));
    test(Struct { field: 42 });
    test(Enum::Unit);
    test(Enum::Tuple(String::new()));
    test(Enum::Struct { field: 66 });
}