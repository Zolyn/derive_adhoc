//! THIS IS A NON-WORKING CONCEPT, FOR FUTURE DEVELOPMENT

// This is a fanciful example of future possibilities!
//
// Example that makes a self-cross-product out of an enum,
// (We only support unit enums without generics, for clarity.)
//
// This demonstrates use of the outer context facility.

define_derive_adhoc!{
    Squared =

    // New expansions:
    //     ${outer_variant ...}
    //     ${outer_field ...}
    // Expands ... in the context of an outer iteration.
    // Can be nested to get an outer outer context.
    $tvis enum $[$ttype Squared] {
        ${for variants {
            ${for variants {
                $[ ${outer_variant $vname} $vname ],
            }}
        }}
    }
}

#[derive(Adhoc)]
#[derive_adhoc(Squared)]
enum Greek { 
    Alpha,
    Beta,
}

// Expands to:

enum GreekSquared {
    AlphaAlpha,
    AlphaBeta,
    BetaAlpha,,
    BetaBeta,
}

fn main(){
}
