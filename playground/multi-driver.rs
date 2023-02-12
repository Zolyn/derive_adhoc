//! THIS IS A NON-WORKING CONCEPT, FOR FUTURE DEVELOPMENT

// This is a fanciful example of future possibilities!
//
// Example that makes a union of any number of enums,
// (We only support unit enums without generics, for clarity.)
//
// This demonstrates multiple drivers, with a template that
// treats all drivers equivalent.y
//
// Before pursuing this we should also
//   - demonstrate a template that expects a fixed number of
//     drivers and treats them differently
//   - consider how to invoke a precanned template on multiple drivers
//   - figure out how even any of this will work underneath

#[derive(Adhoc)]
enum Greek { 
    Alpha,
    Beta,
}

#[derive(Adhoc)]
enum Colour {
    Red,
    Blue,
}

derive_adhoc!{
    Greek, Colour:

    enum $[ ${for_inputs $tname} ] {
        ${for_inputs {
            $vname,
        }}
    }}
}

// Expands to:

enum GreekColour {
    Alpha,
    Beta,
    Red,
    Blue,
}
