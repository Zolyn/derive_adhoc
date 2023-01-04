# derive_adhoc: An ergonomic replacement for (some) proc macros

**WARNING**

As of 14 July 2022, this is still quite a work-in-progress: you probably
shouldn't use it yet unless you like playing with sharp edges.

`derive-adhoc` allows you to write macros which are driven
by Rust data structures, just like proc macro derive macros,
but without having to wrestle with the proc macro system.

## Overview

You can write an ad-hoc template,
which can speak about the fields and types in the data structure.
You can also define named templates and apply them to multiple structures:
effectively, you can define your own derive macro.

You **don't** need to make a separate proc macro crate,
write to the `syn` and `proc_macro` APIs.
take care to properly propagate compile errors,
or, generally, do any of the things that
make writing proc macros so complicated.

The template language resembles the "expander" part
of a `macro_rules` macro,
but you don't have to write the "matcher" part:
derive-adhoc parses the input data structure for you,
and makes the pieces available via predefined expansion variables.

Further documentation is available in the `doc_` module(s)
and the docs for the individual proc macros.

## Example

```
use derive_adhoc::{define_derive_adhoc, Adhoc};

define_derive_adhoc! {
    ListVariants =

    impl $ttype {
        fn list_variants() -> Vec<&'static str> {
            vec! [ $( stringify!( $vname ) , ) ]
        }
    }
}

#[derive(Adhoc)]
#[derive_adhoc(ListVariants)]
enum Enum {
    UnitVariant,
    StructVariant { a: u8, b: u16 },
    TupleVariant(u8, u16),
}

assert_eq!(
    Enum::list_variants(),
    ["UnitVariant", "StructVariant", "TupleVariant"],
);
```
