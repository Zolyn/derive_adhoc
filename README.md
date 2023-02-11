# derive-adhoc: An ergonomic replacement for (some) proc macros

`derive-adhoc` allows you to write macros which are driven
by Rust data structures, just like proc macro derive macros,
but without having to wrestle with the proc macro system.

**Stability and useability warning**

We do plan to make a `1.x` version of this library,
but right now we are still gaining experience with the template syntax.
So the template syntax is still subject to change.

derive-adhoc has seen limited, if any, real-world use.
So there may well be sharp edges.

The documentation is currently very dry and terse,
and there are few good examples
(mostly, there are just the test cases).

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

## Examples

There are several examples in
[our source repo](https://gitlab.torproject.org/Diziet/rust-derive-adhoc/-/tree/main/tests/expand).
You may want to start with
[`clone.rs`](https://gitlab.torproject.org/Diziet/rust-derive-adhoc/-/blob/main/tests/expand/clone.rs),
and its expansion
[`clone.expanded.rs`](https://gitlab.torproject.org/Diziet/rust-derive-adhoc/-/blob/main/tests/expand/clone.expanded.rs),

### Simple example - providing `Vec` containing enum variant names

```
use derive_adhoc::{define_derive_adhoc, Adhoc};

define_derive_adhoc! {
    ListVariants =

    impl $ttype {
        fn list_variants() -> Vec<&'static str> {
            vec![ $( stringify!( $vname ) , ) ]
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

## Next steps

Why not have a look at our [friendly introduction?](doc_introduction)?

It isn't done yet,
but it tries to be an approachable guide
to what `derive_adhoc` is, and how to use it.
