# Implementation approach - how does this work?

**You do not need to understand this in order to use derive-adhoc.**

Also, you should not rely on the details here.
They don't form part of the public interface.

## Introduction

It is widely understood that proc macro invocations
cannot communicate with each other.
(Some people have tried sneaking round the back with disk files etc.
but this can break due to incremental and concurrent compilation.)

But, a proc macro can *define a `macro_rules` macro*.
Then, later proc macros can invoke that macro.
The expansion can even invoke further macros.
In this way, a proc macro invocation *can* communicate with
subsequent proc macro invocations.
(This trick is also used by
 [`ambassador`](https://crates.io/crates/ambassador).)

There is a further complication.
One macro X cannot peer into and dismantle
the expansion of another macro Y.
(This is necessary for soundness, when macros can generate `unsafe`.)
So we must sometimes define macros that apply other macros
(whose name is supplied as an argument)
to what is conceptually the first macro's output.

## Implementation approach - truly ad-hoc macros

### 1. `#[derive(Adhoc)]` proc macro for saving struct definitions

Implemented in `capture.rs::derive_adhoc_derive_macro`.

When applied to (e.g.) `pub struct StructName`, generates this

```rust,ignore
    macro_rules! derive_adhoc_driver_StructName { {
        { $($template:tt)* }
        { ($ORGDOLLAR:tt) $(future:tt)* }
        $($tpassthrough:tt)* 
     } => {
        derive_adhoc_expand!{
            { pub struct StructName { /* original struct definition */ } }
            { }
            { $($template)* }
            { $($tpassthrough)* }
        }
    } }
```

(The extra `{ }` parts after the driver and template
include space for future expansion.)

In the `pub struct` part every `$` is replaced with `$ORGDOLLAR`,
to use the `$` passed in at the invocation site.

### 2. `derive_adhoc!` function-like proc macro for applying to a template

Implemented in `invocation.rs::derive_adhoc_func_macro`.

When applied like this
```rust,ignore
    derive_adhoc!{
       StructName TOPTIONS...:
       TEMPLATE...
    }
```

Expands to
```rust,ignore
    derive_adhoc_driver_StructName! {
       { TEMPLATE... }
       { ($) }
       crate; (TOPTIONS...)
    }
```

### 3. Function-like proc macro to do the actual expansion

Implemented in `expand.rs::derive_adhoc_expand_func_macro`.

The result of expanding the above is this:

```rust,ignore
    derive_adhoc_expand!{
        { pub struct StructName { /* original struct definition */ } }
        { }
        { TEMPLATE... }
        { crate; (TOPTIONS...) }
    }
```

`derive_adhoc_expand` parses `pub struct StructName`,
and implements a bespoke template expander,
whose template syntax resembles the expansion syntax from `macro_rules`.

`crate` is just used as the expansion for `${crate}`.
(For an ad-hoc template, the local crate is correct.)

## Implementation approach - reusable template macros

## 1. `define_derive_adhoc!` macro for defining a reuseable template

Implemented in `definition.rs::define_derive_adhoc_func_macro`.

When used like this
```rust,ignore
    define_derive_adhoc! {
        MyMacro TOPTIONS... =
        TEMPLATE...
    }
```
Expands to
```rust,ignore
    macro_rules! derive_adhoc_template_MyDebug { {
        { $($driver:tt)* }
        { $($future:tt)* }
        $($dpassthrough:tt)*
    } => {
        derive_adhoc_expand! {
            { $($driver)* }
            { $($dpassthrough)* }
            { TEMPLATE... }
            { $crate; (TOPTIONS...) }
        }
    } }
```

Except, every `$` in the TEMPLATE is replaced with `$ORIGDOLLAR`.
This is because a `macro_rules!` template
is not capable of generating a
literal in the expansion `$`.
(With the still-unstable
[`decl_macro`](https://github.com/rust-lang/rust/issues/83527)
feature, `$$` does this.)
The template expansion engine treats `$ORGDOLLAR` as just a single `$`.

(Again, the extra `{ }` parts after the driver and template
include space for future expansion.)

## 2. `#[derive_adhoc(Template)]`, implemented in `#[derive(Adhoc)]`

Template reuse is also implemented in `capture.rs::derive_adhoc_derive_macro`.

This

```rust,ignore
    #[derive(Adhoc)]
    #[derive_adhoc(Template)]
    pub struct StructName { ... }
```

Generates (in addition to the `derive_adhoc_driver_StructName` definition)

```rust,ignore
    derive_adhoc_template_Template! {
        { #[derive_adhoc(Template)] struct StructName { ... } }
        { }
    }
```

The literal `$` is there to work around a limitation in `macro_rules!`,
see above.

## 3. Actual expansion

The call to `derive_adhoc_template_Template!`
is expanded according to the `macro_rules!` definition,
resulting in a call to `derive_adhoc_expand`.
