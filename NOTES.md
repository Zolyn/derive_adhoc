# **Internal notes document (`NOTES.md`)**

## Implementation approach - truly ad-hoc macros

Context: a macro A cannot simply access the expansion of another macro B.
So we must sometimes define macros that apply other macros
(whose name is supplied as an argument)
to what is conceptually their output.

### 1. `#[derive(Adhoc)]` proc macro for saving struct definitions

Implemented in [capture::derive_adhoc_derive_macro].

When applied to (e.g.) `pub struct StructName`, generates this

```rust,ignore
    macro_rules! derive_adhoc_apply_StructName {
        { $($template:tt)* } => {
            derive_adhoc_expand!{
                { pub struct StructName { /* original struct definition */ } }
                $($template)*
            }
        }
    }
```

### 2. `derive_adhoc!` function-like macro for applying to a template

Implemented in [invocation::derive_adhoc_func_macro].

When applied like this
```rust,ignore
    derive_adhoc!{
       StructName:
       TEMPLATE...
    }
```

Expands to
```rust,ignore
    derive_adhoc_apply_StructName! {
       TEMPLATE...
    }
```

### 3. Function-like proc macro to do the actual expansion

Implemented in [expand::derive_adhoc_expand_func_macro].

The result of expanding the above is this:

```rust,ignore
    derive_adhoc_expand!{
        { pub struct StructName { /* original struct definition */ } }
        TEMPLATE...
    }
```

`derive_adhoc_expand` parses `pub struct StructName`,
and implements a bespoke template expander,
whose template syntax resembles the expansion syntax from `macro_rules`.


## Implementation approach - reusable template macros

## 1. `define_derive_adhoc!` macro for defining a reuseable template

Implemented in [definition::define_derive_adhoc_func_macro].

When used like this
```rust,ignore
    define_derive_adhoc! {
        MyMacro =
        TEMPLATE...
    }
```
Expands to
```rust,ignore
    macro_rules! derive_adhoc_call_MyDebug {
        { $dollar : tt $($driver : tt) * } => {
            derive_adhoc_expand! {
                { $($driver)* }
                TEMPLATE...
            }
        }
    }
```

Except, every `$` in the template is replaced with `$dollar`.  This is
because a `macro_rules!` template is not capable of generating a
literal in the expansion `$`.  (With the still-unstable
[`decl_macro`](https://github.com/rust-lang/rust/issues/83527)
feature, `$$` does this.)  So the proc macro code passes in a `$` for
the `macro_rules` macro to emit when it needs to expand to a dollar.

## 2. `#[derive_adhoc(Template)]`, implemented in `#[derive(Adhoc)]`

Template reuse is also implemented in [capture::derive_adhoc_derive_macro].

This

```rust,ignore
    #[derive(Adhoc)]
    #[derive_adhoc(Template)]
    pub struct StructName { ... }
```

Generates (in addition to the `derive_adhoc_apply_StructName` definition)

```rust,ignore
    derive_adhoc_call_Template! {
        $
        #[derive_adhoc(Template)]
        struct StructName { ... }
    }
```

The literal `$` is there to work around a limitation in `macro_rules!`,
see above.

## 3. Actual expansion

The call to `derive_adhoc_call_Template!`
is expanded according to the `macro_rules!` definition,
resulting in a call to `derive_adhoc_expand`.

## Cross-crate API stability

We want people to be able to export derive-adhoc reuseable macros,
and also to export the "struct captures".

This is not so easy because:

 * The template language might evolve, and then we would need to
   make sure that the right expander was used.
   (Possibly newer expanders would support old syntax, but there
   would at least need to be a way to *specify* the desired variant,
   and always using a newer expander might complicate MSRV etc. etc.)

 * The various macros need to find the template expander, and the
   right versions of things.  Rust's rather strange import rules,
   and the interaction of this with strange macro_rules scoping,
   are not helping.

We need to think properly about the following:

 * What about if crates use semver-different versions of derive-adhoc

   The define_derive_adhoc macros embody a template,
   should perhaps embody the derive-adhoc expander major version.

 * We should make semver major breaks the template language infrequently.
   Extensions are OK: if one crate needs the latest then they can all
   have it.

 * Can you derive_adhoc! someone else's struct ?

   The generated capture macro embodies the calling convention
   for the expander but not an expansion syntax.
   And it embodies the struct innards!

 * What about crate renaming?  This is a bit of a mess in Rust.

 * `derive_adhoc_expand!` should perhaps take an argument saying what
   the driver is.  Right now it gets the driver in `{ }` so actually
   wouldn't be ambiguous, and maybe we could just punt.

 * The macro encapsulating the struct body that #[derive(Adhoc)]
   generates ought to work like this
   `define_derive_adhoc!{ [pub] MACNAME = TEMPLATE }`
   and
   <https://gitlab.torproject.org/Diziet/rust-derive-adhoc/-/issues/1>

