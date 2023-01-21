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


# Future template features

## Tuple vs unit vs struct (variant) agility

Precise design TBD.

`${vconstructor}` or `${vbody}` should perhaps not implicity
establish iteration over fields, so that other fields can be added?
(Would break with unit structs/variants.)

Expansion prefix char `v` may come to mean both `value` and `variant`.

 * Full constructor with delimiters
```text
        ${vconstructor ... }    Struct { ... }  struct [variant] or union
                                Struct( ... )   tuple struct/variant
                                Struct          unit struct/variant
        // What if the struct name is a path?  Do we need expansions
        // which just produce the name, or just the full path, or what?
```

 * Variant (or value) body and delimiter
```text
        ${vbody ... }           { ... }         struct [variant] or union
                                ( ... )         tuple struct/variant
                                nothing         unit struct/variant
```

  * Field specifier
```text

        $fspec                  $fname :        in struct [variant] or union
                                nothing         in tuple variant/struct
```


## String concatenation `${string ...}`
		
Argument can contain any expansions (will be expanded into tokens and
stringified), and string literals (in `" "`).  No un-quoted literal
text is allowed.  Expansion is a single string literal.  FTAOD
`${string ${string "y"}}` expands to `r#""y""#`

Do we need this?  Perhaps we should just expect people
to use `stringify!` from `std`, which works well with derive-adhoc.


# Future plans wrt macro namespace questions

## Deriving from things other than data structures

It would be nice to be able to eventually support deriving from
items (traits, fns, ...).  This would have to be an attribute proc macro.  Attribute proc macros get to modify their applicand, but we would not do that.

Ideally that attribute macro would be `#[derive_adhoc]`.  However:

 * We are already using that as an inert helper attribute for `#[derive(Adhoc)]`.  Possibly we could experiment to see how that would interact with a non-inert attribute macro, except that:

 * It is not possible to have an attribute macro and a function-like macro with the same name; even though the invocation syntaxes (and implementing macro function signatures) are different.

We are using `derive_adhoc!` for the main truly adhoc from-struct derivation.  I don't think we want to change that.  So we need a different name for the attribute macro.

## Proposed taxonomy of macros and attributes

We won't implement all of this right away,
but it is good to have a plan to make sure the names we take now
won't get in the way.

 * **`#[derive(Adhoc)]`**: invokes the from-struct derivation machinery; enables:
    1. use of `#[derive_adhoc(ReuseableMacro)]` on this very struct
    2. later use of `derive_adhoc!` of the same struct
    3. `#[adhoc(...)]` attributes on bits of the data structure

 * **`define_derive_adhoc!{ [pub] MACNAME = TEMPLATE }`**: define a reusable template, which may be invoked as `#[derive_adhoc(MACNAME)]` (within a struct annotated with `#[derive(Adhoc)]` or `#[item_derive_adhoc(MACNAME)]`.

 * **`derive_adhoc!{ DRIVERNAME: TEMPLATE }`**: truly-adhoc derivation from something previously annotated with `#[derive(Ahoc)]` or `#[item_derive_adhoc]`.  `DRIVERNAME` is an item path; we conflate the type and value namespaces.

 * **`#[item_derive_adhoc]`**: attribute macro to be applied to items.  The item is reproduced unchanged, except that `#[adhoc]` attributes *in places where we would look for them* are filtered out. `#[item_derive_adhoc(MACNAME)]` can be used to apply a reuseable template.

 * **`#[adhoc]`**: Inert helper attribute for `#[derive(Adhoc)]`.  Filtered-out attribute for `#[item_derive_adhoc]`.  Contents available via `$Xmeta`.

 * **`#[only_derive_adhoc]`**: attribute macro to be applied to items; like `#[item_derive_adhoc]` but *consumes and does not emit* the item.
   (We don't really need to be sure about this name; this will be an unusual case and we can give it whatever name seems good, later.)

### Internals

 * **`derive_adhoc_expand!`**: Proc macro that does all the work.

 * **`derive_adhoc_apply_DRIVERNAME!`**: `macro_rules` macro generated by `#[derive(Adhoc)]` and `#[item_derive_adhoc]`, embodying a driver.

 * **`derive_adhoc_call_MACNAME!`**: `macro_rules` macro generated by `define_derive_adhoc!`, embodying a template.


# Things to check before declaring 1.0

Ensure we haven't painted ourselves into a corner for adhoc-derive
from a trait, fn, impl block, ... .  And for being able to apply adhoc
derive as an attribute macro, rather than derive macro, thus arranging
that the decorated struct is not actually emitted.

See
[Future plans wrt macro namespace questions](#future-plans-wrt-macro-namespace-questions)
and the feedback request ticket
<https://gitlab.torproject.org/Diziet/rust-derive-adhoc/-/issues/3>
