### Implementation approach

#### 1. `#[derive(Adhoc)]` proc macro for saving struct definitions

When applied to the `struct Config`, generates this:

```
    macro_rules! derive_adhoc_apply_Config {
        { $($template:tt)* } => {
            derive_adhoc_expand!{
                { pub struct Config { /* original struct definition */ } }
                $($template)*
            }
        }
    }
```

#### 2. `derive_adhoc!` macro for applying to a template

When applied in the example above, generates this:

```
    derive_adhoc_apply_ChannelsParams!{
        pub struct ChannelsParamsUpdates { $( /* etc. */ )* }
    }
```

#### 3. function-like proc macro to do the actual expansion

The result of expanding the above is this:

```
    derive_adhoc_expand!{
        { pub struct Config { /* original struct definition */ } }
        pub struct ChannelsParamsUpdates { $( /* etc. */ )* }
    }
```

`derive_adhoc_expand` parses `pub struct Config`,
and implements a bespoke template expander,
whose template syntax resembles the expansion syntax from `macro_rules`.


Cross-crate API stability
=========================

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
