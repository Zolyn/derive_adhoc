### Background

Rust has two primary macro systems: `macro_rules`, which is easy to use, and proc macros, which are powerful: in particular, they can `derive`: autogenerate code from data structure definitions.

### Project goal and outline

This project will provide a facility which can be used to easily autogenerate code, ad-hoc, based on data structures, without having to write proc macro code (which is hard, and unsuitable for one-off use cases) and without having to try to write macro_rules pattern matchers for (sort-of-) struct definitions.

The project proposer has a plan for how to achieve this.

There are motivating use cases in the Arti code base, such as:

 * The [channel operational parameters](https://gitlab.torproject.org/tpo/core/arti/-/blob/main/crates/tor-proto/src/channel/params.rs)
   struct is wrapped up in a macro call
   so that a parameters update struct, and support code,
   can be automatically generated.

 * Where Arti's configuration contains lists of things,
   we use [macros to generate the types and accessors](https://gitlab.torproject.org/tpo/core/arti/-/blob/main/crates/tor-config/src/list_builder.rs),
   but the arrangements require us to
   [recapitulate the struct field definitions](https://gitlab.torproject.org/tpo/core/arti/-/blob/main/crates/arti/src/logging.rs#L68).

### Skills and resources

Collaborators should have prior experience of Rust and a Rust development environment.

We will be writing proc macro code, but you can learn that on the job :-).
Indeed, this might be a fun opportunity to play with proc macros.

### Example of what using this facility might look like:

```
    #[derive(Adhoc)]
    pub struct Config {
        enabled: bool,
        padding: PaddingParameters,
    }

    #[derive_adhoc(ChannelsParams)]
    pub struct ChannelsParamsUpdates {
        $(
            pub(crate) $field: Option<$ty>,
        )*
    }
```

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

#### 2. `#[derive_adhoc]` attribute macro for applying to a template

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