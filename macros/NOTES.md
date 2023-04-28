# **Notes and plans (`NOTES.md`)**

# Future improvements which are (currently) difficult

(Should this section be tickets instead?)

 * Ideally we would check the case of reuseable derive names.
   These should be in Pascal case.
   But we only want to *lint* for this (and, suppressably), not fail,
   and that's hard.


# Future template features

## Abbreviated syntax for `${paste ...}`

Options include

 * `$<...>` eg `$tkeyword $<$tname Reference><'reference, $tgens>`
 * `$[...]` eg `$tkeyword $[$tname Reference]<'reference, $tgens>`
 * `$[<...>]` eg `$tkeyword $[<$tname Reference>]<'reference, $tgens>`

The `paste` crate uses `[< >]` - but there, `$` is for `macro_rules!`.
I'm pretty sure we don't want to invent expansions that don't start
with `$`, or it will really complicate quoting.

Some discussion so far
<https://gitlab.torproject.org/Diziet/rust-derive-adhoc/-/merge_requests/37#note_2877531>
		
## String concatenation `${string ...}`
		
Argument can contain any expansions (will be expanded into tokens and
stringified), and string literals (in `" "`).  No un-quoted literal
text is allowed.  Expansion is a single string literal.  FTAOD
`${string ${string "y"}}` expands to `r#""y""#`

Do we need this?  Perhaps we should just expect people
to use `stringify!` from `std`, which works well with derive-adhoc.

Would we support case conversion ?


## Splitting off fields and handling subsets of the generics

Syntax and semantics TBD.  Some notes:

```text
   For things that need to split off fields        struct Foo as above {
   and talk only about subsets of the generics         field: Box<T>,
      generic parameter uses (for fields)
      		$fgens					T,
		$fgens_omitted				'l, C
      For explicit iteration, within ${for tgens ...} or $( ... )
		$tgname					'l   	T   	C
		$tgbounds ???

   Something for being able to handle structs/unions/enums
   equally in template, whatever that means.  We need to expand
   something to struct/union/enum, and possibly the brackets and
   commas in enum { ..., ..., }, and so on.
```

## Restricting a macro to only structs (to avoid later consequential errors)

This syntax:

```text
define_derive_adhoc! {
    MyHash for struct =
    //     ^^^^^^^^^^
    impl<$tgens> Hash for $ttype
```

has been suggested to restrict a template to use only on structs
(not enums, etc.)

An alternative would be something like:

```text
define_derive_adhoc! {
    MyHash =
    ${assert is_struct}
    impl<$tgens> Hash for $ttype
```

which would fit into the existing template syntax.

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

 * **`derive_adhoc_driver_DRIVERNAME!`**: `macro_rules` macro generated by `#[derive(Adhoc)]` and `#[item_derive_adhoc]`, embodying a driver.

 * **`derive_adhoc_template_MACNAME!`**: `macro_rules` macro generated by `define_derive_adhoc!`, embodying a template.


# Things to check before declaring 1.0

Ensure we haven't painted ourselves into a corner for adhoc-derive
from a trait, fn, impl block, ... .  And for being able to apply adhoc
derive as an attribute macro, rather than derive macro, thus arranging
that the decorated struct is not actually emitted.

See
[Future plans wrt macro namespace questions](#future-plans-wrt-macro-namespace-questions)
and the feedback request ticket
<https://gitlab.torproject.org/Diziet/rust-derive-adhoc/-/issues/3>
