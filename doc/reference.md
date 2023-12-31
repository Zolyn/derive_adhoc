# **Template syntax (and expansion options) reference**

**Table of contents**

<!--##toc##-->
   * [Template syntax overview](#template-syntax-overview)
      * [Named and positional template arguments to expansions and conditions](#named-and-positional-template-arguments-to-expansions-and-conditions)
   * [Repetition and nesting](#repetition-and-nesting)
   * [Expansions](#expansions)
      * [`$fname`, `$vname`, `$tname` -- names](#fname-vname-tname--names)
      * [`$fvis`, `$tvis`, `$fdefvis` -- visibility](#fvis-tvis-fdefvis--visibility)
      * [`$vpat`, `$fpatname` -- pattern matching and value deconstruction](#vpat-fpatname--pattern-matching-and-value-deconstruction)
      * [`$ftype`, `$vtype`, `$ttype`, `$tdeftype` -- types](#ftype-vtype-ttype-tdeftype--types)
      * [`$tgens`, `$tgnames`, `$twheres`, `$tdefgens` -- generics](#tgens-tgnames-twheres-tdefgens--generics)
      * [`${tmeta(...)}` `${vmeta(...)}` `${fmeta(...)}` -- `#[adhoc]` attributes](#tmeta-vmeta-fmeta--adhoc-attributes)
      * [`${fattrs ...}` `${vattrs ...}` `${tattrs ...}` -- other attributes](#fattrs--vattrs--tattrs---other-attributes)
      * [`$<...>`, `${paste ...}` -- identifier pasting](#-paste---identifier-pasting)
      * [`${CASE_CHANGE ...}` -- case changing](#case_change---case-changing)
      * [`${when CONDITION}` -- filtering out repetitions by a predicate](#when-condition--filtering-out-repetitions-by-a-predicate)
      * [`${if COND1 { ... } else if COND2 { ... } else { ... }}` -- conditional](#if-cond1----else-if-cond2----else-----conditional)
      * [`${select1 COND1 { ... } else if COND2 { ... } else { ... }}` -- expect precisely one predicate](#select1-cond1----else-if-cond2----else-----expect-precisely-one-predicate)
      * [`${for fields { ... }}`, `${for variants { ... }}`, `$( )` -- repetition](#for-fields----for-variants-------repetition)
      * [`$crate` -- root of template crate](#crate--root-of-template-crate)
      * [`$tdefkwd` -- keyword introducing the new data structure](#tdefkwd--keyword-introducing-the-new-data-structure)
      * [`$tdefvariants`, `$vdefbody`, `$fdefine` -- tools for defining types](#tdefvariants-vdefbody-fdefine--tools-for-defining-types)
      * [`$dbg_all_keywords` -- Dump expansions of all keywords to compiler stderr](#dbg_all_keywords--dump-expansions-of-all-keywords-to-compiler-stderr)
      * [`${define ...}`, `${defcond ...}` -- user-defined expansions and conditions](#define--defcond---user-defined-expansions-and-conditions)
   * [Conditions](#conditions)
      * [`fvis`, `tvis`, `fdefvis` -- test for public visibility](#fvis-tvis-fdefvis--test-for-public-visibility)
      * [`fmeta(NAME)`, `vmeta(NAME)`, `tmeta(NAME)` -- `#[adhoc]` attributes](#fmetaname-vmetaname-tmetaname--adhoc-attributes)
      * [`is_struct`, `is_enum`, `is_union`](#is_struct-is_enum-is_union)
      * [`v_is_unit`, `v_is_tuple`, `v_is_named`](#v_is_unit-v_is_tuple-v_is_named)
      * [`approx_equal(ARG1, ARG2)` -- equality comparison (token comparison)](#approx_equalarg1-arg2--equality-comparison-token-comparison)
      * [`false`, `true`, `not(CONDITION)`, `any(COND1,COND2,...)`, `all(COND1,COND2,...)` -- boolean logic](#false-true-notcondition-anycond1cond2-allcond1cond2--boolean-logic)
   * [Case changing](#case-changing)
   * [Expansion options](#expansion-options)
      * [`expect items`, `expect expr` -- syntax check the expansion](#expect-items-expect-expr--syntax-check-the-expansion)
      * [`for struct`, `for enum`, `for union` -- Insist on a particular driver kind](#for-struct-for-enum-for-union--insist-on-a-particular-driver-kind)
      * [`dbg` -- Print the expansion to stderr, for debugging](#dbg--print-the-expansion-to-stderr-for-debugging)
      * [Expansion options example](#expansion-options-example)
   * [Structs used in examples](#structs-used-in-examples)

**Reference documentation for the actual proc macros** is in
the [crate-level docs for derive-adhoc](../index.html#macros).

## Template syntax overview

Within the macro template,
expansions (and control structures) are introduced with `$`.
They generally refer to properties of the data structure that
we're deriving from.
We call that data structure the **driver**.

In general the syntax is:

 * `$KEYWORD`: Invoke the expansion of the keyword `KEYWORD`.
 * `${KEYWORD ARGS...}`: Invoke with parameters.
 * `$( .... )`: Repetition (abbreviated, automatic, form).
   (Note: there is no `+` or `*` after the `)`)
 * `$< .... >`: Identifier pasting (shorthand for
   [`${paste ...}`](#-paste---identifier-pasting)).

In all cases, `$KEYWORD` is equivalent to `${KEYWORD}`.
You can pass a `$` through
(e.g. if you want to confuse yourself
by making derive-adhoc-generated pattern macros)
by writing `$$`.

Many of the expansion keywords start with `f`, `v`, or `s` to indicate
the depth of the thing being expanded:

 * `f...`: Expand something belonging to a particular Field.

 * `v...`: Expand something belonging to a particular Variant.

 * `t...`: Expand something applying to the whole Top-level type.

In the keyword descriptions below,
`X` is used to stand in for one of `f`, `v` or `t`.

Defining a new type based on the driver
requires more complex and subtle syntax,
generated by special-purpose expansions `$Xdef...`.

(Here, within this documentation,
we often write in `CAPITALS` to indicate meta-meta-syntactic elements,
since all of the punctuation is already taken.)

Inner attributes (`#![...]` and `//!...`)
are not allowed in templates.

### Named and positional template arguments to expansions and conditions

Some expansions and conditions take
(possibly optional)
named arguments,
or multiple positional arguments,
whose values are templates:

 * `${KEYWORD NAME=ARG NAME=ARG ...}`
 * `${KEYWORD ARG1 ARG2 ...}`
 * `CONDITION(NAME=ARG, NAME=ARG, ...)`
 * `CONDITION(ARG1, ARG2, ...)`

The acceptable contents vary,
but the syntax is always the same.
Each `ARG` must be one of:

 * `IDENTIFIER`
 * `LITERAL` (eg, `NUMBER` or `"STRING"`)
 * `$EXPANSION` or `${EXPANSION}` or `${EXPANSION...}` or `$<...>`
 * `{ STUFF }`, where `STUFF` is expanded.
   (The `{ }` just for delimiting the value, and are discarded).

## Repetition and nesting

The driving data structure can contain multiple variants,
which can in turn contain multiple fields;
there are also attributes.

Correspondingly,
sections of the template, indicated by `${for ...}` and `$( ...)`,
are expanded multiple times.

With `${for ...}`, what is iterated over is specified explicitly.

When `$( ... )` is used, what is iterated over is automatically
inferred from the content:
most expansions and conditions imply a "level":
what possibly-repeated part of the driver they correspond to.
All the expansions directly within `$(...)`
must have the same repetition level.

With both `${for }` and `$( ... )`,
if the repetition level is "deeper" than the level
of the surrounding template,
the surrounding levels are also repeated over,
effectively "flattening".
For example, expanding `$( $fname )` at the very toplevel,
will iterate over all of the field names;
if the driver is an enum;
it will iterate over all of the fields in each of the variants
in turn.

structs and unions do not have variants, but
derive-adhoc treats them as having a single (unnamed) variant.

#### Examples

<!--##examples-for `Enum`##-->
<!--##examples-ignore##-->
For [example enum `Enum`](#structs-used-in-examples):

 * `$($vname,)`: `UnitVariant, TupleVariant, NamedVariant,`
 * `$($fname)`: `0 field field_b field_e`
 * `${for fields { hello }}`: `hello hello hello hello`

## Expansions

Each expansion keyword is described in this section.
The examples each show the expansions for (elements of)
the same example `Unit`, `Tuple`, `Struct` and `Enum`,
shown below.

<!-- ## maint/check-keywords-documented expansions ## -->

### `$fname`, `$vname`, `$tname` -- names

The name of the field, variant, or toplevel type.
This is an the identifier (without any path or generics).
For tuple fields, `$fname` is the field number.

`$fname` is not suitable for direct use as a local variable name.
It might clash with other local variables;
and, unlike most other expansions,
`$fname` has the hygiene span of the driver field name.
Instead, use `$vpat`, `$fpatname`,
or `${paste ... $fname ...}` (`$<... $fname ...>`).

#### Examples

 * `$fname`: `0`, `field`, `field_b`
 * `$vname`: `UnitVariant`
 * `$tname`: `Tuple`, `Struct`, `Enum`

### `$fvis`, `$tvis`, `$fdefvis` -- visibility

The visibility of the field, or toplevel type.

Expands to `pub`, `pub(crate)`, etc.
Expands to nothing for private types or fields.

This looks only at the syntax in the driver definition;
an item which is `pub` might still not be reachable,
for example if it is in a private inner module.

#### Enums and visibility

In Rust,
enum variants and fields don't have separate visibility;
they inherit visibility from the enum itself.
So there is no `$vvis`.

For enum fields, `$fvis` expands to the same as `$tvis`.
Use `$fvis` for the effective visibility of a field,
eg when defining a derived method.

`$fdefvis` is precisely what was written in the driver field definition,
so always expands to nothing for enum fields -
even though those might be public.
Use `$deffvis` when defining a new enum.

#### Examples

 * `$tvis` for `Unit`: `pub`
 * `$tvis` for `Enum`: `pub`
 * `$tvis` for others: nothing
 * `$fvis` for `field` in `Struct`: `pub`
 * `$fvis` for `field_b` in `Struct`: `pub(crate)`
 * `$fvis` for fields in `Enum`: `pub`
 * `$fvis` for others: nothing
 * `$fdefvis` for `field` in `Struct`: `pub`
 * `$fdefvis` for `field_b` in `Struct`: `pub(crate)`
 * `$fdefvis` for fields in `Enum`: nothing
 * `$fdefvis` for others: nothing

### `$vpat`, `$fpatname` -- pattern matching and value deconstruction

`$vpat` expands to a pattern
suitable for matching a value of the top-level type.
It expands to `TYPE { FIELD: f_FNAME, ... }`,
where `TYPE` names the top-level type or enum variant.
(`TYPE` doesn't have generics,
since those are not allowed in patterns.)

Each field is bound to a local variant `f_FNAME`,
where `FNAME` is the actual field name (or tuple field number).

`$fpatname` expands to `f_FNAME` for the current field.

#### `$vpat` named arguments

 * `self`: top level type path.  Default is `$tname`.
   Must expand to a syntactically valid type path,
   without generics.
 * `vname`: variant name.  Default is `$vname`.
   Not expanded for structs.
 * `fprefix`: prefix to use for the local bindings.
   Useful if you need to bind multiple values at once.
   Default is `f_`.
   When using this, use pasting (`$<FPREFIX $fname>`)
   rather than `$fpatname`.

These use derive-adhoc's usual
[syntax for named arguments](#named-and-positional-template-arguments-to-expansions-and-conditions).

#### Examples

 * `$vpat` for structs: `Unit { }`, `Tuple { 0: f_0, }`
 * `$vpat` for enum variant: `Enum::NamedVariant { field: f_field, ... }`
 * `$fpatname`: `f_0`, `f_field`
 * `${vpat self=$<$tname Reference> vname=$<Ref $vname> fprefix=other_}`: `EnumReference::RefNamedVariant { field: other_field, ... }`

### `$ftype`, `$vtype`, `$ttype`, `$tdeftype` -- types

The type of the field, variant, or the toplevel type.

`$ftype`, `$vtype` and `$ttype`
are suitable for referencing the type in any context
(for example, when defining the type of a binding,
or as a type parameter for a generic type).
These contains all necessary generics
(as names, without any bounds etc., but within `::<...>`).

`$vtype` includes both the top-level enum type, and the variant.
To construct a value, prefer `$vtype` rather than `$ttype`,
since `$vtype` works with enums too.

`$tdeftype` is
the top-level driver type name in a form suitable for defining
a new type with a derived name (eg, using pasting).
Contains all the necessary generics, with bounds,
within `<...>` but without an introducing `::`.

The toplevel type expansions, `$ttype` and `$tdeftype`,
don't contain a path prefix, even if
the driver type argument to
`derive_adhoc!`
had a path prefix.

`$vtype` (and `$ttype` and `$tdeftype`) are not suitable for matching.
Use `$vpat` for that.

#### `$vtype` named arguments

 * `self`: top level type.  Default is `$ttype`.
   Must expand to a syntactically valid type.
 * `vname`: variant name.  Default is `$vname`.
   Not expanded for structs.

These can be used with pasting
to name related (derived) types and variants.

They use derive-adhoc's usual
[syntax for named arguments](#named-and-positional-template-arguments-to-expansions-and-conditions).

#### Examples

 * `$ftype`: `std::iter::Once::<T>`
 * `$vtype` for struct: `Tuple::<'a, 'l, T, C>`
 * `$vtype` for enum variant: `Enum::TupleVariant::<'a, 'l, T, C>`
 * `$ttype`: `Enum::<'a, 'l, T, C>`
 * `$tdeftype`: `Enum<'a, 'l: 'a, T: Display = usize, const C: usize = 1>`
 * `${vtype self=$<$ttype Reference> vname=$<Ref $vname>}`
   for enum variant:
   `EnumReference::RefTupleVariant::<'a, 'l, T, C>`

### `$tgens`, `$tgnames`, `$twheres`, `$tdefgens` -- generics

Generic parameters and bounds, from the toplevel type,
in various forms.

   * **`$tgens`**:
     The generic arguments, with bounds, but without defaults.
     Suitable for use when starting an `impl`.

   * **`$tgnames`**:
     The generic argument names, without bounds.
     Suitable for use in a field type or in the body of an impl.

   * **`$twheres`**:
     The where clauses, as written in the toplevel type definition.

   * **`$tdefgens`**:
     The generic arguments, with bounds, *with* defaults,
     as written in the toplevel type definition,
     suitable for defining a derived type.

If not empty, each of these will always have a trailing comma.

Bounds appear in `$tgens`/`$tdefgens` or `$twheres`,
according to where they appear in the toplevel type,
so for full support of generic types the template must expand both.

#### Examples

 * `$tgens`: `'a, 'l: 'a, T: Display, const C: usize,`
 * `$tgnames`: `'a, 'l, T, C,`
 * `$twheres`: `T: 'l, T: TryInto<u8>,`
 * `$tdefgens`: `'a, 'l: 'a, T: Display = usize, const C: usize = 1,`

### `${tmeta(...)}` `${vmeta(...)}` `${fmeta(...)}` -- `#[adhoc]` attributes

Accesses macro parameters passed via `#[adhoc(...)]` attributes.

 * **`${Xmeta(NAME)}`**:
   Looks for `#[adhoc(NAME="VALUE")]`, and expands to `VALUE`.
   `"VALUE"` must be be a string literal,
   which is parsed as an arbitrary series of tokens.
   `${Xmeta...}` expands to those tokens.

 * **`${Xmeta(SUB(NAME))}`**:
   Looks for `#[adhoc(SUB(NAME="VALUE"))]`.
   The `#[adhoc()]` is parsed as
   a set of nested, comma-separated, lists.
   So this would find `NAME` 
   in `#[adhoc(SUB1,SUB(N1,NAME="VALUE",N2),SUB2)]`.
   The path can be arbitrarily deep, e.g.: `${Xmeta(L1(L2(L3(ATTR))))}`.

 * **`${Xmeta(...) as SYNTYPE}`**:
   Treats the value as a `SYNTYPE`,
   rather than an arbitrary sequence of tokens.
   `SYNTYPE`s available are:

    * **`str`**: Expands to a string literal
     with the same contents as
     the string provided for `VALUE`.
     Ie, the attribute's string value is *not* parsed.
     This is the default within pasting and case changing,
     if no `as` was specified.
     Within pasting and case changing,
     the provided string becomes part of the pasted identifier
     (and so must consist of legal identifier characters).

    * **`ty`**:
     `VALUE` is parsed as a type,
     possibly with generics etc. (`syn::Type`).

    * **`tokens`**:
     `VALUE` is parsed as an arbtitrary sequence of tokens
     (`TokenStream`).

When expanding `${Xmeta}`,
it is an error if the value was not specified in the driver,
and also an error if multiple values were specified.

For a struct, both `$tmeta` and `$vmeta`
look in the top-level attributes.
This allows a template to have uniform handling of attributes
which should affect how a set of fields should be processed.

#### Examples

 * `${tmeta(simple)}`: `String`
 * `${tmeta(simple) as ty}`: `String`
 * `${tmeta(simple) as str}`: `"String"`
 * `${tmeta(gentype)}`: `Vec<i32>`
 * `${tmeta(gentype) as ty}`: `Vec<i32>`
 * `${tmeta(gentype) as str}`: `"Vec<i32>"`
 * `${vmeta(value)}`: `unit-toplevel`, `enum-variant`
 * `${fmeta(nested(inner))}` for `field` in `Struct`: `42`
 * `${fmeta(nested)}`: error, ``tried to expand attribute which is nested list``

#### Examples involving pasting

 * `$<Small ${tmeta(simple)}>`: `SmallString`
 * `$<Small ${tmeta(simple) as str}>`: `SmallString`
 * `$<Small ${tmeta(simple) as ty}>`: `SmallString`
 * `$<Small ${tmeta(gentype) as ty}>`: `SmallVec<i32>`
 * `$<$ttype ${tmeta(simple) as str}>`: `UnitString::<C>`
 * `$<$ttype ${tmeta(simple) as ty}>`: error, ``multiple nontrivial entries``

### `${fattrs ...}` `${vattrs ...}` `${tattrs ...}` -- other attributes

Expands to attributes, including non-`#[adhoc()]` ones.
The attributes can be filtered:

  * **`$Xattrs`**: All the attributes
    except `#[adhoc]` and `#[derive_adhoc]`
  * **`${Xattrs A1, A2, ...}`**, or
    **`${Xattrs = A, A2, ...}`**:
    Attributes `#[A1...]` and `#[A2...]` only.
  * **`${Xattrs ! A1, A2, ...}`**:
    All attributes *except* those.

With `${Xattrs}`, unlike `${Xmeta}`,

   * The expansion is the whole of each attribute, including the `#[...]`;
   * All attributes are included.
   * But `#[adhoc(...)]` and `#[derive_adhoc(...)]` are *excluded* by default,
     because typically they would be rejected by the compiler:
     the expanded output is (perhaps) no longer within `#[derive(Adhoc)]`,
     so those attributes might be unrecognised there.
   * The attributes can be filtered by toplevel attribute name,
     but not deeply manipulated.
   * `$vattrs` does not, for a non-enum, include the top-level attributes .

Note that derive macros,
only see attributes
that come *after* the `#[derive(...)]` that invoked them.
So derive-adhoc templates only see attributes
that come *after* the `#[derive(..., Adhoc, ...)]`.

#### Examples

##### For `Unit`

<!--##examples-for `Unit`##-->

 * `${tattrs}`: ``#[derive(Clone)]``
 * `${tattrs ! adhoc}`: ``#[derive(Clone)]``
 * `${tattrs missing}`: nothing
 * `${tattrs derive}`: ``#[derive(Clone)]``
 * `${vattrs adhoc}`: nothing

##### For `Tuple`

<!--##examples-for `Tuple`##-->

 * `${tattrs}`: ``#[doc=" Title for `Tuple`"] #[repr(C)]``
 * `${tattrs repr}`: ``#[repr(C)]``
 * `${tattrs repr, adhoc}`: ``#[adhoc(unused)] #[repr(C)]``
 * `${tattrs ! derive, doc}`: ``#[adhoc(unused)] #[repr(C)] #[derive_adhoc(SomeOtherTemplate)]``

##### For `Enum`

 * `${vattrs adhoc}` for `UnitVariant`: `#[adhoc(value="enum-variant")]`

### `$<...>`, `${paste ...}` -- identifier pasting

Expands the contents and pastes it together into a single identifier.
The contents may only contain identifer fragments, strings (`"..."`),
and (certain) expansions.
Supported expansions are `$ftype`, `$ttype`, `$tdeftype`, `$Xname`,
`${Xmeta as ty}`, `${Xmeta as str}`,
`$<...>`,
`${paste ...}`,
`${CASE_CHANGE ...}`,
`$tdefkwd`,
as well as conditionals and repetitions.

The contents can contain at most one occurrence of
a more complex type expansion `${Xtype}`
(or `${Xmeta as ty)`),
which must refer to a path (perhaps with generics).
Then the pasting will be applied to the final path element identifier,
and the path prefix and generics reproduced unaltered.

#### Example

 * `$<Zingy $ftype Builder>` for `TupleVariant`:
    `std::iter::ZingyOnceBuilder::<T>`
 * `${paste x_ $fname}` for tuple: `x_0`
 * `${paste $fname _x}` for tuple: error, ``constructed identifier "0_x" is invalid``

### `${CASE_CHANGE ...}` -- case changing

Expands the content, and changes its case
(eg. uppercase to lowercase, etc.
See [Case changing](#case-changing).
`CASE_CHANGE` is one of the values listed there.

### `${when CONDITION}` -- filtering out repetitions by a predicate

Allowed only within repetitions, and only at the toplevel
of the repetition,
before other expansions.
Skips this repetition if the `CONDITION` is not true.

#### Example

 * `$( ${when vmeta(value)} ${vmeta(value) as str} )` for `Enum`: `"enum-variant"`

### `${if COND1 { ... } else if COND2 { ... } else { ... }}` -- conditional

Conditionals.  The else clause is, of course, optional.
The `else if` between arms is also optional,
but `else` in the fallback clause is mandatory.
So you can write `${if COND1 { ... } COND2 { ... } else { ... }`.

#### Examples

 * `${if is_enum { E } is_struct { S }}` for `Enum`: `E`
 * `${if is_enum { E } is_struct { S }}` for others: `S`
 * `$( ${if v_is_named { N } v_is_tuple { T }} )` for `Enum`: `T N`
 * `$( ${if v_is_named { N } v_is_tuple { T } else { X }} )` for `Enum`: `X T N`
 * `${if v_is_unit { U } tmeta(gentype) { GT }}` for `Unit`: `U`

### `${select1 COND1 { ... } else if COND2 { ... } else { ... }}` -- expect precisely one predicate

Conditionals which insist on expanding exactly one of the branches.
Syntax is identical to that of `${if }`.
*All* of the `COND` are always evaluated.
Exactly one of them must be true;
or, none of them, but only if an `else` is supplied -
otherwise it is an error.

#### Examples

 * `${select1 is_enum { E } is_struct { S }}`: `E`, `S`
 * `${select1 v_is_named { N } v_is_tuple { T }}` for `Enum`: error, ``no conditions matched, and no else clause``
 * `$( ${select1 v_is_named { N } v_is_tuple { T } else { X }} )` for `Enum`: `X T N`
 * `${select1 v_is_unit { U } tmeta(gentype) { GT }}` for `Unit`: error, ``multiple conditions matched``

### `${for fields { ... }}`, `${for variants { ... }}`, `$( )` -- repetition

`${for ...}` expands the contents once per field, or once per variant.

`$( ... )` expands the input with an appropriate number of iterations -
see [Repetition and nesting](#repetition-and-nesting).

### `$crate` -- root of template crate

`$crate` always refers to the root of the crate 
defining the template.
Within a `pub` template,
being expanded in another crate,
it refers to the crate containing the template definition.
In templates being used locally,
it refers to the current crate, ie simply `crate`.

This is similar to the `$crate` builtin expansion
in `macro_rules!`.

### `$tdefkwd` -- keyword introducing the new data structure

Expands to `struct`, `enum`, or `union`.

### `$tdefvariants`, `$vdefbody`, `$fdefine` -- tools for defining types

These, used together, allow the template to expand to a
new definition, mirroring the driver type in form.

**`${tdefvariants VARIANTS}`** expands to `{ VARIANTS }` for an enum,
or just `VARIANTS` otherwise.
Usually, it would contain a `$( )` repeating over the variants,
expanding `$vdefbody` for each one.

**`${fdefine FNAME}`** expands to `FNAME:` in the context of
named fields (a "struct" or "struct variant"),
or nothing otherwise.

**`${vdefbody VNAME FIELDS}`** expands to the definition of a variant,
with a appropriate delimiters.
Usualy, it would contain a `$( )` repeating over the fields,
using `$fdefine` to introduce each one.
Specifically:

<!--##examples-possibilities-blockquote##-->
<!--iiiiiiiiiiiiiiiiiiiiiii fffffffffffffffffffff oooooooooooooooooo ###############-->

```text
 ${vdefbody VNAME FIELDS}   for unit                FIELDS;          [*] ie  ;
 ${vdefbody VNAME FIELDS}   for tuple             ( FIELDS );
 ${vdefbody VNAME FIELDS}   for braced struct     { FIELDS }
 ${vdefbody VNAME FIELDS}   for unit variant      VNAME   FIELDS,    [*] ie  VNAME,
 ${vdefbody VNAME FIELDS}   for tuple variant     VNAME ( FIELDS ),
 ${vdefbody VNAME FIELDS}   for braced variant    VNAME { FIELDS }
```

`[*]`: In the unit and unit variant cases,
`FIELDS` ought to expand to nothing;
otherwise, the expansion of `$vdefbody`
will probably be syntactically invalid in context.

#### Example

```text
$tvis $tdefkwd $<$tname Copy><$tdefgens>
${tdefvariants $(
    ${vdefbody $<$vname Copy> $(
        $fdefvis ${fdefine $<$fname _copy>} $ftype,
    ) }
) }
```

<!--##examples-for-toplevels-concat Tuple Enum##-->
Expands to (when applied to `Tuple` and `Enum`):

```rust,ignore
struct TupleCopy<'a, 'l: 'a, T: Display = usize, const C: usize = 1,>(
    &'a &'l T,
);
pub enum EnumCopy<'a, 'l: 'a, T: Display = usize, const C: usize = 1,> {
    UnitVariantCopy,
    TupleVariantCopy(std::iter::Once::<T>,),
    NamedVariantCopy { field_copy: &'l &'a T, ... }
}
```

### `$dbg_all_keywords` -- Dump expansions of all keywords to compiler stderr

Prints a listing of all the available expansion keywords,
and conditions,
along with their expansions and values.
(The output goes to the compiler's stderr;
the actual expansion is empty.)

This can be helpful to see which expansion keywords
might be useful for a particular task.
(Before making a final selection of keyword
you probably want to refer to this reference manual.)

You will not want to leave this option in production code,
as it makes builds noisy.

See also the [`dbg` expansion option](#dbg--print-the-expansion-to-stderr-for-debugging).

#### Example

```rust
# use derive_adhoc::{Adhoc, derive_adhoc};
#[derive(Adhoc)]
enum Enum {
    Unit,
    Tuple(usize),
    Struct { field: String },
}
derive_adhoc! {
    Enum:
    $dbg_all_keywords
    // ... rest of the template you're developing ...
}
```

### `${define ...}`, `${defcond ...}` -- user-defined expansions and conditions

`${define NAME BODY}` defines a reuseable piece of template.
Afterwards, `$NAME` (and `${NAME}`) expand `BODY`.

`${defcond NAME CONDITION}` defines a reuseable condition.
Afterwards, the name `NAME` can be used as a condition -
evaluating `CONDITION`.

`NAME` is an identifier.
It may not start with a lowercase letter or underscore:
those expansion names are reserved for
derive-adhoc's built-in functionality.

`BODY` is in the
[standard syntax for positional arguments](#named-and-positional-template-arguments-to-expansions-and-conditions).

`CONDITION` is in the standard syntax for a condition.

`NAME` is visible after its definition in the same template or group,
including in inner templates and groups.
Definitions may be re-defined by inner scopes.
Scope is dynamic,
both for derive-adhoc built-ins and user definitions:
`BODY` and `CONDITION` are captured
without expansion/evaluation at the site of `$define`/`$defcond`,
and the contents expanded/evaluated
each time according
to the values and definitions prevailing
in the dynamic context where `NAME` is used.

`${NAME}` may only be used
inside pasting and case changing
if `BODY` was precisely an invocation of `${paste }` or `$<...>`.

You can define an expansion and a condition with the same name;
they won't interfere.

#### Examples

 * `${define VN $vname} ${for variants { $VN }}`:
   `UnitVariant TupleVariant NamedVariant`
 * `${define FN $<$fname _>} $<${for fields { "F" $FN }}>`:
   `F0_`, `Ffield_Ffield_b_`

##### Example including a condition

```rust,ignore
${define T_FIELDS ${paste $tname Fields}}
${defcond F_ENABLE all(fvis, v_is_named)}
$tvis struct $T_FIELDS { $(
    ${when F_ENABLE} $fvis $fname: bool,
) }
$tvis const ${shouty_snake_case ALL_ $T_FIELDS}: $T_FIELDS = { $(
    ${when F_ENABLE} $fname: true,
) };
```

<!--##examples-for-toplevels-concat Unit Tuple Struct##-->
Expands to (for `Unit`, `Tuple` and `Struct`):

```rust,ignore
pub struct UnitFields {}
pub const ALL_UNIT_FIELDS: UnitFields = {};
struct TupleFields {}
const ALL_TUPLE_FIELDS: TupleFields = {};
struct StructFields {
    pub field: bool,
}
const ALL_STRUCT_FIELDS: StructFields = {
    field: true,
};
```

## Conditions

Conditions all start with a `KEYWORD`.
They are found within `${if }`, `${when }`, and `${select1 }`.

<!-- ## maint/check-keywords-documented conditions ## -->

### `fvis`, `tvis`, `fdefvis` -- test for public visibility

True iff the field, or the whole toplevel type, is `pub`.

See
[`$fvis`, `$tvis` and `$fdefvis`](#fvis-tvis-fdefvis--visibility)
for details of the semantics (especially for enums),
and the difference between `$fvis` and `$fdefvis`.

Within-crate visibility, e.g. `pub(crate)`, is treated as "not visible"
for the purposes of `fvis` and `tvis`
(although the `$fvis` and `$tvis` expansions will handle those faithfully).

#### Examples

 * `tvis`: true for `Unit`, and `Enum`
 * `fvis`: true for `field` in `Struct`, and fields in `Enum`
 * `fdefvis`: true for `field` in `Struct`

<!--##examples-ignore##-->
And in each case, false for all others.
(Refer to the [example structs](#structs-used-in-examples), below.)

### `fmeta(NAME)`, `vmeta(NAME)`, `tmeta(NAME)` -- `#[adhoc]` attributes

Looks for `#[adhoc(NAME)]`.

True iff there was such an attribute.
`Xmeta(SUB(NAME))` works, just as with the `${Xmeta ...}` expansion.

The condition is true if there is at least one matching entry,
and (unlike `${Xmeta}`)
the corresponding driver attribute does not need to be a `=LIT`.

So `Xmeta(SUB(NAME))` is true if the driver has
`#[adhoc(SUB(NAME(INNER=...)))]` or `#[adhoc(SUB(NAME))]` or
`#[adhoc(SUB(NAME=LIT))]` or even `#[adhoc(SUB(NAME()))]`.

#### Examples

 * `tmeta(unused)`: true for `Tuple`
 * `tmeta(gentype)`: true for `Unit`
 * `vmeta(value)`: true for `Unit`, and `Enum::UnitVariant`
 * `fmeta(nested)`: true for `field` in `Struct`

### `is_struct`, `is_enum`, `is_union`

The driver data structure is a struct, enum, or union, respectively.

Prefer to avoid these explicit tests,
when writing a template to work with either structs or enums.
Instead,
use `match` and `$vpat` for deconstructing values,
and `$vtype` for constructing them.
Use `$tdefvariants` when defining a derived type.

### `v_is_unit`, `v_is_tuple`, `v_is_named`

Whether and what kind of fields there are.

Prefer to avoid these explicit tests,
when writing a template to work with any shape of structure.
Instead,
use Rust's universal `Typename { }` syntax,
possibly via `$vpat` and `$fpatname`,
or via `$vtype`.
The `Typename { }` syntax can be used for matching and constructing
all kinds of structures, including units and tuples.
Use `$vdefbody` and `$fdefine` when defining a derived type.

#### Examples

 * `v_is_unit`: true for `struct Unit;`, and `Enum::UnitVariant;`
 * `v_is_tuple`: true for `struct Tuple(...);`, and `Enum::TupleVariant(...);`
 * `v_is_named`: true for `struct Struct {...}`, and `Enum::NamedVariant {...}`

### `approx_equal(ARG1, ARG2)` -- equality comparison (token comparison)

The two `ARGS`s are each expanded (as series of tokens)
and compared for equality.

Span is disregarded, so
two identifiers that would refer to different types or values,
but which have the same name,
would count as equal.

Spacing is disregarded, even between punctuation characters.
For example, `approx_equal` regards `<<` as equal to `< <`.
This means expansions might count as equal
even if the Rust compiler would accept one and reject the other;
and, expansions migtht count as equal
even if macros could tell the difference.

If both inputs are valid Rust types,
they will only compare equal if they are syntactically the same.
(Note that different ways of writing the same type
are treated as different:
for example, `Vec<u8>` is not equal to `Vec<u8, Global>`
and `std::os::raw::c_char` is not equal to `std::ffi::c_char`.)

The `ARG`s are in derive-adhoc's usual
[syntax for positional arguments](#named-and-positional-template-arguments-to-expansions-and-conditions).

### `false`, `true`, `not(CONDITION)`, `any(COND1,COND2,...)`, `all(COND1,COND2,...)` -- boolean logic

## Case changing

`${CASE_CHANGE ...}` makes an identifier
with a different case to the input which produces it.
This is useful to make identifiers with the natural spelling
for their kind,
out of identifiers originally for something else.

If the content's expansion is a path, only the final segment is changed.

The content must be valid within `${paste }`,
and is treated the same way.
`${CASE_CHANGE }` may appear within pasting and vice versa.

This table shows the supported case styles.
Note that changing the case can add and remove underscores.
The precise details are as for [`heck`],
which is used to implement the actual case changing.

<!-- ## maint/check-keywords-documented cases ## -->

| `CASE_CHANGE`        | `CASE_CHANGE` aliases            | Name in [`heck`]  | Example of results    |
|----------------------|----------------------------------|-----------------------------------|-----------------------|
| `pascal_case`        | `upper_camel_case`               | `UpperCamelCase`                  | `PascalCase`          |
| `snake_case`         |                                  | `SnakeCase`                       | `snake_case`          |
| `shouty_snake_case`  |                                  | `ShoutySnakeCase`                 | `SHOUTY_SNAKE_CASE`   |
| `lower_camel_case`   |                                  | `LowerCamelCase`                  | `lowerCamelCase`      |


#### Examples

 * `${shouty_snake_case $ttype}`: `ENUM::<'a, 'l, T, C>`
 * `${pascal_case $fname}`: `Field`, `FieldB`
 * `${pascal_case x_ $fname _y}`: `XFieldBY`
 * `$<x_ ${lower_camel_case $fname} _y>`: `x_fieldB_y`
 * `${lower_camel_case $fname}` for tuple: error, ``constructed identifier "0" is invalid``

## Expansion options

You can pass options,
which will be applied to each relevant template expansion:

```rust,ignore
// Expand TEMPLATE for DataStructureType, with OPTIONS
derive_adhoc! { DataStructureType OPTIONS,... : TEMPLATE }

// Define a template Template which always expands with OPTIONS
define_derive_adhoc! { Template OPTIONS,... = TEMPLATE }

// Expand Template for DataStructureType, with OPTIONS
#[derive(Adhoc)]
#[derive_adhoc(Template[OPTIONS,...])]
struct DataStructureType {
# }
```

Multiple options, perhaps specified in different places,
may apply to a single expansion.
Even multiple occurrences of the same option are fine,
so long as they don't contradict each other.

The following expansion options are recognised:

### `expect items`, `expect expr` -- syntax check the expansion

Syntax checks the expansion,
checking that it can be parsed as items, or as an expression.

If not, it is an error.
Also, then, an attempt is made to produce
compiler error message(s) pointing to the syntax error
*in a copy of the template expansion*,
as well as reporting the error at
the part of the template or driver which generated
that part of the expansiuon.

This is useful for debugging.

### `for struct`, `for enum`, `for union` -- Insist on a particular driver kind

Checks the driver data structure kind
against the `for` option value.
If it doesn't match, it is an error.

This is useful to produce good error messages:
Normally, derive-adhoc does not explicitly check the driver kind,
and simply makes it available to the template via expansion variables.
But, often,
a template is written only with a particular driver kind in mind,
and otherwise produces syntactically invalid output
leading to confusing compiler errors.

This option is only allowed in a template,
not in a driver's `#[derive_adhoc]` attribute.

### `dbg` -- Print the expansion to stderr, for debugging

Prints the template's expansion to stderr, during compilation,
for debugging purposes.

You will not want to leave this option in production code,
as it makes builds noisy.

See also the [`$dbg_all_keywords` expansion](#dbg_all_keywords--dump-expansions-of-all-keywords-to-compiler-stderr).

### Expansion options example

```
# use derive_adhoc::{define_derive_adhoc, Adhoc};
define_derive_adhoc! { Nothing for struct, expect items = }

#[derive(Adhoc)]
#[derive_adhoc(Nothing[expect items, dbg])]
struct Unit;
```

This defines a reuseable template `Nothing`
which can be applied only to structs,
and whose output is syntax checked as item(s).
(The template's actual expansion is empty,
so it does indeed expand to zero items.)

Then it applies that to template to `struct Unit`,
restating the requirement that the expansion should be item(s).
and dumping the expansion to stderr during compilation.

## Structs used in examples

The example expansions in the syntax reference 
are those generated for the following driver types:

<!--##examples-structs##-->

```ignore
# use std::fmt::Display;
# use std::convert::TryInto;
#
#[derive(Adhoc)]
#[derive(Clone)]
#[adhoc(simple="String", gentype="Vec<i32>")]
#[adhoc(value="unit-toplevel")]
pub struct Unit<const C: usize = 1>;

#[derive(Adhoc, Clone)]
/// Title for `Tuple`
#[adhoc(unused)]
#[repr(C)]
#[derive_adhoc(SomeOtherTemplate)]
struct Tuple<'a, 'l: 'a, T: Display = usize, const C: usize = 1>(
    &'a &'l T,
);

#[derive(Adhoc)]
struct Struct<'a, 'l: 'a, T: Display = usize, const C: usize = 1>
where T: 'l, T: TryInto<u8>
{
    #[adhoc(nested(inner = "42"))]
    pub field: &'l &'a T,
    pub(crate) field_b: String,
}

#[derive(Adhoc)]
pub enum Enum<'a, 'l: 'a, T: Display = usize, const C: usize = 1>
where T: 'l, T: TryInto<u8>
{
    #[adhoc(value="enum-variant")]
    UnitVariant,
    TupleVariant(std::iter::Once::<T>),
    NamedVariant { 
        field: &'l &'a T,
        field_b: String,
        field_e: <T as TryInto<u8>>::Error,
     },
}
```
