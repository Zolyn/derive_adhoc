# **Template syntax reference**

Within the macro template,
expansions (and control structures) are introduced with `$`.
They generally refer to properties of the data structure that
we're deriving from.
We call that data structure the **driver**.

In general the syntax is:

 * `$KEYWORD`: Invoke the expansion of the keyword `KEYWORD`.
 * `${KEYWORD PARAMS...}`: Invoke with parameters.
 * `$( .... )`: Repetition (abbreviated, automatic, form).
   (Note: there is no `+` or `*` after the `)`)

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

(Here, within this documentation,
we often write in `CAPITALS` to indicate meta-meta-syntactic elements,
since all of the punctuation is already taken.)

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

## Expansions

### `$fname`, `$vname`, `$tname` - names

The name of the field, variant, or toplevel type.
This is an the identifier (without any path or generics).
For tuple fields, `$fname` is the field number.

`$fname` is not suitable for direct use as a local variable name.
It might clash with other local variables;
and, unlike most other expansions,
`$fname` has the hygiene span of the driver field name.
Instead, use `$vpat`, `$fpatname`, or `${paste ... $fname ...}`.

### `$ftype`, `$ttype` - types

The type of the field, or the toplevel type.
These are suitable for referencing the type in any context
(for example, when defining the type of a binding,
or as a type parameter for a generic type).

This contains all necessary generics
(as names, without any bunds etc., but within `::<...>`).
For the toplevel type it doesn't contains a path prefix, even if
the driver type argument to
`derive_adhoc!{ }`
had a path prefix.

#### Examples

 * `$ftype`: `std::iter::Once::<T>`
 * `$ttype`: `Enum<'a, 'l, T, C>`

### `$tdeftype` - type name, for defining a new type

The top-level driver type name in a form suitable for defining
a new type with a derived name (eg, using `${paste }`).
Contains all the necessary generics, with bounds,
within `<...>` but without an introducing `::`.

### `$tgens`, `$tgens`, `$twheres` - generics

Generic parameters and bounds, from the toplevel type,
in various forms.

   * **`$tgens`**:
     The generic arguments, with bounds, but without defaults,
     as written in the toplevel type definition.

   * **`$tgnames`**:
     The generic argument names, without bounds,
     as might be used in a field type or on an impl.

   * **`$twheres`**:
     The where clauses, as written in the toplevel type definition.

If not empty, will always have a trailing comma.

Bounds appear in `$tgens` or `$twheres`,
according to where they appear in the toplevel type,
so for full support of generic types the template must expand both.

#### Examples

 * `$tgens`: `a, 'l: 'a, T: Display, const C: usize,`
 * `$tgnames`: `'a, 'l, T, C,`
 * `$twheres`: `T: 'l, T: TryInto<u8>,`

### <a name="derive_adhoc_syntax_Xmeta">`${tmeta(...)}` `${vmeta(...)}` `${fmeta(...)}`</a> - `#[adhoc]` attributes

Accesses macro parameters passed via `#[adhoc(...)]` attributes.

 * **`${Xmeta(NAME)}`**:
   Looks for `#[adhoc(NAME=LIT)]`, and expands to `LIT`.
   `LIT` can only be a literal, which is parsed as Rust tokens,
   which become the result of the expansion.
   (Within `${paste }` and `${case }`, the literal is used directly.)

 * **`${Xmeta(SUB(NAME))}`**:
   Looks for `#[adhoc(SUB(NAME=LIT))]`, and expands to `LIT`.
   The `#[adhoc()]` is parsed as
   a set of nested, comma-separated, lists.
   So this could would find `NAME` 
   in `#[adhoc(SUB1,SUB(N1,NAME=LIT,N2),SUB2)]`.
   The path can be arbitrarily deep, e.g.: `${Xmeta(L1(L2(L3(ATTR))))}`.

 * **`${Xmeta(...) as SYNTYPE}`**:
   Parses `LIT` as Rust code specifying a `SYNTYPE`,
   and then expands to that.
   Unless `SYNTYPE` is `lit`, `LIT` must be a *string* literal.
   `SYNTYPE`s available are:

    * **`lit`**: A literal value; expands `LIT` directly;
      equivalent to not having said `as`.
    * **`ty`**: A type, possibly with generics etc. (`syn::Type`).

When expanding `${Xmeta}`,
it is an error if the value was not specified in the driver,
and also an error if multiple values were specified.

For a struct, both `$tmeta` and `$vmeta`
look in the top-level attributes.
This allows a template to have uniform handling of attributes
which should affect how a set of fields should be processed.

### `${fattrs ...}` `${vattrs ...}` `${tattrs ...}` - other attributes

Expands to non-`#[adhoc()]` attributes.
The attributes can be filtered:

  * **`$Xattrs`**: All the attributes
    except `#[adhoc]` and `#[derive_adhoc]`
  * **`${Xattrs A1, A2, ...}`**, or
    **`${Xattrs = A, A2, ...}`**:
    Attributes `#[A1...]` and `#[A2...]` only.
  * **`${Xattrs ! A1, A2, ...}`**:
    All attributes *except* those.

With `${Xattrs}`, unlike `${Xmeta}`,

   * The expansion is all of the attributes, including the `#[...]`;
   * All attributes, are included.
   * But `#[adhoc(...)]` and `#[derive_adhoc(...)]` are *excluded* by default,
     because typically they would be rejected by the compiler:
     the expanded output is no longer within `#[derive(Adhoc)]`,
     so those attributes are not recognised there.
   * The attributes can be filtered by toplevel attribute name,
     but not deeply manipulated.

### `${paste ...}` - identifier pasting

Expand the contents and paste it together into a single identifier.
The contents may only contain identifer fragments, strings (`"..."`),
and (certain) expansions.
Supported expansions are `$Xtype`, `$Xname`, `$Xmeta`,
`${CASE_CHANGE}`,
$tdefkwd,
as well as conditionals and repetitions.

The contents can contain at most one occurrence of
a more complex type expansion `${Xtype}`
(or `${}Xmeta as ty)`),
which must refer to a path (perhaps with generics).
Then the pasting will be applied to the final path element identifier,
and the path prefix and generics reproduced unaltered.
For example, with
a struct field `field: crate::config::Foo<'a,T,C>`,
writing
`${paste Zingy $ftype Builder}`
generates
`crate::config::ZingyFooBuilder<'a,T,C>`.

### `${CASE_CHANGE ...}` - case changing

Expands the content, and changes its case
(eg. uppercase to lowercase, etc.
See [Case changing](#case-changing).

### `${when CONDITION}` - filtering out repetitions by a predicate

Allowed only within repetitions, and only at the toplevel
of the repetition,
before other expansions.
Skips this repetition if the `CONDITION` is not true.

### `${if COND1 { ... } else if COND2 { ... } else { ... }}` - conditional

Conditionals.  The else clause is, of course, optional.
The `else if` between arms is also optional,
but `else` in the fallback clause is mandatory.
So you can write `${if COND1 { ... } COND2 { ... } else { ... }`.

### `${select1 COND1 { ... } else if COND2 { ... } else { ... }}` - expect precisely one predicate

Conditionals which insist on expanding exactly one of the branches.
Syntax is identical to that of `${if }`.
*All* of the `COND` are always evaluated.
Exactly one of them must be true;
or, none of them, bot only if an `else` is supplied -
otherwise it is an error.

### `${for fields { ... }}`, `${for variants { ... }}` - explicit repetition

Expands the contents once per field, or once per variant.

### `$tdefkwd` - keyword introducing the new data structure

Expands to `struct`, `enum`, or `union`.

## Conditions

Conditions all start with a `KEYWORD`.
They are found within `${if }`, `${when }`, and `${select1 }`.

### `fmeta(NAME)`, `vmeta(NAME)`, `tmeta(NAME)` - `#[adhoc]` attributes

Looks for `#[adhoc(NAME)]`.

True iff there was such an attribute.
`Xmeta(SUB(NAME))` works, just as with the `${Xmeta ...}` expansion.

The condition is true if there is at least one matching entry,
and (unlike `${Xmeta}`)
the corresponding driver attribute does not need to be a `=LIT`.

So `Xmeta(SUB(NAME))` is true if the driver has
`#[adhoc(SUB(NAME(INNER=...)))]` or `#[adhoc(SUB(NAME))]` or
`#[adhoc(SUB(NAME=LIT))]` or even `#[adhoc(SUB(NAME()))]`.

### `is_enum`

The driver data structure is an enum.

### `false`, `true`, `not(CONDITION)`, `any(COND1,COND2,...)`, `all(COND1,COND2,...)` -- boolean logic

## Case changing

`${CASE_CHANGE ...}` makes an identifier
with a different case to the input which produces it.
This is useful to make identifiers with the natural spelling
for their kind,
out of identifiers originally for something else.

The content must be a single (terminal) expansion item
which would be valid within `${paste }`.
If the content's expansion is a path, only the final segment is changed.
`${CASE_CHANGE }` may appear within `${paste }` to change the case of
a paste fragment, before concatenation.
(Concatenating before changing case, `${CASE_CHANGE ${paste ...}}`,
is not supported.)

This table shows the supported case styles.
Note that changing the case can add and remove underscores.
The precise details are as for [`heck`],
which is used to implement the actual case changing.

| `CASE_CHANGE`        | `CASE_CHANGE` aliases            | Name in [`heck`] (also an alias)  | Example of results    |
|----------------------|----------------------------------|-----------------------------------|-----------------------|
| `pascal_case`        | `PascalCase` `upper_camel_case`  | `UpperCamelCase`                  | `PascalCase`          |
| `snake_case`         |                                  | `SnakeCase`                       | `snake_case`          |
| `shouty_snake_case`  | `SHOUTY_SNAKE_CASE`              | `ShoutySnakeCase`                 | `SHOUTY_SNAKE_CASE`   |
| `lower_camel_case`   | `lowerCamelCase`                 | `LowerCamelCase`                  | `lowerCamelCase`      |

## Structs used in examples

The example expansions in the syntax reference 
are those generated for the following driver types:

```
# use std::fmt::Display;
# use std::convert::TryInto;
#
struct Unit<const C: usize = 1>;

struct Tuple<'a, 'l: 'a, T: Display = usize, const C: usize = 1>(
    &'a &'l T,
);

struct Named<'a, 'l: 'a, T: Display = usize, const C: usize = 1>
where T: 'l, T: TryInto<u8>
{
    field: &'l &'a T,
    field_b: String,
}

enum Enum<'a, 'l: 'a, T: Display = usize, const C: usize = 1>
where T: 'l, T: TryInto<u8>
{
    UnitVariant,
    TupleVariant(std::iter::Once::<T>),
    NamedVariant { 
        field: &'l &'a T,
        field_b: String,
        field_e: <T as TryInto<u8>>::Error,
     },
}
```
