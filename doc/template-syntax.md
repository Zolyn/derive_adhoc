# Template syntax reference

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
if the repetition level is "depeer" than the level
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

 * **`$fname`, `$vname`, `$tname`**:
   The name of the field, variant, or toplevel type.
   This is an the identifier (without any path or generics).
   For tuple fields, `$fname` is the field number.

 * **`$ftype`, `$ttype`**:
   The type of the field, or the toplevel type.
   This contains all necessary generics (as names).
   For the toplevel type it contains a path prefix iff
   the driver type argument to
   `derive_adhoc!{ }`
   had a path prefix.

 * **`$tgens`**, **`$tgens`**, **`$twheres`**:
   Generic parameters and bounds, from the toplevel type,
   in various forms.

   * **`$tgens`**:
     The generic arguments, with bounds,
     as written in the toplevel type definition.

     Example: `'l:'a, T:X, const C=1,` .

   * **`$tgnames`**:
     The generic argument names, without bounds,
     as might be used in a field type or inherent impl.

     Example: `'l, T, C,` .

   * **`$twheres`**:
     The where clauses, as written in the toplevel type definition.

     Example: `T: 'a,` .

   If not empty, will always have a trailing comma.

   Bounds appear in `$tgens` or `$twheres`,
   according to where they appear in the toplevel type,
   so for full support of generic types the template must expand both.

   Examples each show the expansion for
   `struct Foo<'l:'a, T:X, const C=1> where T: 'a {...}`.

 * **`${tmeta(...)}` `${vmeta(...)}` `${fmeta(...)}`**:
   <a name="derive_adhoc_syntax_Xmeta" style="border-right:none"></a>
   Accesses macro parameters passed via `#[adhoc(...)]` attributes.

    + **`${Xmeta(NAME)}`**:
      Looks for `#[adhoc(NAME=LIT)]`, and expands to `LIT`.
      `LIT` can only be a literal, and is expanded as such.

    + **`${Xmeta(SUB(NAME))}`**:
      Looks for `#[adhoc(SUB(NAME=LIT))]`, and expands to `LIT`.
      The `#[adhoc()]` is parsed as
      a set of nested, comma-separated, lists.
      So this could would find `NAME` 
      in `#[adhoc(SUB1,SUB(N1,NAME=LIT,N2),SUB2)]`.
      The path can be arbitrarily deep, e.g.: `${Xmeta(L1(L2(L3(ATTR))))}`.

    + **`${Xmeta(...) as SYNTYPE}`**:
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

 * **`${fattrs ...}` `${vattrs ...}` `${tattrs ...}`**:
   Expands to non-`#[adhoc()]` attributes.
   The attributes can be filtered:

    * **`$Xattrs`**: All the attributes.
    * **`${Xattrs A1, A2, ...}`**, or
      **`${Xattrs = A, A2, ...}`**:
      Attributes `#[A1...]` and `#[A2...]` only.
    * **`${Xattrs ! A1, A2, ...}`**:
      All attributes *except* those.

   With `${Xattrs}`, unlike `${Xmeta}`,
     * The expansion is all of the attributes, including the `#[...]`;
     * All attributes, not just `#[adhoc(...)]`,, are included.
     * The attributes can be filtered by toplevel attribute name,
       but not deeply manipulated.

 * **`${paste ...}`**:
   Expand the contents and paste it together into a single identifier.
   The contents may only contain identifer fragments, strings (`"..."`),
   and (certain) expansions.
   Supported expansions are `${Xtype}`, `${Xname}`, `${Xmeta}`,
   as well as conditionals and repetitions.

   The contents can contain at most one occurrence of
   a more complex type expansion `${Xtype}`,
   which must refer to a path (perhaps with generics).
   Then the pasting will be applied to the final path element identifier,
   and the path prefix and generics reproduced unaltered.
   For example, with
   a struct field `field: crate::config::Foo<'a,T,C>`,
   writing
   `${paste Zingy $ftype Builder}`
   generates
   `crate::config::ZingyFooBuilder<'a,T,C>`.

   `${Xmeta}` must reference a (supplied) `#[adhoc]` meta item,
   whose value must be a literal.

 * **`${when CONDITION}`**:
   Allowed only within repetitions, and only at the toplevel,
   before other expansions.
   Skips this repetition if the `CONDITION` is not true.

 * **`${if COND1 { ... } else if COND2 { ... } else { ... }}`**:
   Conditionals.  The else clause is, of course, optional.

 * **`${for fields { ... }}`**:
   Expands the contents once per field.

## Conditions

Conditions all start with a `KEYWORD`.
They are found within `${if }` and `${when }`.

 * **`fmeta(NAME)`, `vmeta(NAME)`, `tmeta(NAME)`**:
   Looks for `#[adhoc(NAME)]`.

   True iff there was such an attribute.
   `Xmeta(SUB(NAME))` works, just as with the `${Xmeta ...}` expansion.

   The condition is true if there is at least one matching entry,
   and (unlike `${Xmeta}`)
   the corresponding driver attribute does not need to be a `=LIT`.

   So `Xmeta(SUB(NAME))` is true if the driver has
   `#[adhoc(SUB(NAME(INNER=...)))]` or `#[adhoc(SUB(NAME))]` or
   `#[adhoc(SUB(NAME=LIT))]` or even `#[adhoc(SUB(NAME()))]`.

 * **`is_enum`**: The driver data structure is an enum.

 * **`false`, `true`, `not(CONDITION)`, 
   `any(COND1,COND2,...)`, `all(COND1,COND2,...)`**.
