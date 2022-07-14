# derive_adhoc: An ergonomic replacement for (some) proc macros

### Status

As of 1 July 2022, this is still very much a work-in-progress: you probably
shouldn't use it yet unless you like playing with sharp edges.

## Expansions

Within the macro template,
expansions (and control structures) are introduced with `$`.

In general the syntax is

 * `$KEYWORD`: Invoke the expansion of the keyword `KEYWORD`.
 * `${KEYWORD PARAMS...}`: Invoke with parameters.
 * `$( .... )`: Repetition (abbreviated form).
   (Note: there is no `+` or `*` after the `)`)

In all cases, `$_identifier_` is equivalent to `${_identifier_}`.

Many of the expansion keywords start with `f`, `v`, or `s` to indicate
the depth of the thing being expanded:

 * `f...`: Expand something belonging to a particular Field.

 * `v...`: Expand something belonging to a particular Variant.

 * `t...`: Expand something applying to the whole Top-level type.

In the keyword descriptions below,
`X` is used to stand in for one of `f`, `v` or `t`.

### Repetition and nesting

The driving data structure can contain multiple variants,
which can in turn contain multiple fields;
there are also attributes.
So there are repeated sub-sectons.

Correspondingly,
sections of the template, indicated by `${for ...}` and `$( ...)`,
are expanded multiple times.

What is being repeated over is specified explicitly with `${for ...}`.

When `$( ... )` is used, what is being repeated over is automatically
inferred from the content:
Each expansion keyword has a "level":
what possibly-repeated part of the driver it corresponds to.
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

### Expansion keywords

 * `$fname`, `$vname`, `$tname`:
   The name of the field, variant, or toplevel type.
   This is an the identifier (without any path or generics).

 * `$ftype`, `$ttype`:
   The type of the field, or the toplevel type.
   This contains all necessary generics.
   For the toplevel type it contains a path prefix iff
   the driver stype argument to
   `derive_adhoc! { }`
   had a path prefix.

 * `${tmeta(...)}` `${vmeta(...)}` `${fmeta(...)}`:
   Accesses macro parameters passed via `#[adhoc(...)]` attributes.

    + `${Xmeta(NAME)}`: 
      Looks for `#[adhoc(NAME=LIT)]`, and expands to `LIT`.
      `LIT` can only be a literal, and is expanded as such.

    + `${Xmeta(SUB(NAME))}`: 
      Looks for `#[adhoc(SUB(NAME=LIT))]`, and expands to `LIT`.
      The `#[adhoc()]` is parsed as
      a set of nested, comma-separated, lists.
      So this could would find `NAME` 
      in `#[adhoc(SUB1,SUB(N1,NAME=LIT,N2),SUB2)]`.
      The path can be arbitrarily deep, e.g.: `${Xmeta(L1(L2(L3(ATTR))))}`.

    + `${Xmeta(...) as SYNTYPE}`: 
      Parses `LIT` as Rust code specifying a `SYNTYPE`,
      and then expands to that.
      Unless `SYNTYPE` is `lit`, `LIT` must be a *string* literal.
      `SYNTYPE`s available are:

       * `lit`: A literal value; expands `LIT` directly;
         equivalent to not having said `as`.
       * `ty`: A type, possibly with generics etc. (`syn::Type`).

   When expanding `${Xmeta}`,
   it is an error if the value was not specified in the driver,
   and also an error if multiple values were specified.

 * `${fattrs ...}` `${vattrs ...}` `${tattrs ...}`:
   Expands to non-`#[adhoc()]` attributes.
   The attributes can be filtered:

    * `$Xattrs`: All the attributes.
    * `${Xattrs ATTRNAME, ATTRNAME2, ...}`, or
      `${Xattrs = ATTRNAME, ATTRNAME2, ...}`:
      Attributes `#[ATTRNAME1...]` and `#[ATTRNAME2...]` only.
    * `${Xattrs ! ATTRNAME, ATTRNAME2, ...}`:
      All attributes *escept* those.

   With `${Xattrs}`, unlike `${Xmeta}`,
     * The expansion is all of the attributes, including the `#[...]`;
     * All attributes, not just `#[adhoc(...)]`,, are included.
     * The attributes can be filtered by toplevel attribute name,
       but not deeply manipulated.

 * `${when CONDITION}`:
   Allowed only within repetitions, and only at the toplevel,
   before other expansions.
   Skips this repetition if the `CONDITION` is not true.

 * `${if COND1 { ... } else if COND2 { ... } else { ... }}`:
   Conditionals.  The else clause is, of course, optional.

 * `${for fields { ... }}`:
   Expands the contents once per field.

## CONDITIONS

Conditions all start with a `KEYWORD`.
They are found within `${if }` and `${when }`.

 * `fmeta(NAME)`, `vmeta(NAME)`, `tmeta(NAME)`:
   Looks for `#[adhoc(NAME)]`.

   True iff there was such an attribute.
   `Xmeta(SUB(NAME))` works, just as with the `${Xmeta ...}` expansion.

   The condition is true if there is at least one matching entry,
   and (unlike `${Xmeta}`)
   the corresponding driver attribute does not need to be a `=LIT`.

   So `Xmeta(SUB(NAME))` is true if the driver has
   `#[adhoc(SUB(NAME(INNER=...)))]` or `#[adhoc(SUB(NAME))]` or
   `#[adhoc(SUB(NAME=LIT))]` or even `#[adhoc(SUB(NAME()))]`.

 * `is_enum`: The driver data structure is an enum.

 * `false`, `true`, `not(CONDITION)`, 
   `any(COND1,COND2,...)`, `all(COND1,COND2,...)`.
