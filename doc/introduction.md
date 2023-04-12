# Getting started with `derive_adhoc`.

> Here I want to put an introduction about what derive_adhoc does.  For
> now I'll just link to the [README].
>
> This is a work in progress.

There are two parts to using `derive_adhoc`:
specifying _templates_ that you can use to derive new features for your
structs and enums
and then _applying_ those templates to your types.

To define a template, you use `define_derive_adhoc!`, as in

```
use derive_adhoc::define_derive_adhoc;

define_derive_adhoc! {
    NamePrinter =

    impl $ttype {
        pub fn print_name() {
            println!("The name of this type is {}", stringify!($ttype));
        }
    }
}
```

This is a very simple template: it uses a single expansion: `$ttype`.
(We'll get into more expansions, and more useful examples, later on.
For now, all you need to know
is that `$ttype` expands to the type
on which you're applying your template.)


Later on, you can apply `NamePrinter` to your own type, as in:

```
# use derive_adhoc::define_derive_adhoc;
# define_derive_adhoc! {
#    NamePrinter =
#
#    impl $ttype {
#        pub fn print_name() {
#            println!("The name of this type is {}", stringify!($ttype));
#        }
#    }
# }
use derive_adhoc::Adhoc;

#[derive(Clone, Debug, Adhoc)]
#[derive_adhoc(NamePrinter)]
pub struct MyStruct;

MyStruct::print_name();
```

## Exporting templates

But now suppose that you want to expose your template,
so that people can use it from any crate.

To do this, you use `pub` before the name of your macro.
```
pub trait NamePrinter {
    fn print_name();
}

derive_adhoc::define_derive_adhoc! {
    pub NamePrinter =
    impl $crate::NamePrinter for $ttype {
        fn print_name() {
            println!("The name of this type is {}", stringify!($ttype));
        }
    }
}
```

Note that this time,
we've defined `NamePrinter` as a trait,
and we've changed our template to refer to that trait as
`$crate::NamePrinter`.
The `$crate` syntax will expand to the name of the crate
in which our template was defined,
so that when later we expand this template,
it can find the right template.

Additionally, we need to re-export `derive_adhoc`
from our crate, so that users get the correct version:

```rust,ignore
// you might want to apply #[doc(hidden)] to this.
pub use derive_adhoc;
```

Now, when somebody wants to use our template,
they can do it like this:

```rust,ignore
// Let's pretend our crate is called name_printer.
use name_printer::{
    // This is the trait we defined...
    NamePrinter,
    // This is the macro that makes our template work.
    // (We might come up with a better syntax for this later).
    derive_adhoc_template_NamePrinter.
};
use derive_adhoc::Adhoc;

#[derive(Adhoc)]
#[derive_adhoc(NamePrinter)]
struct TheirStructure {
    // ...
}
```

## If you're only deriving once...

If you want, you can apply a template to an existing type
without having to name that template.
You might want to do this if you have a template
that you only want to apply to a single struct,
and so you don't want to bother naming it.

Supposing that you wanted to apply the template above to `MyStruct`
and `MyStruct` alone,
you could have said:

```
use derive_adhoc::{Adhoc, derive_adhoc};

#[derive(Clone, Debug, Adhoc)]
pub struct MyStruct;

derive_adhoc!{
    MyStruct:

    impl $ttype {
        pub fn print_name() {
            println!("The name of this type is {}", stringify!($ttype));
        }
    }
}
```

Of course, that's not so useful yet.
In this case, it would have been easier just to write
`impl MyStruct { pub fn print_name() { ... } }`.
But soon, we'll see how to write more interesting templates,
and how to use them to create much more interesting code.


The rest of this document will focus
on how to use derive_adhoc's template features
to your fullest advantage.
If you're the kind of person who wants to skip
straight to the reference manual,
you can find it [over here][reference].
(It's still pretty terse, but we're working on that.)

# A brief tutorial: How to write your own Clone

For the next few sections, for a toy example,
we'll be using `derive_adhoc` to define
our own version of `derive(Clone)`.
At first, it will be very simple;
later on, we'll add a few features
that Rust's `derive(Clone)` doesn't have.

> Aside:
>
> We've picked a simple trait to derive on purpose,
> so that we can focus on the features of `derive_adhoc`
> without the additional complexity
> of introducing an unfamiliar trait as well.
>
> Please let us know if this approach works for you!
> We're learning how to explain these concepts
> as we go along.

## Simple templates: fields and repetition

Let's imagine we had to write Clone from scratch for a simple structure
like this:

```
struct GiftBasket {
   n_apples: u8,
   n_oranges: u8,
   given_from: Option<String>,
   given_to: String,
}
```

We'd probably write something like this:
```
# struct GiftBasket {
#   n_apples: u8,
#   n_oranges: u8,
#   given_from: Option<String>,
#   given_to: String,
# }

impl Clone for GiftBasket {
    fn clone(&self) -> Self {
        Self {
            n_apples: self.n_apples.clone(),
            n_oranges: self.n_oranges.clone(),
            given_from: self.given_from.clone(),
            given_to: self.given_to.clone()
        }
    }
}
```

(In reality,
since `n_apples` and `n_oranges` both implement `Copy`,
you wouldn't actually call `clone()` on them.
But since the compiler knows their types,
it should be smart enough to
optimize the Clone away for you.)

If you imagine generalizing this
to any simple struct struct with named fields,
you might come up with a pseudocode template
like this one:

```text,ignore
impl Clone for ⟪Your struct⟫ {
    fn clone(&self) -> Self {
        Self {
            for each field:
                ⟪field name⟫: self.⟪field name⟫.clone(),
        }
    }
}
```

And here's how that pseudocode translates into
a `derive_adhoc` template:

```
use derive_adhoc::define_derive_adhoc;

define_derive_adhoc! {
    MyClone =

    impl Clone for $ttype {
        fn clone(&self) -> Self {
            Self {
                $( $fname : self.$fname.clone() , )
            }
        }
    }
}
```

Let's look at that template.  You've already seen `$ttype`: it expands
to the type on which you are applying the macro.  There are two new
pieces of syntax here, though: `$( ... )` and `$fname`.


In `derive_adhoc` templates, `$( ... )`  denotes repetition:
it repeats what is inside it
an "appropriate" number of times.
(We'll give a definition of "appropriate" later on.)
Since we want to clone every field in our struct,
we are repating the `field: self.field.clone() ,`
part of our implementation.

The `$fname` expansion means "the name of a field".
Which field?
Since `$fname` occurs inside `$( ... )`,
we will repeat the body of the `$( ... )` once for each
field, and expand `$fname`
to the name of a different field each time.

(Again, more advanced repetition is possible;
there's more to come.)


> ### On naming
>
> Many `derive_adhoc` expansions names start with
> `t` for **top-level**
> (whatever you are applying the template to),
> `v` for **variant**
> (a variant of an `enum`),
> or `f` for **field**
> (a single field of a struct or variant).
>
> So far, you've seen `$ttype` for "top-level type" and `$fname` for
> "field name".
>
> (We say "top-level" instead of "struct":
> later on, we'll be showing you how to apply
> derive_adhoc to `enums`.)
>
> Many `derive_adhoc` expansions end with
> a short identifier for what they contain.
> For example, `$tname` is the name of a top-level type,
> `$vname` is the name of a variant,
> and `$fname` is the name of a field.
> Whenever possible, we have tried to use the same
> identifier for the `t`, `v`, and `f` cases,
> whenever it is logical.


### Will MyClone apply to other kinds of struct?

Rust defines several kinds of struct:
structs with fields (`struct Foo {...};`),
tuple structs (`struct Foo(...);`),
and unit structs (`struct Foo;`).

If you try to apply the `MyClone` template above
to a struct with fields,
it will work just fine.
But with a tuple struct, or a unit struct,
you might expect it to fail.

Surprisingly, it will still work fine!
This isn't because of any clever trickery
from `derive_adhoc`:
it's just how Rust works.
When you use it on tuple or unit structs,
the `MyClone` template we wrote above will expand
to code like this...
which happens to be valid syntax!

```
struct TupleStruct(String, Option<String>);
impl Clone for TupleStruct {
    fn clone(&self) -> Self {
        Self {
            0: self.0.clone(),
            1: self.1.clone(),
        }
    }
}

struct UnitStruct;
impl Clone for UnitStruct {
    fn clone(&self) -> Self {
        Self {
        }
    }
}
```

This will be a common theme in what follows:
Rust often lets you use a slightly unidiomatic syntax
so that you can handle many different cases
in the same way.

## Making MyClone apply to generics

But here's a structure where our current `MyClone` implementation
will fall flat:

```
# use std::fmt::Debug;
struct MyItems<T:Clone, U>
    where U: Clone + Debug
{
    things: Vec<T>,
    items: Vec<U>
}
```

When we go to expand the template, it will generate something like:
```rust,ignore
impl Clone for MyItems { ... }
```

That isn't valid!  We need to use the generic parameters, like so:
```rust,ignore
impl<T:Clone, U> Clone for MyItems<T,U> 
    where U: Clone+Debug
{ ... }
```

We can expand our `MyClone` definition to look that way:

```
# use derive_adhoc::define_derive_adhoc;
define_derive_adhoc! {
    MyClone =

    impl<$tgens> Clone for $ttype
    where $twheres
    {
        fn clone(&self) -> Self {
            Self {
                $( $fname : self.$fname.clone() , )
            }
        }
    }
}
```

Here we meet two new expansions.
`$tgens` ("top-level generics") becomes
the generic parameters as declared on the top-level type.
(In our case, that's `$T:Clone, U`.)
`$twheres` ("top-level where clauses") becomes
the `where` constraints= as declared on the top-level type.
(In our case, that's `U: Clone+Debug`.)

Note that `$ttype` expands to the top-level _type_:
that's now `MyItems<T,U>`,
which is what we want.
If we had wanted only `MyItems`,
we would say `$tname` instead.

Will this template still work for non-parameterized types?
Again, yes!
To Rust, this syntax is perfectly fine:
```rust
struct Simple {
    a: String
}
impl<> Clone for Simple
where
{
    fn clone(&self) -> Self {
        Self {
            a: self.a.clone(),
        }
    }
}
```


## Making MyClone apply conditionally

Now, for the first time, we will make MyClone do something
that Rust's `#[derive(Clone]` does not:
it will apply only when the fields of a struct are `Clone`.

For example, suppose have a struct like this:
```
# use std::sync::Arc;
struct Indirect<T>(Arc<T>, u16);
```
If you try to derive `Clone` on it,
the compiler will generate code something like this:

```rust,ignore
impl<T: Clone> Clone for Indirect<T> { ... }
```

But that `T: Clone` constraint isn't necessary: `Arc<T>` always
implements `Clone`, so your struct could be clone unconditionally.

But using `derive_adhoc`,
you can define a template
that derives `Clone` only for the cases
where the _actual_ required constraints are met:

```
# use derive_adhoc::define_derive_adhoc;
define_derive_adhoc! {
    MyClone =

    impl<$tgens> Clone for $ttype
    where $twheres
          $( $ftype : Clone , )
    {
        fn clone(&self) -> Self {
            Self {
                $( $fname : self.$fname.clone() , )
            }
        }
    }
}
```

Here, we are using `$ftype`, the type of a field.
Since we're repeating it with `$( ... )`,
we are requiring every field to be `Clone`.

Will this work with non-generic fields,
or if the same field is used more than once?
Once again, yes!
To Rust, this is a perfectly valid example:

```rust,ignore
impl<T> Clone for Direct
where
    T: Clone,
    T: Clone,
    String: Clone
{
    ...
}
```


> This time,
> `derive_adhoc` has exactly _one_ piece cleverness at work.
> It makes sure that either `$twheres` is empty,
> or that it ends with a comma.
> That way, your template won't expand to something like
> `where U: Debug + Clone T: Clone`
> (which is a syntax eror)
> or soemthing like
> `where ,`
> (which is also a syntax error).

## Deriving for enumerations

At this point, you've probably noticed
that we've defined `MyClone` to apply to `struct`s only,
but it won't (yet) work on `enum`s.
Let's fix that!

Suppose that we have enumeration defined like this:

```
enum AllTypes {
    NoData,
    Tuple(u8, u16),
    Struct { a: String, b: String }
}
```
We want to make sure that
MyClone can recognize and re-construct
each of the three variants.

We can do that as follow
(For simplicity, we're going to ignore generics for now.)
```
# use derive_adhoc::define_derive_adhoc;
define_derive_adhoc! {
    MyClone =

    impl Clone for $ttype
    {
        fn clone(&self) -> Self {
            match self {
                $(
                    $vpat => $vtype {
                        $(
                            $fname: $fpatname.clone(),
                        )
                    },
                )
            }
        }
    }
}
```

Note that now we have two levels of nested repetition.
First, we match once for each variant.
(This is at the `$vpat` and `$vtype` level.)
Then we match once for each field of each variant.
(This is at the `$fname` and `$fpatname` level.)

Let's go over the new expansions here.
First, we have `$vpat`:
that expands to a pattern that can match and deconstruct
a single variant.
Then, we have `$vtype`:
that's the type of the variant,
suitable for use as a constructor.
Then, inside the variant, we have `$fname`:
that's our field name, which we've seen it before.
Finally, we have `$fpatname`:
that is the name of the variable that we used for this field
in the pattern that deconstructed it.

When we apply `MyClone` to our enumeration,
we get something like this:
```
# enum AllTypes { NoData, Tuple(u8, u16), Struct { a: String, b: String } }
impl Clone for AllTypes {
    fn clone(&self) -> Self {
        match self {
            AllTypes::NoData {} => AllTypes::NoData {},
            AllTypes::Tuple {
                0: f_0,
                1: f_1,
            } => AllTypes::Tuple {
                0: f_0.clone(),
                1: f_1.clone()
            },
            AllTypes::Struct {
                a: f_a,
                b: f_b,
            } => AllTypes::Struct {
                a: f_a.clone(),
                b: f_b.clone()
            },
        }
    }
}
```

> ... Or we _would_ get that, if it weren't for
> [bug #15](https://gitlab.torproject.org/Diziet/rust-derive-adhoc/-/issues/15).
> It turns out that derive_adhoc doesn't work
> for multi-field enum variants yet. ☹

Note that our template above will still work fine on a regular struct,
even though it's written for an `enum`.
If we apply `MyClone` above
to `struct Example { a: u8, b: String }`,
we get this:

```
# struct Example { a: u8, b: String }
impl Clone for Example {
    fn clone(&self) -> Self {
        match self {
            Example {
                a: f_a,
                b: f_b,
            } => Example {
                a: f_a.clone(),
                b: f_b.clone(),
            }
        }
    }
}
```

So (in this case at least)
we were able to write a single template expansion
that worked for both `struct`s and enum`s.

### Putting the generics back into our enumeration-friendly template

Now let's see how it works when we try to handle generics again.
(It's surprisingly straightforward!)

```
# use derive_adhoc::define_derive_adhoc;
define_derive_adhoc! {
    MyClone =

    impl<$tgens> Clone for $ttype
    where $( $ftype: Clone, )
          $twheres
    {
        fn clone(&self) -> Self {
            match self {
                $(
                    $vpat => $vtype {
                        $(
                            $fname: $fpatname.clone(),
                        )
                    },
                )
            }
        }
    }
}
```


Note that when we define our additional `where` clauses,
we don't have to specify separate of repetition
for variants and fields:
if we just have `$ftype` in a top-level repetition,
`derive_adhoc` will iterate over all fields in all variants.

# Some more advanced topics

Now that we've our first basic example under our belt,
let's look at some other things that `derive_adhoc` can do.

## Transforming names and strings

Often, it's useful to define new identifiers
based on existing ones,
or to convert identifiers into strings.

You _could_ use the existing [`paste`] crate for this,
or you can use a native facility provided by `derive_adhoc`.

For example, suppose that you want
to define a template that makes a "discriminant" type
for your enumerations.
You want the new type to be named `FooDiscriminant`,
where `Foo` is the name of your existing type.
While you're at it, you want to add an `is_` function
to detect each variant.

You can do that like this:
```
# use derive_adhoc::define_derive_adhoc;
define_derive_adhoc! {
    Discriminant =

    #[derive(Copy,Clone,Eq,PartialEq,Debug)]
    enum ${paste $tname Discriminant} {
        $(
            $vname,
        )
    }

    impl<$tgens> $ttype where $twheres {
       fn discriminant(&self) -> ${paste $tname Discriminant} {
          match self {
              $(
                  $vpat => ${paste $tname Discriminant}::$vname,
              )
          }
        }

        $(
            fn ${paste is_ ${snake_case $vname}} (&self) -> bool {
                self.discriminant() ==
                    ${paste $tname Discriminant} ::$vname
            }
        )
    }
}
```

Here we see a couple of new constructs.

First, we're using `${paste}`
to glue several identifiers together into one.
When we say `${paste $tname Discriminant}`,
we are generating a new identifier from `$tname`
(the type name)
and the word Discriminant.
So if the type name is `Foo`,
the new type will be called `FooDiscriminant`.

Second, we're using `${snake_case}`
to transform an identifier into `snake_case`
(that is, lowercase words separated by underscores).
We use this to turn the name of each variant (`$vname`)
into a name suitable for use in a function name.
So if a variant is called `ExampleVariant`,
`${snake_case $vname}`  will be `example_variant`,
and `${paste is_ ${snake_case $vname}}` will be
`is_example_variant`.

There are other case-changers:
  * `${pascal_case my_ident}` becomes `MyIdent`.
    You can also write this as
    `${PascalCase ..}`,
    `${upper_camel_case ..}`,
    or `${UpperCamelCase ..}`.
  * `${lower_camel_case my_ident}` becomes `myIdent`.
    You can also write this as
    `${lowerCamelCase .. }` or
    `${LowerCamelCase ..}`
  * `${shouty_snake_case MyIdent}` becomes `MY_IDENT`.
    You can also write this as
    `${SHOUTY_SNAKE_CASE ..}` or
    `${ShoutySnakeCase ..}`.
  * `${snake_case MyIdent}` becomes `my_ident`, as you've already seen.
    You can also write it as
    `${SnakeCase ..}`.

### A note on syntax

In this last section,
you've seen a new syntax for the first time.
Both `${paste ident ident..}` and `${snake_case ident}`
are special cases of the following meta-syntax,
which `derive_adhoc` uses everywhere:

`${KEYWORD ARGS.. }`

In fact, if you want,
you can use this format
for all of the expansion macros you have already seen:
`$ttype` is just a shortened form for `${ttype}`,
`$fname` is just `${fname}`,
and so on.

Some keywords,
including some of those we've already seen,
can take named arguments.
The syntax for this is:

`${KEYWORD ARGNAME=VALUE ARGNAME=VALUE...}`

> For example, we can use this syntax to give optional arguments to `$vpat`;
> see the template syntax reference for more information.

If you ever need to write a literal `$`,
you can write `$$`.




> Coming sections:
>
>  ## Something something lifetimes
>
> ## Explicit repetition

> ## Conditional compilation

> ## Working with attributes
>
> ## visibility
>
> ## `$tdef*`, `$vdef*, `$fdef*` — what it's for and why.



[reference]: crate::doc_reference
[README]: crate
[`paste`]: https://docs.rs/paste/latest/paste/
