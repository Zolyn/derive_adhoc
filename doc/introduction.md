# Introduction to derive-adhoc

`derive-adhoc` allows you to write `#[derive]` macros
-- macros driven by Rust data structures -
by writing templates in a fairly straightforward template language.

<!--##toc##-->
   * [Getting started with `derive_adhoc`.](#getting-started-with-derive_adhoc)
      * [Exporting templates](#exporting-templates)
      * [If you're only deriving once...](#if-youre-only-deriving-once)
   * [A brief tutorial: How to write your own Clone](#a-brief-tutorial-how-to-write-your-own-clone)
      * [Simple templates: fields and repetition](#simple-templates-fields-and-repetition)
      * [Making MyClone apply to generics](#making-myclone-apply-to-generics)
      * [Making MyClone apply conditionally](#making-myclone-apply-conditionally)
      * [Deriving for enumerations](#deriving-for-enumerations)
   * [Some more advanced topics](#some-more-advanced-topics)
      * [Transforming names and strings](#transforming-names-and-strings)
   * [Another example: Defining a constructor function.](#another-example-defining-a-constructor-function)
      * [Marking a template's limitations](#marking-a-templates-limitations)
      * [Working with visibility](#working-with-visibility)
      * [Using attributes to make a template take arguments](#using-attributes-to-make-a-template-take-arguments)
      * [Getting started with conditionals](#getting-started-with-conditionals)
      * [More complicated conditionals](#more-complicated-conditionals)
   * [Other features](#other-features)

## Getting started with `derive_adhoc`.

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

### Exporting templates

But now suppose that you want to expose your template,
so that people can use it from any crate.

To do this, you use `pub` before the name of your macro.
```
pub trait NamePrinter {
    fn print_name();
}

derive_adhoc::define_derive_adhoc! {
    /// Derives `NamePrinter`, providing `print_name`
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

We've also added a doc comment,
which will appear in the public API documentation for our crate.

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

### If you're only deriving once...

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

## A brief tutorial: How to write your own Clone

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

### Simple templates: fields and repetition

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


#### Will MyClone apply to other kinds of struct?

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

### Making MyClone apply to generics

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


### Making MyClone apply conditionally

Now, for the first time, we will make MyClone do something
that Rust's `#[derive(Clone)]` does not:
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

### Deriving for enumerations

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

#### Putting the generics back into our enumeration-friendly template

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

## Some more advanced topics

Now that we've our first basic example under our belt,
let's look at some other things that `derive_adhoc` can do.

### Transforming names and strings

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
    `${upper_camel_case ..}`.
  * `${lower_camel_case my_ident}` becomes `myIdent`.
  * `${shouty_snake_case MyIdent}` becomes `MY_IDENT`.
  * `${snake_case MyIdent}` becomes `my_ident`, as you've already seen.

#### A note on syntax

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


## Another example: Defining a constructor function.

In this section,
we'll be using another example to demonstrate
more of what `derive_adhoc` can do.

We'll be building a `Constructor` template
to define a `new()` function for a struct,
without having to write out all of its arguments.

Let's start with the following (struct-only) template:

```
# use derive_adhoc::define_derive_adhoc;
define_derive_adhoc! {
   Constructor =

   impl<$tgens> $ttype where $twheres {
      pub fn new( $( $fname: $ftype , ) ) -> Self {
          Self {
              $( $fname , )
          }
      }
   }
}
```

When you apply the above template to a type like this:

```
# use derive_adhoc::define_derive_adhoc;
# define_derive_adhoc! {
#   Constructor =
#
#   impl<$tgens> $ttype where $twheres {
#       pub fn new( $( $fname: $ftype , ) ) -> Self {
#           Self {
#               $( $fname , )
#           }
#       }
#   }
# }
use derive_adhoc::Adhoc;
#[derive(Adhoc)]
#[derive_adhoc(Constructor)]
struct Ex<A> {
  a: f64,
  b: A,
  c: String
}
```

You'll get a constructor like this:
```
# struct Ex<A> { a: f64, b: A, c: String }
impl<A> Ex<A> {
    pub fn new( a: f64, b: A, c: String ) -> Self {
        Self { a, b, c }
    }
}
```

So far, there aren't any new techniques at work here.
We'll add some more down below.


### Marking a template's limitations

The template above doesn't work for enumerations.
If you try to apply it to one, you'll get
a not-entirely-helpful error message.

In earlier examples,
we've shown how to make templates
that apply to enums as well as structs.
But let's say that in this case,
we want our template to be struct-only.

We can tell `derive_adhoc` about this restriction,
to help it generate more useful error messages:

```
# use derive_adhoc::define_derive_adhoc;
define_derive_adhoc! {
   Constructor for struct = // (1)

   impl<$tgens> $ttype where $twheres {
      pub fn new( $( $fname: $ftype , ) ) -> Self {
          Self {
              $( $fname , )
          }
      }
   }
}
```

(Note the use of `for struct` above at `// (1)`.)

Now if we try to apply our template to an enum,
we'll get a more useful error:


```text,ignore
error: template defined for struct, but applied to enum
```

### Working with visibility

Our `Constructor` template above doesn't really make sense
if it's applied to a non-public type:
Rust may even complain that we're declaring
a public function that ruturns a private type!

Let's fix this, and have it give our constructor
the same visibility as the type itself:

```
# use derive_adhoc::define_derive_adhoc;
define_derive_adhoc! {
   Constructor for struct =

   impl<$tgens> $ttype where $twheres {
      $tvis fn new( $( $fname: $ftype , ) ) -> Self {
          Self {
              $( $fname , )
          }
      }
   }
}
```

Here instead of saying `pub fn new`,
we said `$tvis fn new`.
The `$tvis` keyword will expand
to the visibility of the top-level type.

There is a similar similar `$fvis`
that expands to the visibility of the current field.

(Since enums variants are always visible, there is no `$vvis`.)

### Using attributes to make a template take arguments

Let's suppose we want to make our `Constructor` template
a little more flexible:
we'd like to be able to give the `new` function a different name.

We could do this as follows:

```
# use derive_adhoc::define_derive_adhoc;
define_derive_adhoc! {
   Constructor for struct =

   impl<$tgens> $ttype where $twheres {
      pub fn ${tmeta(newfn)} // (1)
      ( $( $fname: $ftype , ) ) -> Self {
          Self {
              $( $fname , )
          }
      }
   }
}

use derive_adhoc::Adhoc;
#[derive(Adhoc)]
#[derive_adhoc(Constructor)]
#[adhoc(newfn="construct_example")]
struct Example {
    a: f64,
    b: String
}
```

Here, instead of specifying "new"
for the method name in our template,
we give the name as `${tmeta(newfn)}`.
This tells the template to look for an
`#[adhoc(newfn="...")]` attribute on the type,
and to use the value of that attribute
in place of the keyword.

If we want our attribute to be more namespaced,
we can instead say something like
`${tmeta(Constructor(newfn = "..."))}`.
If we do, the template will look for an attribute like
`#[adhoc(Constructor(newfn = "..."))]`.


The `$tmeta` keyword that we used here
tells the template
to look at the `#[adhoc]` attributes for the _type_.
We can, instead, use `$vmeta`
to look for `#[adhoc]` attributes for the current _variant_,
or `$fmeta` to
to look for `#[adhoc]` attributes for the current _field_.

 <!--
> TODO: Is this the right way to talk about "as lit" and "as ty"?
> I'm thinking not yet.
 -->

### Getting started with conditionals

In the example above,
we made it possible to rename the "new" function
generated by our template.
But our approach is flawed:
if the user _doesn't_ provide
the `#[adhoc(newfn)]` adhoc attribute,
the template won't make a function name at all!

Let's show how to fix that:
```
# use derive_adhoc::define_derive_adhoc;
define_derive_adhoc! {
   Constructor for struct =

   impl<$tgens> $ttype where $twheres {
   pub fn
        ${if tmeta(newfn) { ${tmeta(newfn)} } else { new } } // (1)
     ( $( $fname: $ftype , ) ) -> Self {
          Self {
              $( $fname , )
          }
      }
   }
}
```

Have a look at the line marked with `// (1)`.
It introduces a new concept: _conditional expansion_.
The `${if ...}` keyword checks whether a given _condition_ is true.
If it is, then it expands to one of its arguments.
Otherwise, it expands to an "else" argument (if any).

> Also, you can chain `$if`s, as in
> `${if COND1 { ... } else if COND2 { ... } else { ... }`
> and so on!

Here, the condition is `tmeta(newfn)`.
That condition is true if the current type
has an `#[adhoc(newfn)]` attribute,
and false otherwise.
There are also `vmeta` and `fmeta` attributes
to detect `#[adhoc(..)]` attributes
on variants and fields respectively.

### More complicated conditionals

Frequently, we'd like our template
to behave in different ways different fields.
For example, let's suppose that we want our template
to be able to set fields to their default values,
and not take them as arguments.

We could do this with an explicit conditional for each field:
```
# use derive_adhoc::define_derive_adhoc;
define_derive_adhoc! {
   Constructor =

   impl<$tgens> $ttype where $twheres {
     pub fn new
     ( $(
          ${when not(fmeta(Constructor(default))) } // (1)
          $fname: $ftype ,
        ) ) -> Self {
          Self {
              $( $fname:
                  ${if fmeta(Constructor(default)) { Default::default() }
                  else { $fname } }
                 , )
          }
      }
   }
}

use derive_adhoc::Adhoc;
#[derive(Adhoc)]
#[derive_adhoc(Constructor)]
struct Foo {
    #[adhoc(Constructor(default))]
    s: Vec<String>,
    n: u32,
}
```

Here we're using a new construct: `$when`.
It's only valid inside a loop like `$( ... )`.
It causes the output of the loop to be surpressed
whenever the condition is not true.

The condition in this cases is `not(fmeta(Constructor(default)))`.
You've seen `fmeta` before;
`not` is just how we express negation.
All together, this `$when` keyword causes each field
that has `#[adhoc(Constructor(default))]` applied to it
to be omitted from the list of arguments
to the `new()` function.

You can use other boolean operators in conditions too:
there is an `any(...)` that is true
whenever at least one of its arguments is true,
and an `all(...)` that is true
when _all_ of its arguments are true.


## Other features

derive-adhoc has many more features,
that aren't yet explained in this tutorial.
For example:

 * `is_enum`, `is_struct`, `is_union`;
   `v_is_unit`, `is_named`, `is_tuple`;
   and `fvis`, `tvis`:
   more conditions for dealing with various cases by hand.

 * `$tdef*`, `$vdef*`, `$fdef*`
   for defining a new data structure
   in terms of features of the input data structure,
   and `$Xattrs` for passing through attributes.

 * `${select1}`
   can help with writing careful templates
   that will reject incoherent inputs.

Some of these can be seen in action in test cases in
[our source repo](https://gitlab.torproject.org/Diziet/rust-derive-adhoc/-/tree/main/tests/expand)

Full details are in the [reference].

<!--

 TODO:

> ## Explicit repetition
>
>   (When to use `${for}`?)
>
> ## What else am I missing?
>
> ## Links
>
>  - Link to more worked and commented examples.
>    - Try to clone some proc-macros that we use a lot, and see
>      if we can.
>  - Link to each reference section.

-->

[reference]: crate::doc_reference
[README]: crate
[`paste`]: https://docs.rs/paste/latest/paste/
