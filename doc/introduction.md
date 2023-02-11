# `derive_adhoc` and how to make it pay

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

<!--

## Exporting templates

Explain how to declare a template that can be used from another crate?

Do we even support that well?

I don't want to have to tell people to manually `#[macro_export]` and `pub use`
a thing with a funny name.

-->


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




> Coming sections:
>
>  ## Something something lifetimes
>
>  ## Making MyClone apply to `enum`s
>
>
>
> # More advanced techniques
>
> ## Explicit repetition

> ## Conditional compilation

> ## Working with attributes

> ## Transforming names and strings


[reference]: crate::doc_template_syntax
[README]: crate
