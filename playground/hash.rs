// Here's a simple syntax for the Hash attribute.  I'm using it to
// imagine what parameters look like, and how a "skip" attribute might
// look, and how a "hash_with" attrbite might look.

#[derive(Adhoc)]
#[derive_adhoc(MyHash)]
struct DataType {
    foo: u8,
    #[adhoc(hash(skip))]
    bar: Vec<String>,
}

#[derive(Adhoc)]
#[derive_adhoc(MyHash)]
struct Pair<S,T:Debug> {
    first: S,
    second: T,
}


define_derive_adhoc!{
    MyHash  => {
        impl std::hash::Hash for $type // [0]
            where $( ${if not(attr(hash::skip)) { $ty : std::hash::Hash }} )"+"+  // [1] [2]
        {
            fn hash<H : std::hash::Hasher>(&self, state: &mut H) {
                $( ${if not(attr(hash::skip)) { self.$field .hash(state); }} )*
            }
        }
    }
}

// [0] I am implicitly chosing "$type" as a thing that expands to the main
//     type.  Maybe $self would be better?
// [1] Problem: this syntax for generating a where clause is tricky:
//     - I made up an imaginary $( .. )"+"+ syntax for expanding entries
//       with an interpolated `+` separatotr.
//     - How should we make the "where" optional" when there are no params?
// [2] I'm chosing this syntax for conditional expansion:
//      ${if expr { content }}.
//      where expr ::= not(expr)
//                   | all(expr,*)
//                   | any(expr,*)
//                   | attr(path)    <-- this indicates "attribute is present"
//     I'm also choosing "hash::skip" to mean `#[adhoc(hash(skip))].


// This should expand to:

impl std::hash::Hash for DataType where u8 : std::hash::Hash {
    fn hash<H : std::hash::Hasher>(&self, state: &mut H) {
        self.foo.hash(state);
    }
}

impl<S,T:Debug> //[3]
    std::hash::Hash for Pair<S,T> where S : std::hash::Hash + T: std::hash::Hash 

{
    fn hash<H : std::hash::Hasher>(&self, state: &mut H) {
        self.first.hash(state);
        self.second.hsah(state);
    }
}

// [3] This <S,T:Debug> appears by magic here, and I'm not sure what
//     to do about it. Can we automagially generate it on our own?  It seems
//     better not to make the user type out a whole pile of crud to make
//     their derive_adhoc macro work for generic types.
