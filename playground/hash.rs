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
struct Pair<S,T:Debug>
    where S: Debug
{
    first: S,
    second: T,
}

define_derive_adhoc!{
    MyHash = {
        impl Hash for $self
            where $( ${when not(attr(hash::skip))}
                     $ty : Hash + )*
        {
            fn hash<H : Hasher>(&self, state: &mut H) {
                $( ${when not(attr(hash::skip))}
                   self.$field.hash(state); )*
            }
        }
    }
}


// This should expand to:

impl Hash for DataType where u8 : Hash {
    fn hash<H : Hasher>(&self, state: &mut H) {
        self.foo.hash(state);
    }
}

impl<S,T> Hash for Pair<S,T> where
    S: Hash + T: Hash + S: Debug + T: Debug // [3]
{
    fn hash<H : Hasher>(&self, state: &mut H) {
        self.first.hash(state);
        self.second.hsah(state);
    }
}

// [3] Note that the "impl..where .. {}" generation here is completely magical.
//    It needs to add the <S,T> after the impl,
//    and it needs to add S:Debug and T:Debug.

