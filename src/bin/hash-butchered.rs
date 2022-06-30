// Here's a simple syntax for the Hash attribute.  I'm using it to
// imagine what parameters look like, and how a "skip" attribute might
// look, and how a "hash_with" attrbite might look.

use derive_adhoc_macros::{derive_adhoc, derive_adhoc_expand, Adhoc};

use std::hash::{Hash, Hasher};

#[derive(Adhoc)]

struct DataType {
    foo: u8,
    #[adhoc(hash(skip))]
    bar: Vec<String>,
}

derive_adhoc! {
    DataType:

    impl Hash for $ttype
    where $( ${when not(fattr(hash(skip)))}
             $ftype : Hash + )
    {
        fn hash<H : Hasher>(&self, state: &mut H) {
            $(
                ${when not(fattr(hash::skip))}
                self.$fname.hash(state);
            )
        }
    }
}

fn main() {
    let v = DataType {
        foo: 23,
        bar: vec!["hi".into()],
    };
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    v.hash(&mut hasher);
    println!("{:x}", hasher.finish());
}

// [1] The "for struct" syntax here means that only structs are supported.

// This should expand to:
/*

impl Hash for DataType where u8 : Hash {
    fn hash<H : Hasher>(&self, state: &mut H) {
        self.foo.hash(state);
    }
}

impl<S,T> Hash for Pair<S,T> where
    S: Hash, T: Hash, S: Debug, T: Debug // [3]
{
    fn hash<H : Hasher>(&self, state: &mut H) {
        self.first.hash(state);
        self.second.hsah(state);
    }
}

impl Hash for IntPair
    where usize: Hash + usize: Hash
{
    fn hash<H : Hasher>(&self, state: &mut H) {
        self.0.hash(state);
        self.1.hash(state);
    }
}
*/

// [3] Note that the "impl..where .. {}" generation here is completely magical.
//    It needs to add the <S,T> after the impl,
//    and it needs to add S:Debug and T:Debug.
