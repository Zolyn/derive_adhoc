//! Very simple example for deriving Clone.

use derive_adhoc::{define_derive_adhoc, Adhoc};
use std::sync::Arc;

// Very simple `Clone`
//
// Useful because it doesn't infer Clone bounds on generic type
// parameters, like std's derive of Clone does.  Instead, it
// unconditionally attempts to implement Clone.
//
// Only works on `struct { }` structs.
//
// (This does a small subset of what the educe crate's `Clone` does.)
define_derive_adhoc!{
    MyClone =

    impl<$tgens> Clone for $ttype {
        fn clone(&self) -> Self {
            Self {
                $(
                    $fname: self.$fname.clone(),
                )
            }
        }
    }
}

// If we were to `#[derive(Clone)]`, DecoratedError<io::Error> wouldn't
// be Clone, because io::Error isn't, even though the Arc means we can clone.
#[derive(Adhoc)]
#[derive_adhoc(MyClone)]
struct DecoratedError<E> {
    context: String,
    error: Arc<E>,
}

fn main() {
    let error = std::fs::File::open("/nonexistent").unwrap_err();
    let error = DecoratedError {
        context: "open /nonexistent".into(),
        error: Arc::new(error),
    };
    let cloned_error = error.clone();

    // This generates much less macro output than assert_eq!
    if cloned_error.error.kind() != std::io::ErrorKind::NotFound {
        panic!();
    }
}
