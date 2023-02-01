//! Very simple example for deriving Clone.
use derive_adhoc::{define_derive_adhoc, Adhoc};
use std::sync::Arc;
#[derive_adhoc(MyClone)]
struct DecoratedError<E> {
    context: String,
    error: Arc<E>,
}
impl<E> Clone for DecoratedError<E> {
    fn clone(&self) -> Self {
        Self {
            context: self.context.clone(),
            error: self.error.clone(),
        }
    }
}
fn main() {
    let error = std::fs::File::open("/nonexistent").unwrap_err();
    let error = DecoratedError {
        context: "open /nonexistent".into(),
        error: Arc::new(error),
    };
    let cloned_error = error.clone();
    if cloned_error.error.kind() != std::io::ErrorKind::NotFound {
        { ::std::rt::begin_panic("explicit panic") };
    }
}
