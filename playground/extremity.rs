
#[derive(Adhoc)]
enum Extremity {
    Hand { num_fingers: usize },
    Foot { num_toes: usize },
    Nose,
}

fn main(){
    derive_adhoc!{
        Extremity:
        $(
            ${for fields} // [0]
            println!("variant name {:?} field name {:?}",
                     stringify!($vname),
                     stringify!($fname));
        )
    }
}

// [0]
//    This could be done with ${for fields { ... } }
//    but that's another layer of rightward drift.
