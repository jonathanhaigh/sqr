//! Types and functions for getting requested data from the system.

use crate::sqvalue::SqBValue;

pub mod sqbool;
pub mod sqfile;
pub mod sqint;
pub mod sqosstring;
pub mod sqpath;
pub mod sqroot;
pub mod sqstring;

/// Get the root field, where all queries start.
pub fn root() -> SqBValue {
    Box::new(sqroot::SqRoot::new())
}

// For each SQ type mentioned in the schema, `build.rs` creates a trait defining the methods for
// the type's fields that must be implemented.
include!(concat!(env!("OUT_DIR"), "/field_traits.gen.rs"));

// For each SQ type mentioned in the schema, `build.rs` generates an implementation of the `Field`
// trait.
include!(concat!(env!("OUT_DIR"), "/field_base_impls.gen.rs"));
