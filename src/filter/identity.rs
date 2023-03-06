//! Items relating to when a filter is not specified in a field call in the query.

use crate::error::Result;
use crate::filter::SequenceToSequenceFilter;
use crate::sqvalue::SqBValueSequence;

/// Filter that just returns the sequence as-is. This is used when a filter is not specified for a
/// field call in the query.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct IdentityFilter;

impl IdentityFilter {
    /// Create a new IdentityFilter.
    pub fn new() -> Self {
        Self {}
    }
}

impl SequenceToSequenceFilter for IdentityFilter {
    fn filter<'a>(&'a self, seq: SqBValueSequence<'a>) -> Result<SqBValueSequence<'a>> {
        Ok(seq)
    }
}
