// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

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

impl Default for IdentityFilter {
    /// Create a new IdentityFilter.
    fn default() -> Self {
        Self::new()
    }
}

impl SequenceToSequenceFilter for IdentityFilter {
    fn filter<'a>(&'a self, seq: SqBValueSequence<'a>) -> Result<SqBValueSequence<'a>> {
        Ok(seq)
    }
}

#[cfg(test)]
mod tests {
    use rstest::rstest;

    use super::*;
    use crate::filter::test_util::{assert_eq_sqbvalueseqs, gen_test_sequence, SequenceType};

    #[rstest]
    fn test_identity_filter(
        #[values(
            SequenceType::Iterator,
            SequenceType::ExactSizeIterator,
            SequenceType::DoubleEndedIterator,
            SequenceType::ExactSizeDoubleEndedIterator
        )]
        seq_type: SequenceType,
        #[values(0, 1, 10)] seq_len: usize,
    ) {
        let seq = gen_test_sequence(seq_type, seq_len);
        let expected = gen_test_sequence(seq_type, seq_len);
        let filter = IdentityFilter::new();
        let filtered = filter.filter(seq).unwrap();
        assert_eq_sqbvalueseqs(filtered, expected);
    }
}
