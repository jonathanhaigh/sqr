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
    use super::*;
    use crate::filter::test_util::{assert_eq_sqbvalueseqs, gen_test_sequence, SequenceType};

    macro_rules! identity_filter_test {
        ($name:ident, $seq_type:ident, $seq_len:expr) => {
            #[test]
            fn $name() {
                let seq = gen_test_sequence(SequenceType::$seq_type, $seq_len);
                let expected = gen_test_sequence(SequenceType::$seq_type, $seq_len);
                let filter = IdentityFilter::new();
                let filtered = filter.filter(seq).unwrap();
                assert_eq_sqbvalueseqs(filtered, expected);
            }
        };
    }

    identity_filter_test!(iter_identity_empty, Iterator, 0);
    identity_filter_test!(es_iter_identity_empty, ExactSizeIterator, 0);
    identity_filter_test!(de_iter_identity_empty, DoubleEndedIterator, 0);
    identity_filter_test!(esde_iter_identity_empty, ExactSizeDoubleEndedIterator, 0);

    identity_filter_test!(iter_identity_single, Iterator, 1);
    identity_filter_test!(es_iter_identity_single, ExactSizeIterator, 1);
    identity_filter_test!(de_iter_identity_single, DoubleEndedIterator, 1);
    identity_filter_test!(esde_iter_identity_single, ExactSizeDoubleEndedIterator, 1);

    identity_filter_test!(iter_identity_ten, Iterator, 10);
    identity_filter_test!(es_iter_identity_ten, ExactSizeIterator, 10);
    identity_filter_test!(de_iter_identity_ten, DoubleEndedIterator, 10);
    identity_filter_test!(esde_iter_identity_ten, ExactSizeDoubleEndedIterator, 10);
}
