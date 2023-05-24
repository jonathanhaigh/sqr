// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

//! Items relating to index filters in the query (e.g. `path.children[0]`).

use std::collections::VecDeque;

use crate::ast;
use crate::error::{AddFieldCallContext, Error, Result};
use crate::fieldcall::FieldCallInfo;
use crate::filter::SequenceToSingleFilter;
use crate::sqvalue::{
    SqBValue, SqBValueDoubleEndedIterator, SqBValueExactSizeDoubleEndedIterator,
    SqBValueExactSizeIterator, SqBValueIterator, SqBValueSequence,
};
use crate::util::{CategorizedInt, IterUtils};

#[must_use]
#[derive(Debug)]
/// Filter corresponding to a list index in the query. E.g. `path.children[0]`.
pub struct IndexFilter<'a> {
    call_info: &'a FieldCallInfo<'a>,
    int_literal: &'a ast::IntLiteral,
}

impl<'a> IndexFilter<'a> {
    /// Create a new filter corresponding to a list index in the query.
    ///
    /// # Parameters:
    /// - `call_info`: info about the field call that contains the filter.
    /// - `int_literal`: the AST node corresponding to the index in the query. E.g. the `0` in
    ///   `path.children[0]`.
    pub fn new(call_info: &'a FieldCallInfo<'a>, int_literal: &'a ast::IntLiteral) -> Self {
        Self {
            call_info,
            int_literal,
        }
    }

    fn raw_index(&self) -> i128 {
        self.int_literal.value
    }

    fn index(&self) -> Result<CategorizedInt> {
        CategorizedInt::try_from(self.int_literal.value).map_err(|e| e.add_context(self.call_info))
    }

    fn index_out_of_bounds_error(&self, len: usize) -> Box<Error> {
        Box::new(Error::FilterIndexOutOfBounds {
            span: self.int_literal.span,
            type_name: self.call_info.type_name().to_owned(),
            field_name: self.call_info.field_name().to_owned(),
            index: self.raw_index(),
            len,
        })
    }
}

impl<'a> SequenceToSingleFilter for IndexFilter<'a> {
    /// Get the nth element of a sequence, where n is the index in a list index in the query.
    fn filter(&self, seq: SqBValueSequence) -> Result<SqBValue> {
        match seq {
            SqBValueSequence::Iterator(it) => IteratorIndexFilter::new(self).filter(it),
            SqBValueSequence::ExactSizeIterator(it) => {
                ExactSizeIteratorIndexFilter::new(self).filter(it)
            }
            SqBValueSequence::DoubleEndedIterator(it) => {
                DoubleEndedIteratorIndexFilter::new(self).filter(it)
            }
            SqBValueSequence::ExactSizeDoubleEndedIterator(it) => {
                ExactSizeDoubleEndedIteratorIndexFilter::new(self).filter(it)
            }
        }
    }
}

#[must_use]
#[derive(Debug)]
struct IteratorIndexFilter<'a> {
    parent: &'a IndexFilter<'a>,
}

impl<'a> IteratorIndexFilter<'a> {
    fn new(parent: &'a IndexFilter<'a>) -> Self {
        Self { parent }
    }
}

impl<'a> IteratorIndexFilter<'a> {
    fn filter(&self, mut iter: SqBValueIterator) -> Result<SqBValue> {
        match self.parent.index()? {
            CategorizedInt::Negative(uminus_index) => {
                let mut buff = VecDeque::new();
                for (index, element) in iter.enumerate() {
                    if index >= uminus_index {
                        buff.pop_front();
                    }
                    buff.push_back(element);
                }

                if uminus_index > buff.len() {
                    return Err(self.parent.index_out_of_bounds_error(buff.len()));
                }

                buff.swap_remove_back(buff.len() - uminus_index).unwrap()
            }

            CategorizedInt::NonNegative(uindex) => iter
                .nth_or_len(uindex)
                .map_err(|len| self.parent.index_out_of_bounds_error(len))?,
        }
    }
}

#[must_use]
#[derive(Debug)]
struct ExactSizeIteratorIndexFilter<'a> {
    parent: &'a IndexFilter<'a>,
}

impl<'a> ExactSizeIteratorIndexFilter<'a> {
    fn new(parent: &'a IndexFilter<'a>) -> Self {
        Self { parent }
    }
}

impl<'a> ExactSizeIteratorIndexFilter<'a> {
    fn filter(&self, mut iter: SqBValueExactSizeIterator) -> Result<SqBValue> {
        let uindex = match self.parent.index()? {
            CategorizedInt::NonNegative(uindex) => uindex,
            CategorizedInt::Negative(uminus_index) => {
                if uminus_index > iter.len() {
                    return Err(self.parent.index_out_of_bounds_error(iter.len()));
                }
                iter.len() - uminus_index
            }
        };

        iter.nth_or_len(uindex)
            .map_err(|len| self.parent.index_out_of_bounds_error(len))?
    }
}

#[must_use]
#[derive(Debug)]
struct DoubleEndedIteratorIndexFilter<'a> {
    parent: &'a IndexFilter<'a>,
}

impl<'a> DoubleEndedIteratorIndexFilter<'a> {
    fn new(parent: &'a IndexFilter<'a>) -> Self {
        Self { parent }
    }
}

impl<'a> DoubleEndedIteratorIndexFilter<'a> {
    fn filter(&self, mut iter: SqBValueDoubleEndedIterator) -> Result<SqBValue> {
        match self.parent.index()? {
            CategorizedInt::NonNegative(uindex) => iter.nth_or_len(uindex),
            CategorizedInt::Negative(uminus_index) => iter.nth_back_or_len(uminus_index - 1),
        }
        .map_err(|len| self.parent.index_out_of_bounds_error(len))?
    }
}

#[must_use]
#[derive(Debug)]
struct ExactSizeDoubleEndedIteratorIndexFilter<'a> {
    parent: &'a IndexFilter<'a>,
}

impl<'a> ExactSizeDoubleEndedIteratorIndexFilter<'a> {
    fn new(parent: &'a IndexFilter<'a>) -> Self {
        Self { parent }
    }
}

impl<'a> ExactSizeDoubleEndedIteratorIndexFilter<'a> {
    fn filter(&self, mut iter: SqBValueExactSizeDoubleEndedIterator) -> Result<SqBValue> {
        // TODO: do better.
        let uindex = match self.parent.index()? {
            CategorizedInt::NonNegative(uindex) => {
                if uindex + 1 > iter.len() {
                    return Err(self.parent.index_out_of_bounds_error(iter.len()));
                }
                uindex
            }
            CategorizedInt::Negative(uminus_index) => {
                if uminus_index > iter.len() {
                    return Err(self.parent.index_out_of_bounds_error(iter.len()));
                }
                iter.len() - uminus_index
            }
        };

        // If the index is closer to the front of the sequence, iterate from the front, otherwise
        // iterate from the back
        if iter.len() / 2 >= uindex {
            iter.nth_or_len(uindex)
        } else {
            iter.nth_back_or_len(iter.len() - uindex - 1)
        }
        .map_err(|len| self.parent.index_out_of_bounds_error(len))?
    }
}

#[cfg(test)]
mod tests {

    use pretty_assertions::assert_eq;
    use rstest::rstest;

    use super::*;
    use crate::primitive::Primitive;
    use crate::schema;
    use crate::test_util::{fake_field_call_ast, fake_int_literal, gen_sqbvalue_seq, SequenceType};

    #[rstest]
    #[case::iter_index_first(10, 0, Some(0))]
    #[case::iter_index_last(10, 9, Some(9))]
    #[case::iter_index_mid(10, 5, Some(5))]
    #[case::iter_index_neg_first(10, -10, Some(0))]
    #[case::iter_index_neg_last(10, -1, Some(9))]
    #[case::iter_index_neg_mid(10, -5, Some(5))]
    #[case::iter_index_before_first(10, -11, None)]
    #[case::iter_index_after_last(10, 10, None)]
    fn test_index(
        #[values(
            SequenceType::Iterator,
            SequenceType::ExactSizeIterator,
            SequenceType::DoubleEndedIterator,
            SequenceType::ExactSizeDoubleEndedIterator
        )]
        seq_type: SequenceType,

        #[case] seq_len: usize,

        #[case] index: i128,

        #[case] expected: Option<i128>,
    ) {
        let field_call_ast = fake_field_call_ast();
        let call_info = FieldCallInfo::new(&field_call_ast, schema::root_field()).unwrap();
        let seq = gen_sqbvalue_seq(seq_type, seq_len);
        let int_literal = fake_int_literal(index);
        let filter = IndexFilter::new(&call_info, &int_literal);

        match expected {
            Some(v) => assert_eq!(
                filter
                    .filter(seq)
                    .unwrap()
                    .get_primitive(&call_info)
                    .unwrap(),
                Primitive::from(v)
            ),
            None => assert!(filter.filter(seq).is_err()),
        }
    }
}
