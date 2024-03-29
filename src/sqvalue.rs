// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

//! Types related to SQ values.

use crate::error::Result;
use crate::fieldcall::FieldCallInfo;
use crate::primitive::Primitive;
use crate::util::ExactSizeDoubleEndedIterator;

/// The trait that SQ types must implement to provide a generic interface for results generation.
///
/// The implementation of this generic interface is provided by the code generated in `build.rs`
/// from the schema, so the SQ types defined in the system layer do not need to implement this
/// trait directly.
///
/// The code generated in `build.rs` does, however, also generate a trait specifically for each SQ
/// type that declares a method for each of the type's fields that must be implemented.
pub trait SqValue {
    /// Convert the value into an SQ primitive type.
    fn get_primitive(&self, info: &FieldCallInfo) -> Result<Primitive>;

    /// Call a field on this value that returns a single value.
    fn get_single(&self, info: &FieldCallInfo) -> Result<SqBValue>;

    /// Call a field on this value that returns an optional value.
    fn get_option(&self, info: &FieldCallInfo) -> Result<Option<SqBValue>>;

    /// Call a field on this value that returns a sequence of values.
    fn get_sequence<'a>(&'a self, info: &'a FieldCallInfo) -> Result<SqBValueSequence<'a>>;

    /// Get the name of this value's type.
    fn get_type_name(&self) -> &'static str;
}

/// A Boxed `SqValue` (to provide polymorphism).
pub type SqBValue = Box<dyn SqValue>;

/// A Boxed `Iterator` (to provide polymorphism).
pub type DynIterator<'a, T> = Box<dyn Iterator<Item = T> + 'a>;

/// A Boxed `ExactSizeIterator` (to provide polymorphism).
pub type DynExactSizeIterator<'a, T> = Box<dyn ExactSizeIterator<Item = T> + 'a>;

/// A Boxed `DoubleEndedIterator` (to provide polymorphism).
pub type DynDoubleEndedIterator<'a, T> = Box<dyn DoubleEndedIterator<Item = T> + 'a>;

/// A Boxed `ExactSizeDoubleEndedIterator` (to provide polymorphism).
pub type DynExactSizeDoubleEndedIterator<'a, T> =
    Box<dyn ExactSizeDoubleEndedIterator<Item = T> + 'a>;

/// An iterator with its concrete type erased (for polymorphism) but its "category" preserved to
/// allow algorithms to switch behaviour based on the category.
pub enum DynSequence<'a, T> {
    Iterator(DynIterator<'a, T>),
    ExactSizeIterator(DynExactSizeIterator<'a, T>),
    DoubleEndedIterator(DynDoubleEndedIterator<'a, T>),
    ExactSizeDoubleEndedIterator(DynExactSizeDoubleEndedIterator<'a, T>),
}

impl<'a, T: 'a> DynSequence<'a, T> {
    /// Create an empty sequence.
    pub fn empty() -> Self {
        Self::Iterator(Box::new(std::iter::empty()))
    }
}

impl<'a, T: 'a> Default for DynSequence<'a, T> {
    /// create an empty sequence.
    fn default() -> Self {
        Self::empty()
    }
}

// Implement `Iterator` for `DynSequence` so that if we don't care about the category we can still
// use `Iterator` functionality.
impl<'a, T> Iterator for DynSequence<'a, T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Iterator(it) => it.next(),
            Self::ExactSizeIterator(it) => it.next(),
            Self::DoubleEndedIterator(it) => it.next(),
            Self::ExactSizeDoubleEndedIterator(it) => it.next(),
        }
    }
}

/// A boxed `Iterator` used for returning sequences from methods of SQ types (wrapped in
/// `SqValueSequence::Iterator`). The `Item` type is an `anyhow::Result` so that implementors of SQ
/// types don't have to add error variants to `error::Error`.
pub type SqValueIterator<'a, T> = DynIterator<'a, anyhow::Result<T>>;

/// `ExactSizeIterator` version of `SqValueIterator`.
pub type SqValueExactSizeIterator<'a, T> = DynExactSizeIterator<'a, anyhow::Result<T>>;

/// `DoubleEndedIterator` version of `SqValueIterator`.
pub type SqValueDoubleEndedIterator<'a, T> = DynDoubleEndedIterator<'a, anyhow::Result<T>>;

/// `ExactSizeDoubleEndedIterator` version of `SqValueIterator`.
pub type SqValueExactSizeDoubleEndedIterator<'a, T> =
    DynExactSizeDoubleEndedIterator<'a, anyhow::Result<T>>;

/// A sequence of `anyhow::Results` for returning sequences from methods of SQ types. This (wrapped
/// in `anyhow::Result`) is the return type of SQ type methods corresponding to the type's fields
/// that return sequences.
pub type SqValueSequence<'a, T> = DynSequence<'a, anyhow::Result<T>>;

/// A boxed `Iterator` used for returning sequences from `SqValue::get_sequence` (wrapped in
/// `SqBValueSequence::Iterator`).
///
/// The `get_sequence` method converts into this type from the `SqValueIteartor` returned by the SQ
/// type's method corresponding to the requested field. This erases the type of the
/// `SqValueIterator`'s items for polymorphism.
///
/// The concrete functions for getting field data don't return `SqBValueIterator` (wrapped in
/// `SqBValueSequence::Iterator`) directly so that the item type can be checked by the compiler and
/// so that the implementors of those methods don't have to clutter their code with more
/// `Box::new`s.
///
/// The result type is also converted from `anyhow::Result` into `error::Result` to be consistent
/// with the rest of the code and add error context.
pub type SqBValueIterator<'a> = DynIterator<'a, Result<SqBValue>>;

/// `ExactSizeIterator` version of `SqBValueIterator`.
pub type SqBValueExactSizeIterator<'a> = DynExactSizeIterator<'a, Result<SqBValue>>;

/// `DoubleEndedIterator` version of `SqBValueIterator`.
pub type SqBValueDoubleEndedIterator<'a> = DynDoubleEndedIterator<'a, Result<SqBValue>>;

/// `ExactSizeDoubleEndedIterator` version of `SqBValueIterator`.
pub type SqBValueExactSizeDoubleEndedIterator<'a> =
    DynExactSizeDoubleEndedIterator<'a, Result<SqBValue>>;

// A sequence of `error:Result`s for returning sequences from `SqValue::get_sequence`.
pub type SqBValueSequence<'a> = DynSequence<'a, Result<SqBValue>>;

impl<'a> SqBValueSequence<'a> {
    /// Convert an `SqValueSequence<'a, T>' into an `SqBValueSequence<'a>`.
    pub fn from_sq_value_sequence<T>(
        seq: SqValueSequence<'a, T>,
        call_info: &'a FieldCallInfo<'a>,
    ) -> Self
    where
        T: SqValue + 'static,
    {
        match seq {
            SqValueSequence::Iterator(it) => {
                SqBValueSequence::Iterator(Box::new(it.map(|result| -> Result<SqBValue> {
                    result
                        .map(|v| -> SqBValue { Box::new(v) })
                        .map_err(|e| call_info.field_call_error(e))
                })))
            }
            SqValueSequence::ExactSizeIterator(it) => SqBValueSequence::ExactSizeIterator(
                Box::new(it.map(|result| -> Result<SqBValue> {
                    result
                        .map(|v| -> SqBValue { Box::new(v) })
                        .map_err(|e| call_info.field_call_error(e))
                })),
            ),
            SqValueSequence::DoubleEndedIterator(it) => SqBValueSequence::DoubleEndedIterator(
                Box::new(it.map(|result| -> Result<SqBValue> {
                    result
                        .map(|v| -> SqBValue { Box::new(v) })
                        .map_err(|e| call_info.field_call_error(e))
                })),
            ),
            SqValueSequence::ExactSizeDoubleEndedIterator(it) => {
                SqBValueSequence::ExactSizeDoubleEndedIterator(Box::new(it.map(
                    |result| -> Result<SqBValue> {
                        result
                            .map(|v| -> SqBValue { Box::new(v) })
                            .map_err(|e| call_info.field_call_error(e))
                    },
                )))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use rstest::rstest;

    use super::*;
    use crate::schema;
    use crate::test_util::{
        fake_field_call_ast, field_call_result_to_primitive, gen_sqbvalue_seq, gen_sqint_seq,
        get_seq_type, SequenceType,
    };

    #[rstest]
    fn test_dyn_sequence_next(
        #[values(
            SequenceType::Iterator,
            SequenceType::ExactSizeIterator,
            SequenceType::DoubleEndedIterator,
            SequenceType::ExactSizeDoubleEndedIterator
        )]
        seq_type: SequenceType,
    ) {
        let mut seq = gen_sqbvalue_seq(seq_type, 5);
        assert_eq!(
            field_call_result_to_primitive(seq.next().unwrap()),
            Primitive::I128(0)
        );
        assert_eq!(
            field_call_result_to_primitive(seq.next().unwrap()),
            Primitive::I128(1)
        );
        assert_eq!(
            field_call_result_to_primitive(seq.next().unwrap()),
            Primitive::I128(2)
        );
        assert_eq!(
            field_call_result_to_primitive(seq.next().unwrap()),
            Primitive::I128(3)
        );
        assert_eq!(
            field_call_result_to_primitive(seq.next().unwrap()),
            Primitive::I128(4)
        );
        assert!(seq.next().is_none());
    }

    #[test]
    fn test_dyn_sequence_empty() {
        let mut empty = DynSequence::<i128>::empty();
        assert_eq!(empty.next(), None);
    }

    #[test]
    fn test_dyn_sequence_default() {
        let mut def = DynSequence::<i128>::default();
        assert_eq!(def.next(), None);
    }

    #[rstest]
    fn test_sq_bvalue_sequence_from_sq_value_sequence(
        #[values(
            SequenceType::Iterator,
            SequenceType::ExactSizeIterator,
            SequenceType::DoubleEndedIterator,
            SequenceType::ExactSizeDoubleEndedIterator
        )]
        seq_type: SequenceType,
    ) {
        let sqint_seq = gen_sqint_seq(seq_type, 5);
        let field_call_ast = fake_field_call_ast();
        let call_info = FieldCallInfo::new(&field_call_ast, schema::root_field()).unwrap();
        let sqbvalue_seq =
            SqBValueSequence::from_sq_value_sequence(gen_sqint_seq(seq_type, 5), &call_info);

        // We should retain the same iterator category
        assert_eq!(get_seq_type(&sqint_seq), get_seq_type(&sqbvalue_seq));

        let sqint_seq_prims = sqint_seq
            .map(|res| res.unwrap().get_primitive(&call_info).unwrap())
            .collect::<Vec<_>>();
        let sqbvalue_seq_prims = sqbvalue_seq
            .map(|res| res.unwrap().get_primitive(&call_info).unwrap())
            .collect::<Vec<_>>();
        assert_eq!(sqint_seq_prims, sqbvalue_seq_prims);
    }
}
