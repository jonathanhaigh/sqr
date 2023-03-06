use strum_macros::{EnumIter, FromRepr};

use crate::ast;
use crate::error::Result;
use crate::sqvalue::{SqBValue, SqBValueSequence};
use crate::system::sqint::SqInt;

#[derive(Clone, Copy, Debug, EnumIter, Eq, FromRepr, PartialEq)]
pub enum SequenceType {
    Iterator,
    ExactSizeIterator,
    DoubleEndedIterator,
    ExactSizeDoubleEndedIterator,
}

pub fn gen_test_sequence(seq_type: SequenceType, len: usize) -> SqBValueSequence<'static> {
    // Use a range of i32 rather than i64 because Range<i64> doesn't implement
    // ExactSizeIterator (See https://github.com/rust-lang/rust/issues/22047) for why.
    let range = 0..i32::try_from(len).unwrap();
    fn i32_to_field_call_result(num: i32) -> Result<SqBValue> {
        Ok(Box::new(SqInt::new(num.into())))
    }

    match seq_type {
        SequenceType::Iterator => {
            SqBValueSequence::Iterator(Box::new(range.map(i32_to_field_call_result)))
        }
        SequenceType::ExactSizeIterator => {
            SqBValueSequence::ExactSizeIterator(Box::new(range.map(i32_to_field_call_result)))
        }
        SequenceType::DoubleEndedIterator => {
            SqBValueSequence::DoubleEndedIterator(Box::new(range.map(i32_to_field_call_result)))
        }
        SequenceType::ExactSizeDoubleEndedIterator => {
            SqBValueSequence::ExactSizeDoubleEndedIterator(Box::new(
                range.map(i32_to_field_call_result),
            ))
        }
    }
}

pub fn fake_int_literal(value: i64) -> ast::IntLiteral {
    ast::IntLiteral {
        span: (0, 0).into(),
        value,
    }
}

pub fn fake_field_call_ast() -> ast::FieldCall {
    ast::FieldCall {
        span: (0, 0).into(),
        ident: ast::Ident {
            span: (0, 0).into(),
            name: "x".to_owned(),
        },
        opt_field_access: None,
        opt_arg_pack: None,
        opt_filter_pack: None,
    }
}
