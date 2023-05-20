// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

//! Utilities for tests.

use pretty_assertions::assert_eq;
use strum_macros::{EnumIter, FromRepr};

use crate::ast;
use crate::error::Result;
use crate::fieldcall::FieldCallInfo;
use crate::primitive::Primitive;
use crate::schema;
use crate::sqvalue::{DynSequence, SqBValue, SqBValueSequence, SqValueSequence};
use crate::system::sqint::SqInt;

pub fn none_on_32_bit_arch<T>(v: T) -> Option<T> {
    if cfg!(target_pointer_width = "32") {
        None
    } else {
        Some(v)
    }
}

pub fn none_on_64_bit_arch<T>(v: T) -> Option<T> {
    if cfg!(target_pointer_width = "64") {
        None
    } else {
        Some(v)
    }
}

#[derive(Clone, Copy, Debug, EnumIter, Eq, FromRepr, PartialEq)]
pub enum SequenceType {
    Iterator,
    ExactSizeIterator,
    DoubleEndedIterator,
    ExactSizeDoubleEndedIterator,
}

pub fn gen_sqbvalue_seq(seq_type: SequenceType, len: usize) -> SqBValueSequence<'static> {
    // Use a range of i32 rather than i64 because Range<i64> doesn't implement
    // ExactSizeIterator (See https://github.com/rust-lang/rust/issues/22047) for why.
    let range = 0..i32::try_from(len).unwrap();
    let sqbvalue_range =
        Box::new(range.map(|num| -> Result<SqBValue> { Ok(Box::new(SqInt::new(num.into()))) }));

    use SequenceType::*;
    use SqBValueSequence as SBVS;

    match seq_type {
        Iterator => SBVS::Iterator(sqbvalue_range),
        ExactSizeIterator => SBVS::ExactSizeIterator(sqbvalue_range),
        DoubleEndedIterator => SBVS::DoubleEndedIterator(sqbvalue_range),
        ExactSizeDoubleEndedIterator => SBVS::ExactSizeDoubleEndedIterator(sqbvalue_range),
    }
}

pub fn gen_sqint_seq(seq_type: SequenceType, len: usize) -> SqValueSequence<'static, SqInt> {
    // Use a range of i32 rather than i64 because Range<i64> doesn't implement
    // ExactSizeIterator (See https://github.com/rust-lang/rust/issues/22047) for why.
    let range = 0..i32::try_from(len).unwrap();
    let sqint_range = Box::new(range.map(|num| Ok(SqInt::new(num.into()))));

    use SequenceType::*;
    use SqValueSequence as SVS;

    match seq_type {
        Iterator => SVS::Iterator(sqint_range),
        ExactSizeIterator => SVS::ExactSizeIterator(sqint_range),
        DoubleEndedIterator => SVS::DoubleEndedIterator(sqint_range),
        ExactSizeDoubleEndedIterator => SVS::ExactSizeDoubleEndedIterator(sqint_range),
    }
}

pub fn sqbvalue_to_primitive(sqbvalue: SqBValue) -> Primitive {
    let field_call_ast = fake_field_call_ast();
    let call_info = FieldCallInfo::new(&field_call_ast, schema::root_field());
    sqbvalue.get_primitive(&call_info).unwrap()
}

pub fn field_call_result_to_primitive(result: Result<SqBValue>) -> Primitive {
    sqbvalue_to_primitive(result.unwrap())
}

fn sqbvalue_seq_to_vec_of_opt_prims(seq: SqBValueSequence) -> Vec<Option<Primitive>> {
    let field_call_ast = fake_field_call_ast();
    let call_info = FieldCallInfo::new(&field_call_ast, schema::root_field());
    seq.map(|result| result.ok().map(|v| v.get_primitive(&call_info).unwrap()))
        .collect::<Vec<_>>()
}

pub fn assert_eq_sqbvalue_seq_elements(a: SqBValueSequence, b: SqBValueSequence) {
    assert_eq!(
        sqbvalue_seq_to_vec_of_opt_prims(a),
        sqbvalue_seq_to_vec_of_opt_prims(b)
    );
}

pub fn assert_eq_sqbvalue_seqs(a: SqBValueSequence, b: SqBValueSequence) {
    assert_eq!(std::mem::discriminant(&a), std::mem::discriminant(&b));
    assert_eq_sqbvalue_seq_elements(a, b);
}

pub fn get_seq_type<T>(seq: &DynSequence<T>) -> SequenceType {
    use DynSequence as DS;
    use SequenceType as ST;
    match seq {
        DS::Iterator(_) => ST::Iterator,
        DS::ExactSizeIterator(_) => ST::ExactSizeIterator,
        DS::DoubleEndedIterator(_) => ST::DoubleEndedIterator,
        DS::ExactSizeDoubleEndedIterator(_) => ST::ExactSizeDoubleEndedIterator,
    }
}

pub fn fake_int_literal(value: i128) -> ast::IntLiteral {
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
