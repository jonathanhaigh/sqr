// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

//! Error types.
use miette::{Diagnostic, SourceSpan};
use thiserror::Error as ThisError;

use crate::fieldcall::{FieldCallInfo, FieldSequenceType};
use crate::lexer::TokenKind;
use crate::primitive::PrimitiveKind;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ErrorKind {
    Lex,
    UnexpectedToken,
    ParseValue,
    RepeatedNamedArg,
    ArgTypeMismatch,
    Serialize,
    ArgMissing,
    InvalidField,
    DuplicateField,
    PullupWithSiblings,
    System,
    ToPrimitive,
    FilterIndexOutOfBounds,
    SliceStepZero,
    ConvertInteger,
    ConvertIntegerArg,
    NonSingleFieldInComparison,
    ComparisonTypeMismatch,
}

/// Type of error used throughout SQ infrastructure code.
///
/// Errors have `span: SourceSpan` members where possible to point to the part of the input query
/// that is invalid or led to an error.
#[derive(Debug, Diagnostic, ThisError)]
#[must_use]
pub enum Error {
    /// A lexing error.
    #[error("unrecognized token")]
    Lex {
        #[label("unrecognized token")]
        span: SourceSpan,
    },

    /// A parsing error used when a token is not valid in its position in the query.
    #[error("unexpected {kind} token expecting {}", itertools::join(expecting.iter(), ", "))]
    #[diagnostic()]
    UnexpectedToken {
        #[label("unexpected token")]
        span: SourceSpan,

        kind: TokenKind,
        expecting: Vec<TokenKind>,
    },

    /// An error parsing a literal value in the query. This occurs when the token for a value is
    /// grammatically correct but its value doesn't fit into its destination type.
    #[error("{desc}")]
    #[diagnostic()]
    ParseValue {
        #[label("failed to parse this")]
        span: SourceSpan,

        desc: String,
    },

    /// An error when a named argument in a parameter pack is specified multiple times.
    ///
    /// E.g. `path.children(recurse=true, recurse=true)`.
    #[error("named arg {name} given more than once")]
    #[diagnostic()]
    RepeatedNamedArg {
        name: String,

        #[label("first given here")]
        span: SourceSpan,

        #[label("then here")]
        prev_span: SourceSpan,
    },

    /// An error when an argument's type is incorrect.
    ///
    /// E.g. `path.children(recurse="true")`.
    #[error("type mismatch for {type_name}::{field_name} param {param_name:?}: expecting {expecting} got {got}")]
    #[diagnostic()]
    ArgTypeMismatch {
        #[label("wrong type")]
        span: SourceSpan,

        type_name: String,
        field_name: String,
        param_name: String,
        expecting: String,
        got: String,
    },

    /// An error when the output serializer fails.
    #[error("internal error: failed to serialize results: {msg}")]
    #[diagnostic()]
    Serialize { msg: String },

    /// An error when no argument is specified for a mandatory parameter.
    #[error("missing parameter {param_name:?} for {type_name}::{field_name}")]
    #[diagnostic()]
    ArgMissing {
        #[label("missing parameter")]
        span: SourceSpan,

        type_name: String,
        field_name: String,
        param_name: String,
    },

    /// An error when a field name is invalid (for the type from which it is accessed).
    #[error("invalid field {type_name}::{field_name}")]
    #[diagnostic()]
    InvalidField {
        #[label("invalid field")]
        span: SourceSpan,

        type_name: String,
        field_name: String,
    },

    /// An error when the same field is specified multiple times in a brace expression.
    ///
    /// E.g. `path {children children}`.
    #[error("duplicate field {type_name}::{field_name}")]
    DuplicateField {
        #[label("duplicate field")]
        span: SourceSpan,

        #[label("previous mention")]
        prev_span: SourceSpan,

        type_name: String,
        field_name: String,
    },

    /// An error when pullup field access is used on a field with siblings.
    ///
    /// E.g. `path {<children exists}`
    #[error("cannot use pullup field access on a field with siblings")]
    #[diagnostic()]
    PullupWithSiblings {
        #[label("pullup with siblings")]
        span: SourceSpan,
    },

    /// An error when retrieving system data corresponding to a field access.
    #[error("system error when getting {type_name}::{field_name}: {source}")]
    #[diagnostic()]
    System {
        #[label("failed field call")]
        span: SourceSpan,

        type_name: String,
        field_name: String,

        #[source]
        source: anyhow::Error,
    },

    /// An error converting a type to a primitive value.
    #[error("error converting {type_name} to primitive value")]
    #[diagnostic()]
    ToPrimitive {
        #[label("failed conversion")]
        span: SourceSpan,

        type_name: String,

        #[source]
        source: anyhow::Error,
    },

    /// An error when a list index is out of bounds
    #[error("index ({index}) in filter for {type_name}::{field_name} out of bounds for sequence of length {len}")]
    FilterIndexOutOfBounds {
        #[label("out of bounds")]
        span: SourceSpan,

        type_name: String,
        field_name: String,
        index: i128,
        len: usize,
    },

    /// An error when the `step` in a slice filter is zero
    #[error("error in slice filter for {type_name}::{field_name}: a step of 0 is invalid")]
    SliceStepZero {
        #[label("zero is invalid")]
        span: SourceSpan,

        type_name: String,
        field_name: String,
    },

    /// An error converting between types of integer.
    #[error(
        "error converting {from} ({value}) to {to} while processing {type_name}::{field_name}"
    )]
    ConvertInteger {
        #[label("failed conversion")]
        span: SourceSpan,

        type_name: String,
        field_name: String,
        from: String,
        to: String,
        value: String,
    },

    /// An error converting to and argument's integer type.
    #[error(
        "error converting integer {value} to {type_name}::{field_name} {param_name} argument type {param_type}"
    )]
    ConvertIntegerArg {
        #[label("failed conversion")]
        span: SourceSpan,

        type_name: String,
        field_name: String,
        param_name: String,
        param_type: String,
        value: i128,
    },

    /// Field that doesn't return Single sequence type in a comparison filter.
    #[error(
        "non-Single return sequence type {sequence_type} for {type_name}::{field_name} cannot be used in comparison filters"
    )]
    NonSingleFieldInComparison {
        #[label("failed conversion")]
        span: SourceSpan,

        type_name: String,
        field_name: String,
        sequence_type: FieldSequenceType,
    },

    /// Invalid comparison filter operand types.
    #[error("Type mismatch in comparison filter for {type_name}::{field_name}: left operand has type {lhs} and right operand has type {rhs}")]
    ComparisonTypeMismatch {
        #[label("failed conversion")]
        span: SourceSpan,

        type_name: String,
        field_name: String,

        #[label("right operand")]
        lhs_span: SourceSpan,
        lhs: PrimitiveKind,

        #[label("left operand")]
        rhs_span: SourceSpan,
        rhs: PrimitiveKind,
    },
}

impl Error {
    pub fn kind(&self) -> ErrorKind {
        match self {
            Error::Lex { .. } => ErrorKind::Lex,
            Error::UnexpectedToken { .. } => ErrorKind::UnexpectedToken,
            Error::ParseValue { .. } => ErrorKind::ParseValue,
            Error::RepeatedNamedArg { .. } => ErrorKind::RepeatedNamedArg,
            Error::ArgTypeMismatch { .. } => ErrorKind::ArgTypeMismatch,
            Error::Serialize { .. } => ErrorKind::Serialize,
            Error::ArgMissing { .. } => ErrorKind::ArgMissing,
            Error::InvalidField { .. } => ErrorKind::InvalidField,
            Error::DuplicateField { .. } => ErrorKind::DuplicateField,
            Error::PullupWithSiblings { .. } => ErrorKind::PullupWithSiblings,
            Error::System { .. } => ErrorKind::System,
            Error::ToPrimitive { .. } => ErrorKind::ToPrimitive,
            Error::FilterIndexOutOfBounds { .. } => ErrorKind::FilterIndexOutOfBounds,
            Error::SliceStepZero { .. } => ErrorKind::SliceStepZero,
            Error::ConvertInteger { .. } => ErrorKind::ConvertInteger,
            Error::ConvertIntegerArg { .. } => ErrorKind::ConvertIntegerArg,
            Error::NonSingleFieldInComparison { .. } => ErrorKind::NonSingleFieldInComparison,
            Error::ComparisonTypeMismatch { .. } => ErrorKind::ComparisonTypeMismatch,
        }
    }
}

/// A value or an `Error`
pub type Result<T> = std::result::Result<T, Box<Error>>;

/// A value, `None` or an `Error`
pub type OptResult<T> = Result<Option<T>>;

/// Provide an `add_context` method to add context to an error from a `FieldCallInfo`.
pub trait AddFieldCallContext {
    /// Add context to an error from a `FieldCallInfo`, converting it into a `crate::core::Error`.
    fn add_context(self, call_info: &FieldCallInfo) -> Box<Error>;
}
