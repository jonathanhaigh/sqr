// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

//! Definitions relating SQ primitive types.

use std::cmp;
use std::fmt;
use std::result::Result as StdResult;

use serde::Serialize;
use thiserror::Error as ThisError;

use crate::util::{TryAs, TryAsRef};

/// The kind of a primitive value.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PrimitiveKind {
    I128,
    I64,
    I32,
    U64,
    U32,
    F64,
    Str,
    Bool,
}

impl PrimitiveKind {
    /// Get the name of the kind of primitive value.
    pub fn name(&self) -> &'static str {
        match self {
            Self::I128 => "i128",
            Self::I64 => "i64",
            Self::I32 => "i32",
            Self::U64 => "u64",
            Self::U32 => "u32",
            Self::F64 => "f64",
            Self::Str => "str",
            Self::Bool => "bool",
        }
    }
}

impl fmt::Display for PrimitiveKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

/// An SQ primitive value.
///
/// This type is used for field call argument values and for primitive representations of SQ type
/// values.
#[derive(Clone, Debug, PartialEq, Serialize)]
#[serde(untagged)]
pub enum Primitive {
    I128(i128),
    I64(i64),
    I32(i32),
    U64(u64),
    U32(u32),
    F64(f64),
    Str(String),
    Bool(bool),
}

/// Error returned when a Primitive::try_as_* method fails.
#[derive(Debug, ThisError)]
pub enum PrimitiveTryAsError {
    #[error("Cannot convert primitive of type {kind} to {target}")]
    TypeMismatch { kind: PrimitiveKind, target: String },

    #[error("Integer value {value} does not fit in type {target}")]
    IntConversion { value: i128, target: String },
}

impl PrimitiveTryAsError {
    pub fn type_mismatch(value: &Primitive, target: &str) -> Self {
        Self::TypeMismatch {
            kind: value.kind(),
            target: target.to_owned(),
        }
    }

    pub fn int_conversion<T>(value: T, target: &str) -> Self
    where
        i128: From<T>,
    {
        Self::IntConversion {
            value: i128::from(value),
            target: target.to_owned(),
        }
    }
}

impl Primitive {
    /// Get the name of the kind of the SQ primitive value.
    pub fn kind_name(&self) -> &'static str {
        self.kind().name()
    }

    /// Get the kind of the SQ primitive value.
    pub fn kind(&self) -> PrimitiveKind {
        match self {
            Self::I128(_) => PrimitiveKind::I128,
            Self::I64(_) => PrimitiveKind::I64,
            Self::I32(_) => PrimitiveKind::I32,
            Self::U64(_) => PrimitiveKind::U64,
            Self::U32(_) => PrimitiveKind::U32,
            Self::F64(_) => PrimitiveKind::F64,
            Self::Str(_) => PrimitiveKind::Str,
            Self::Bool(_) => PrimitiveKind::Bool,
        }
    }
}

impl TryAs<i128> for Primitive {
    type Error = PrimitiveTryAsError;
    fn try_as(&self) -> StdResult<i128, Self::Error> {
        match self {
            Self::I128(v) => Ok(*v),
            Self::I64(v) => Ok(i128::from(*v)),
            Self::I32(v) => Ok(i128::from(*v)),
            Self::U64(v) => Ok(i128::from(*v)),
            Self::U32(v) => Ok(i128::from(*v)),
            _ => Err(PrimitiveTryAsError::type_mismatch(self, "i128")),
        }
    }
}

impl TryAs<i64> for Primitive {
    type Error = PrimitiveTryAsError;
    fn try_as(&self) -> StdResult<i64, Self::Error> {
        match self {
            Self::I128(v) => {
                i64::try_from(*v).map_err(|_| PrimitiveTryAsError::int_conversion(*v, "i64"))
            }
            Self::I64(v) => Ok(*v),
            Self::U64(v) => {
                i64::try_from(*v).map_err(|_| PrimitiveTryAsError::int_conversion(*v, "i64"))
            }
            Self::I32(v) => Ok(i64::from(*v)),
            Self::U32(v) => Ok(i64::from(*v)),
            _ => Err(PrimitiveTryAsError::type_mismatch(self, "i64")),
        }
    }
}

impl TryAs<i32> for Primitive {
    type Error = PrimitiveTryAsError;
    fn try_as(&self) -> StdResult<i32, Self::Error> {
        match self {
            Self::I128(v) => {
                i32::try_from(*v).map_err(|_| PrimitiveTryAsError::int_conversion(*v, "i32"))
            }
            Self::I64(v) => {
                i32::try_from(*v).map_err(|_| PrimitiveTryAsError::int_conversion(*v, "i32"))
            }
            Self::I32(v) => Ok(*v),
            Self::U64(v) => {
                i32::try_from(*v).map_err(|_| PrimitiveTryAsError::int_conversion(*v, "i32"))
            }
            Self::U32(v) => {
                i32::try_from(*v).map_err(|_| PrimitiveTryAsError::int_conversion(*v, "i32"))
            }
            _ => Err(PrimitiveTryAsError::type_mismatch(self, "i32")),
        }
    }
}

impl TryAs<u64> for Primitive {
    type Error = PrimitiveTryAsError;
    fn try_as(&self) -> StdResult<u64, Self::Error> {
        match self {
            Self::I128(v) => {
                u64::try_from(*v).map_err(|_| PrimitiveTryAsError::int_conversion(*v, "u64"))
            }
            Self::I64(v) => {
                u64::try_from(*v).map_err(|_| PrimitiveTryAsError::int_conversion(*v, "u64"))
            }
            Self::I32(v) => {
                u64::try_from(*v).map_err(|_| PrimitiveTryAsError::int_conversion(*v, "u64"))
            }
            Self::U64(v) => Ok(*v),
            Self::U32(v) => Ok(u64::from(*v)),
            _ => Err(PrimitiveTryAsError::type_mismatch(self, "u64")),
        }
    }
}

impl TryAs<u32> for Primitive {
    type Error = PrimitiveTryAsError;
    fn try_as(&self) -> StdResult<u32, Self::Error> {
        match self {
            Self::I128(v) => {
                u32::try_from(*v).map_err(|_| PrimitiveTryAsError::int_conversion(*v, "u32"))
            }
            Self::I64(v) => {
                u32::try_from(*v).map_err(|_| PrimitiveTryAsError::int_conversion(*v, "u32"))
            }
            Self::I32(v) => {
                u32::try_from(*v).map_err(|_| PrimitiveTryAsError::int_conversion(*v, "u32"))
            }
            Self::U64(v) => {
                u32::try_from(*v).map_err(|_| PrimitiveTryAsError::int_conversion(*v, "u32"))
            }
            Self::U32(v) => Ok(*v),
            _ => Err(PrimitiveTryAsError::type_mismatch(self, "u32")),
        }
    }
}

impl TryAs<f64> for Primitive {
    type Error = PrimitiveTryAsError;
    fn try_as(&self) -> StdResult<f64, Self::Error> {
        match self {
            Self::F64(v) => Ok(*v),
            _ => Err(PrimitiveTryAsError::type_mismatch(self, "f64")),
        }
    }
}

impl TryAs<bool> for Primitive {
    type Error = PrimitiveTryAsError;
    fn try_as(&self) -> StdResult<bool, Self::Error> {
        match self {
            Self::Bool(v) => Ok(*v),
            _ => Err(PrimitiveTryAsError::type_mismatch(self, "bool")),
        }
    }
}

impl TryAsRef<str> for Primitive {
    type Error = PrimitiveTryAsError;
    fn try_as_ref(&self) -> StdResult<&str, Self::Error> {
        match self {
            Self::Str(v) => Ok(v),
            _ => Err(PrimitiveTryAsError::type_mismatch(self, "str")),
        }
    }
}

impl cmp::PartialOrd<Primitive> for Primitive {
    /// Partially order SQ primitive values. In order for two values to be ordered (for this method
    /// to return `Some`), the values must have the same kind (be the same variant).
    ///
    /// Note that two `Primitive::Float`s will not have an order, even though they are the same
    /// kind, if one of them is `NaN`.
    fn partial_cmp(&self, other: &Primitive) -> Option<cmp::Ordering> {
        use Primitive::*;
        match (self, other) {
            (I128(lhs), I128(rhs)) => Some(lhs.cmp(rhs)),
            (I64(lhs), I64(rhs)) => Some(lhs.cmp(rhs)),
            (I32(lhs), I32(rhs)) => Some(lhs.cmp(rhs)),
            (U64(lhs), U64(rhs)) => Some(lhs.cmp(rhs)),
            (U32(lhs), U32(rhs)) => Some(lhs.cmp(rhs)),
            (F64(lhs), F64(rhs)) => lhs.partial_cmp(rhs),
            (Str(lhs), Str(rhs)) => Some(lhs.cmp(rhs)),
            (Bool(lhs), Bool(rhs)) => Some(lhs.cmp(rhs)),
            (_, _) => None,
        }
    }
}

impl From<i128> for Primitive {
    fn from(v: i128) -> Self {
        Self::I128(v)
    }
}

impl From<i64> for Primitive {
    fn from(v: i64) -> Self {
        Self::I64(v)
    }
}

impl From<i32> for Primitive {
    fn from(v: i32) -> Self {
        Self::I32(v)
    }
}

impl From<u64> for Primitive {
    fn from(v: u64) -> Self {
        Self::U64(v)
    }
}

impl From<u32> for Primitive {
    fn from(v: u32) -> Self {
        Self::U32(v)
    }
}

impl From<f64> for Primitive {
    fn from(v: f64) -> Self {
        Self::F64(v)
    }
}

impl From<f32> for Primitive {
    fn from(v: f32) -> Self {
        Self::F64(f64::from(v))
    }
}

impl From<String> for Primitive {
    fn from(v: String) -> Self {
        Self::Str(v)
    }
}

impl From<&str> for Primitive {
    fn from(v: &str) -> Self {
        Self::Str(v.to_owned())
    }
}

impl From<bool> for Primitive {
    fn from(v: bool) -> Self {
        Self::Bool(v)
    }
}

#[cfg(test)]
mod tests {

    use std::cmp::Ordering::{self, *};

    use fancy_regex::Regex;
    use pretty_assertions::assert_eq;
    use rstest::rstest;

    use super::*;
    use Primitive::*;

    #[rstest]
    #[case::i128[PrimitiveKind::I128, "(?i:i128)"]]
    #[case::i64[PrimitiveKind::I64, "(?i:i64)"]]
    #[case::i32[PrimitiveKind::I32, "(?i:i32)"]]
    #[case::u64[PrimitiveKind::U64, "(?i:u64)"]]
    #[case::u32[PrimitiveKind::U32, "(?i:u32)"]]
    #[case::f64[PrimitiveKind::F64, "(?i:f64)"]]
    #[case::str[PrimitiveKind::Str, "(?i:str)"]]
    #[case::bool[PrimitiveKind::Bool, "(?i:bool)"]]
    fn test_primitive_kind_name(#[case] kind: PrimitiveKind, #[case] regex_str: &'static str) {
        let re = Regex::new(regex_str).unwrap();
        assert!(re.is_match(kind.name()).unwrap());
    }

    #[rstest]
    #[case::i128[PrimitiveKind::I128, "(?i:i128)"]]
    #[case::i64[PrimitiveKind::I64, "(?i:i64)"]]
    #[case::i32[PrimitiveKind::I32, "(?i:i32)"]]
    #[case::u64[PrimitiveKind::U64, "(?i:u64)"]]
    #[case::u32[PrimitiveKind::U32, "(?i:u32)"]]
    #[case::f64[PrimitiveKind::F64, "(?i:f64)"]]
    #[case::str[PrimitiveKind::Str, "(?i:str)"]]
    #[case::bool[PrimitiveKind::Bool, "(?i:bool)"]]
    fn test_primitive_kind_display(#[case] kind: PrimitiveKind, #[case] regex_str: &'static str) {
        let re = Regex::new(regex_str).unwrap();
        assert!(re.is_match(&format!("{}", kind)).unwrap());
    }

    #[rstest]
    #[case::i128[PrimitiveKind::I128, "(?i:i128)"]]
    #[case::i64[PrimitiveKind::I64, "(?i:i64)"]]
    #[case::i32[PrimitiveKind::I32, "(?i:i32)"]]
    #[case::u64[PrimitiveKind::U64, "(?i:u64)"]]
    #[case::u32[PrimitiveKind::U32, "(?i:u32)"]]
    #[case::f64[PrimitiveKind::F64, "(?i:f64)"]]
    #[case::str[PrimitiveKind::Str, "(?i:str)"]]
    #[case::bool[PrimitiveKind::Bool, "(?i:bool)"]]
    fn test_primitive_kind_debug(#[case] kind: PrimitiveKind, #[case] regex_str: &'static str) {
        let re = Regex::new(regex_str).unwrap();
        assert!(re.is_match(&format!("{:?}", kind)).unwrap());
    }

    #[rstest]
    #[case::i128(I128(10), PrimitiveKind::I128)]
    #[case::i64(I64(10), PrimitiveKind::I64)]
    #[case::i32(I32(10), PrimitiveKind::I32)]
    #[case::u64(U64(10), PrimitiveKind::U64)]
    #[case::u32(U32(10), PrimitiveKind::U32)]
    #[case::f64(F64(100.0), PrimitiveKind::F64)]
    #[case::str(Str("abc".to_owned()), PrimitiveKind::Str)]
    #[case::bool(Bool(true), PrimitiveKind::Bool)]
    fn test_primitive_dot_kind(#[case] prim: Primitive, #[case] kind: PrimitiveKind) {
        assert_eq!(prim.kind(), kind);
    }

    #[rstest]
    #[case::i128(I128(10), PrimitiveKind::I128)]
    #[case::i64(I64(10), PrimitiveKind::I64)]
    #[case::i32(I32(10), PrimitiveKind::I32)]
    #[case::u64(U64(10), PrimitiveKind::U64)]
    #[case::u32(U32(10), PrimitiveKind::U32)]
    #[case::f64(F64(100.0), PrimitiveKind::F64)]
    #[case::str(Str("abc".to_owned()), PrimitiveKind::Str)]
    #[case::bool(Bool(true), PrimitiveKind::Bool)]
    fn test_primitive_dot_kind_name(#[case] prim: Primitive, #[case] kind: PrimitiveKind) {
        assert_eq!(prim.kind_name(), kind.name());
    }

    #[rstest]
    #[case::i128_10_10(I128(10), I128(10), true)]
    #[case::i128_10_9(I128(10), I128(9), false)]
    #[case::i128_10_i64_10(I128(10), I64(10), false)]
    #[case::i128_10_i32_10(I128(10), I32(10), false)]
    #[case::i128_10_u64_10(I128(10), U64(10), false)]
    #[case::i128_10_u32_10(I128(10), U32(10), false)]
    #[case::i128_10_str_x(I128(10), Str("x".to_owned()), false)]
    #[case::i128_10_f64_9p1(I128(10), F64(9.1), false)]
    #[case::i128_1_bool_true(I128(1), Bool(true), false)]
    #[case::str_x_x(Str("x".to_owned()), Str("x".to_owned()), true)]
    #[case::str_x_y(Str("x".to_owned()), Str("y".to_owned()), false)]
    #[case::str_x_f64_9p1(Str("x".to_owned()), F64(9.1), false)]
    #[case::str_x_bool_true(Str("x".to_owned()), Bool(true), false)]
    #[case::f64_9p1_9p1(F64(9.1), F64(9.1), true)]
    #[case::f64_9p1_8p2(F64(9.1), F64(8.2), false)]
    #[case::f64_nan_9p1(F64(f64::NAN), F64(9.1), false)]
    #[case::f64_nan_nan(F64(f64::NAN), F64(f64::NAN), false)]
    #[case::f64_bool_true(F64(9.1), Bool(true), false)]
    #[case::bool_true_true(Bool(true), Bool(true), true)]
    #[case::bool_false_false(Bool(false), Bool(false), true)]
    #[case::bool_true_false(Bool(true), Bool(false), false)]
    fn test_primitive_partial_eq(#[case] a: Primitive, #[case] b: Primitive, #[case] result: bool) {
        assert_eq!(a == b, result);
    }

    #[rstest]
    #[case::i128_10_10(I128(10), I128(10), Some(Equal))]
    #[case::i128_10_9(I128(10), I128(9), Some(Greater))]
    #[case::i128_10_20(I128(10), I128(20), Some(Less))]
    #[case::i128_10_i64_10(I128(10), I64(10), None)]
    #[case::i128_10_i32_10(I128(10), I32(10), None)]
    #[case::i128_10_u64_10(I128(10), U64(10), None)]
    #[case::i128_10_u32_10(I128(10), U32(10), None)]
    #[case::i128_10_str_x(I128(10), Str("x".to_owned()), None)]
    #[case::i128_10_f64_9p1(I128(10), F64(9.1), None)]
    #[case::i128_1_bool_true(I128(1), Bool(true), None)]
    #[case::str_x_x(Str("x".to_owned()), Str("x".to_owned()), Some(Equal))]
    #[case::str_x_y(Str("b".to_owned()), Str("a".to_owned()), Some(Greater))]
    #[case::str_x_y(Str("a".to_owned()), Str("b".to_owned()), Some(Less))]
    #[case::str_x_f64_9p1(Str("x".to_owned()), F64(9.1), None)]
    #[case::str_x_bool_true(Str("x".to_owned()), Bool(true), None)]
    #[case::f64_9p1_9p1(F64(9.1), F64(9.1), Some(Equal))]
    #[case::f64_9p1_8p2(F64(9.1), F64(8.2), Some(Greater))]
    #[case::f64_9p1_8p2(F64(9.1), F64(100.0), Some(Less))]
    #[case::f64_nan_9p1(F64(f64::NAN), F64(9.1), None)]
    #[case::f64_nan_nan(F64(f64::NAN), F64(f64::NAN), None)]
    #[case::f64_bool_true(F64(9.1), Bool(true), None)]
    #[case::bool_true_true(Bool(true), Bool(true), Some(Equal))]
    #[case::bool_false_false(Bool(false), Bool(false), Some(Equal))]
    #[case::bool_true_false(Bool(true), Bool(false), Some(Greater))]
    #[case::bool_true_false(Bool(false), Bool(true), Some(Less))]
    fn test_primitive_partial_cmp(
        #[case] a: Primitive,
        #[case] b: Primitive,
        #[case] result: Option<Ordering>,
    ) {
        assert_eq!(a.partial_cmp(&b), result);
    }

    #[rstest]
    #[case::i128_max(Primitive::from(i128::MAX), Some(i128::MAX))]
    #[case::i128_min(Primitive::from(i128::MIN), Some(i128::MIN))]
    #[case::i64_max(Primitive::from(i64::MAX), Some(i128::from(i64::MAX)))]
    #[case::i32_max(Primitive::from(i32::MAX), Some(i128::from(i32::MAX)))]
    #[case::u64_max(Primitive::from(u64::MAX), Some(i128::from(u64::MAX)))]
    #[case::u32_max(Primitive::from(u32::MAX), Some(i128::from(u32::MAX)))]
    #[case::f64(Primitive::from(99.0), None)]
    #[case::str(Primitive::from("abc"), None)]
    #[case::bool(Primitive::from(true), None)]
    fn test_primitive_try_as_i128(#[case] prim: Primitive, #[case] expected: Option<i128>) {
        let ret: StdResult<i128, _> = prim.try_as();
        match expected {
            Some(v) => assert_eq!(ret.unwrap(), v),
            None => assert!(ret.is_err()),
        }
    }

    #[rstest]
    #[case::i128_max(Primitive::from(i128::MAX), None)]
    #[case::i128_min(Primitive::from(i128::MIN), None)]
    #[case::i64_max(Primitive::from(i64::MAX), Some(i64::MAX))]
    #[case::i64_min(Primitive::from(i64::MIN), Some(i64::MIN))]
    #[case::i32_max(Primitive::from(i32::MAX), Some(i64::from(i32::MAX)))]
    #[case::u64_max(Primitive::from(u64::MAX), None)]
    #[case::u32_max(Primitive::from(u32::MAX), Some(i64::from(u32::MAX)))]
    #[case::f64(Primitive::from(99.0), None)]
    #[case::str(Primitive::from("abc"), None)]
    #[case::bool(Primitive::from(true), None)]
    fn test_primitive_try_as_i64(#[case] prim: Primitive, #[case] expected: Option<i64>) {
        let ret: StdResult<i64, _> = prim.try_as();
        match expected {
            Some(v) => assert_eq!(ret.unwrap(), v),
            None => assert!(ret.is_err()),
        }
    }

    #[rstest]
    #[case::i128_max(Primitive::from(i128::MAX), None)]
    #[case::i64_max(Primitive::from(i64::MAX), Some(9_223_372_036_854_775_807u64))]
    #[case::i64_min(Primitive::from(i64::MIN), None)]
    #[case::i32_max(Primitive::from(i32::MAX), Some(2_147_483_647u64))]
    #[case::u32_max(Primitive::from(u32::MAX), Some(u64::from(u32::MAX)))]
    #[case::u64_max(Primitive::from(u64::MAX), Some(u64::MAX))]
    #[case::u64_min(Primitive::from(u64::MIN), Some(u64::MIN))]
    #[case::f64(Primitive::from(99.0), None)]
    #[case::str(Primitive::from("abc"), None)]
    #[case::bool(Primitive::from(true), None)]
    fn test_primitive_try_as_u64(#[case] prim: Primitive, #[case] expected: Option<u64>) {
        let ret: StdResult<u64, _> = prim.try_as();
        match expected {
            Some(v) => assert_eq!(ret.unwrap(), v),
            None => assert!(ret.is_err()),
        }
    }

    #[rstest]
    #[case::i128_max(Primitive::from(i128::MAX), None)]
    #[case::u64_max(Primitive::from(u64::MAX), None)]
    #[case::i32_max(Primitive::from(i32::MAX), Some(2_147_483_647u32))]
    #[case::i32_min(Primitive::from(i32::MIN), None)]
    #[case::u32_max(Primitive::from(u32::MAX), Some(u32::MAX))]
    #[case::u32_min(Primitive::from(u32::MIN), Some(u32::MIN))]
    #[case::f64(Primitive::from(99.0), None)]
    #[case::str(Primitive::from("abc"), None)]
    #[case::bool(Primitive::from(true), None)]
    fn test_primitive_try_as_u32(#[case] prim: Primitive, #[case] expected: Option<u32>) {
        let ret: StdResult<u32, _> = prim.try_as();
        match expected {
            Some(v) => assert_eq!(ret.unwrap(), v),
            None => assert!(ret.is_err()),
        }
    }

    #[rstest]
    #[case::i128(Primitive::from(i128::MAX), None)]
    #[case::f64(Primitive::from(99.0f64), Some(99.0f64))]
    #[case::str(Primitive::from("abc"), None)]
    #[case::bool(Primitive::from(true), None)]
    fn test_primitive_try_as_f64(#[case] prim: Primitive, #[case] expected: Option<f64>) {
        let ret: StdResult<f64, _> = prim.try_as();
        match expected {
            Some(v) => assert_eq!(ret.unwrap(), v),
            None => assert!(ret.is_err()),
        }
    }

    #[rstest]
    #[case::i128(Primitive::from(i128::MAX), None)]
    #[case::f64(Primitive::from(99.0f64), None)]
    #[case::str(Primitive::from("abc"), None)]
    #[case::bool(Primitive::from(true), Some(true))]
    fn test_primitive_try_as_bool(#[case] prim: Primitive, #[case] expected: Option<bool>) {
        let ret: StdResult<bool, _> = prim.try_as();
        match expected {
            Some(v) => assert_eq!(ret.unwrap(), v),
            None => assert!(ret.is_err()),
        }
    }

    #[rstest]
    #[case::i128(Primitive::from(i128::MAX), None)]
    #[case::f64(Primitive::from(99.0f64), None)]
    #[case::str(Primitive::from("abc"), Some("abc"))]
    #[case::bool(Primitive::from(true), None)]
    fn test_primitive_try_as_ref_str(#[case] prim: Primitive, #[case] expected: Option<&str>) {
        let ret: StdResult<&str, _> = prim.try_as_ref();
        match expected {
            Some(v) => assert_eq!(ret.unwrap(), v),
            None => assert!(ret.is_err()),
        }
    }
}
