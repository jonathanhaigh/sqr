// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

//! Definitions relating SQ primitive types.

use std::cmp;
use std::fmt;

use serde::Serialize;
use thiserror::Error as ThisError;

use crate::util::TryAsRef;

/// The kind of a primitive value.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PrimitiveKind {
    Int,
    Float,
    Str,
    Bool,
}

impl PrimitiveKind {
    /// Get the name of the kind of primitive value.
    pub fn name(&self) -> &'static str {
        match self {
            Self::Int => "PrimitiveInt",
            Self::Float => "PrimitiveFloat",
            Self::Str => "PrimitiveStr",
            Self::Bool => "PrimitiveBool",
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
    Int(i64),
    Float(f64),
    Str(String),
    Bool(bool),
}

impl Primitive {
    /// Get the name of the kind of the SQ primitive value.
    pub fn kind_name(&self) -> &'static str {
        self.kind().name()
    }

    /// Get the kind of the SQ primitive value.
    pub fn kind(&self) -> PrimitiveKind {
        match self {
            Self::Int(_) => PrimitiveKind::Int,
            Self::Float(_) => PrimitiveKind::Float,
            Self::Str(_) => PrimitiveKind::Str,
            Self::Bool(_) => PrimitiveKind::Bool,
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
            (Int(lhs), Int(rhs)) => Some(lhs.cmp(rhs)),
            (Float(lhs), Float(rhs)) => lhs.partial_cmp(rhs),
            (Str(lhs), Str(rhs)) => Some(lhs.cmp(rhs)),
            (Bool(lhs), Bool(rhs)) => Some(lhs.cmp(rhs)),
            (_, _) => None,
        }
    }
}

/// Error returned by `T::try_from(primitive)` when the primitive is the wrong kind to be converted
/// to `T`.
#[derive(Debug, ThisError)]
#[error("Failed to convert primitive to {to}")]
pub struct TryFromPrimitiveError {
    to: String,
}

/// Generate an implementation of `TryFrom<Primitive>` for a type.
///
/// # Parameters
/// - `$t`: the type to implement `TryFrom<Primitive>` for.
/// - `$variant`: the variant of `Primitive` that can be converted into `$t`.
macro_rules! generate_try_from_for_primitive {
    ($t:ty, $variant:ident) => {
        impl TryFrom<Primitive> for $t {
            type Error = TryFromPrimitiveError;
            fn try_from(value: Primitive) -> std::result::Result<Self, Self::Error> {
                if let Primitive::$variant(v) = value {
                    Ok(v)
                } else {
                    Err(TryFromPrimitiveError {
                        to: stringify!($t).to_owned(),
                    })
                }
            }
        }
    };
}

generate_try_from_for_primitive!(i64, Int);
generate_try_from_for_primitive!(f64, Float);
generate_try_from_for_primitive!(String, Str);
generate_try_from_for_primitive!(bool, Bool);

/// Generate an implementation of `util::TryAsRef` for a primitive type.
///
/// # Parameters
/// - `$t`: the underlying rust data type that `TryAsRef::try_as_ref` should return a reference to.
/// - `$variant`: the variant of `Primitive` from which a reference of type `$t` can be obtained.
macro_rules! generate_try_as_ref_for_primitive {
    ($t:ty, $variant:ident) => {
        impl TryAsRef<$t> for Primitive {
            #[doc = concat!(
                                "Try to get a `&",
                                stringify!($t),
                                "`  from a `Primitive::",
                                stringify!($variant),
                                "`.\n\nReturns `None` if the `Primitive` holds a different variant."
                            )]
            fn try_as_ref(&self) -> Option<&$t> {
                match self {
                    Self::$variant(v) => Some(v),
                    _ => None,
                }
            }
        }
    };
}

generate_try_as_ref_for_primitive!(i64, Int);
generate_try_as_ref_for_primitive!(f64, Float);
generate_try_as_ref_for_primitive!(str, Str);
generate_try_as_ref_for_primitive!(bool, Bool);

#[cfg(test)]
mod tests {

    use std::cmp::Ordering::{self, *};

    use fancy_regex::Regex;
    use pretty_assertions::assert_eq;
    use rstest::rstest;

    use super::*;
    use Primitive::*;

    #[rstest]
    #[case::int[PrimitiveKind::Int, "(?i:int)"]]
    #[case::float[PrimitiveKind::Float, "(?i:float)"]]
    #[case::str[PrimitiveKind::Str, "(?i:str)"]]
    #[case::bool[PrimitiveKind::Bool, "(?i:bool)"]]
    fn test_primitive_kind_name(#[case] kind: PrimitiveKind, #[case] regex_str: &'static str) {
        let re = Regex::new(regex_str).unwrap();
        assert!(re.is_match(kind.name()).unwrap());
    }

    #[rstest]
    #[case::int[PrimitiveKind::Int, "(?i:int)"]]
    #[case::float[PrimitiveKind::Float, "(?i:float)"]]
    #[case::str[PrimitiveKind::Str, "(?i:str)"]]
    #[case::bool[PrimitiveKind::Bool, "(?i:bool)"]]
    fn test_primitive_kind_display(#[case] kind: PrimitiveKind, #[case] regex_str: &'static str) {
        let re = Regex::new(regex_str).unwrap();
        assert!(re.is_match(&format!("{}", kind)).unwrap());
    }

    #[rstest]
    #[case::int[PrimitiveKind::Int, "(?i:int)"]]
    #[case::float[PrimitiveKind::Float, "(?i:float)"]]
    #[case::str[PrimitiveKind::Str, "(?i:str)"]]
    #[case::bool[PrimitiveKind::Bool, "(?i:bool)"]]
    fn test_primitive_kind_debug(#[case] kind: PrimitiveKind, #[case] regex_str: &'static str) {
        let re = Regex::new(regex_str).unwrap();
        assert!(re.is_match(&format!("{:?}", kind)).unwrap());
    }

    #[rstest]
    #[case::int(Int(10), PrimitiveKind::Int)]
    #[case::float(Float(100.0), PrimitiveKind::Float)]
    #[case::str(Str("abc".to_owned()), PrimitiveKind::Str)]
    #[case::bool(Bool(true), PrimitiveKind::Bool)]
    fn test_primitive_dot_kind(#[case] prim: Primitive, #[case] kind: PrimitiveKind) {
        assert_eq!(prim.kind(), kind);
    }

    #[rstest]
    #[case::int(Int(10), PrimitiveKind::Int)]
    #[case::float(Float(100.0), PrimitiveKind::Float)]
    #[case::str(Str("abc".to_owned()), PrimitiveKind::Str)]
    #[case::bool(Bool(true), PrimitiveKind::Bool)]
    fn test_primitive_dot_kind_name(#[case] prim: Primitive, #[case] kind: PrimitiveKind) {
        assert_eq!(prim.kind_name(), kind.name());
    }

    #[rstest]
    #[case::int_10_10(Int(10), Int(10), true)]
    #[case::int_10_9(Int(10), Int(9), false)]
    #[case::int_10_str_x(Int(10), Str("x".to_owned()), false)]
    #[case::int_10_float_9p1(Int(10), Float(9.1), false)]
    #[case::int_1_bool_true(Int(1), Bool(true), false)]
    #[case::str_x_x(Str("x".to_owned()), Str("x".to_owned()), true)]
    #[case::str_x_y(Str("x".to_owned()), Str("y".to_owned()), false)]
    #[case::str_x_float_9p1(Str("x".to_owned()), Float(9.1), false)]
    #[case::str_x_bool_true(Str("x".to_owned()), Bool(true), false)]
    #[case::float_9p1_9p1(Float(9.1), Float(9.1), true)]
    #[case::float_9p1_8p2(Float(9.1), Float(8.2), false)]
    #[case::float_nan_9p1(Float(f64::NAN), Float(9.1), false)]
    #[case::float_nan_nan(Float(f64::NAN), Float(f64::NAN), false)]
    #[case::float_bool_true(Float(9.1), Bool(true), false)]
    #[case::bool_true_true(Bool(true), Bool(true), true)]
    #[case::bool_false_false(Bool(false), Bool(false), true)]
    #[case::bool_true_false(Bool(true), Bool(false), false)]
    fn test_primitive_partial_eq(#[case] a: Primitive, #[case] b: Primitive, #[case] result: bool) {
        assert_eq!(a == b, result);
    }

    #[rstest]
    #[case::int_10_10(Int(10), Int(10), Some(Equal))]
    #[case::int_10_9(Int(10), Int(9), Some(Greater))]
    #[case::int_10_20(Int(10), Int(20), Some(Less))]
    #[case::int_10_str_x(Int(10), Str("x".to_owned()), None)]
    #[case::int_10_float_9p1(Int(10), Float(9.1), None)]
    #[case::int_1_bool_true(Int(1), Bool(true), None)]
    #[case::str_x_x(Str("x".to_owned()), Str("x".to_owned()), Some(Equal))]
    #[case::str_x_y(Str("b".to_owned()), Str("a".to_owned()), Some(Greater))]
    #[case::str_x_y(Str("a".to_owned()), Str("b".to_owned()), Some(Less))]
    #[case::str_x_float_9p1(Str("x".to_owned()), Float(9.1), None)]
    #[case::str_x_bool_true(Str("x".to_owned()), Bool(true), None)]
    #[case::float_9p1_9p1(Float(9.1), Float(9.1), Some(Equal))]
    #[case::float_9p1_8p2(Float(9.1), Float(8.2), Some(Greater))]
    #[case::float_9p1_8p2(Float(9.1), Float(100.0), Some(Less))]
    #[case::float_nan_9p1(Float(f64::NAN), Float(9.1), None)]
    #[case::float_nan_nan(Float(f64::NAN), Float(f64::NAN), None)]
    #[case::float_bool_true(Float(9.1), Bool(true), None)]
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
    #[case::int_int(Int(10), Some(10i64))]
    #[case::int_float(Int(10), Option::<f64>::None)]
    #[case::int_str(Int(10), Option::<String>::None)]
    #[case::int_bool(Int(10), Option::<bool>::None)]
    #[case::float_int(Float(10.1), Option::<i64>::None)]
    #[case::float_float(Float(10.1), Some(10.1f64))]
    #[case::float_str(Float(10.1), Option::<String>::None)]
    #[case::float_bool(Float(10.1), Option::<bool>::None)]
    #[case::str_int(Str("x".to_owned()), Option::<i64>::None)]
    #[case::str_float(Str("x".to_owned()), Option::<f64>::None)]
    #[case::str_str(Str("x".to_owned()), Some("x".to_owned()))]
    #[case::str_bool(Str("x".to_owned()), Option::<bool>::None)]
    #[case::bool_int(Bool(true), Option::<i64>::None)]
    #[case::bool_float(Bool(true), Option::<f64>::None)]
    #[case::bool_str(Bool(true), Option::<String>::None)]
    #[case::bool_true(Bool(true), Some(true))]
    #[case::bool_false(Bool(false), Some(false))]
    fn test_try_from_primitive<T>(#[case] prim: Primitive, #[case] expected: Option<T>)
    where
        T: std::convert::TryFrom<Primitive> + std::cmp::PartialEq + std::fmt::Debug,
        <T as TryFrom<Primitive>>::Error: std::fmt::Debug,
    {
        match expected {
            Some(v) => assert_eq!(T::try_from(prim).unwrap(), v),
            None => assert!(T::try_from(prim).is_err()),
        }
    }

    #[rstest]
    #[case::int_int(Int(10), Some(&10i64))]
    #[case::int_float(Int(10), Option::<&f64>::None)]
    #[case::int_str(Int(10), Option::<&str>::None)]
    #[case::int_bool(Int(10), Option::<&bool>::None)]
    #[case::float_int(Float(10.1), Option::<&i64>::None)]
    #[case::float_float(Float(10.1), Some(&10.1f64))]
    #[case::float_str(Float(10.1), Option::<&str>::None)]
    #[case::float_bool(Float(10.1), Option::<&bool>::None)]
    #[case::str_int(Str("x".to_owned()), Option::<&i64>::None)]
    #[case::str_float(Str("x".to_owned()), Option::<&f64>::None)]
    #[case::str_str(Str("x".to_owned()), Some("x"))]
    #[case::str_bool(Str("x".to_owned()), Option::<&bool>::None)]
    #[case::bool_int(Bool(true), Option::<&i64>::None)]
    #[case::bool_float(Bool(true), Option::<&f64>::None)]
    #[case::bool_str(Bool(true), Option::<&str>::None)]
    #[case::bool_true(Bool(true), Some(&true))]
    #[case::bool_false(Bool(false), Some(&false))]
    fn test_try_as_ref_from_primitive<T>(#[case] prim: Primitive, #[case] expected: Option<&T>)
    where
        Primitive: crate::util::TryAsRef<T>,
        T: ?Sized + std::cmp::PartialEq + std::fmt::Debug,
    {
        assert_eq!(prim.try_as_ref(), expected);
    }
}
