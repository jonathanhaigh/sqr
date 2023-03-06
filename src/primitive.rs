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
