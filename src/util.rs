// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

//! General utilities

use std::any;
use std::result::Result as StdResult;

use thiserror::Error as ThisError;

use crate::error::{AddFieldCallContext, Error};
use crate::fieldcall::FieldCallInfo;

/// Return early if an `OptResult<T>` expression is `Ok(None)` or `Err(_)`.
///
/// # Parameters
/// - `$x`: an `OptResult<T>` expression.
///
/// # Behaviour
/// - if `$x` is `Ok(Some(value))`, evaluates to the unwrapped `value`.
/// - if `$x` is `Ok(None)`, causes an early return of `Ok(None)`.
/// - if `$x` is `Err(e)`, causes an early return of `Err(e)`.
macro_rules! return_none_or_err {
    ($x:expr) => {
        match $x {
            Ok(Some(v)) => v,
            Ok(None) => return Ok(None),
            Err(e) => return Err(e),
        }
    };
}
pub(crate) use return_none_or_err;

/// A trait to be implemented by types for which a `&T` can sometimes be obtained.
///
/// Unlike `std::convert::AsRef::as_ref()`, `TryAsRef::try_as_ref()` can fail.
pub trait TryAsRef<T: ?Sized> {
    /// Try to borrow self as a`&T`.
    ///
    /// If the conversion fails, `None` is returned.
    fn try_as_ref(&self) -> Option<&T>;
}

#[derive(Clone, Debug, Eq, PartialEq, ThisError)]
#[error("Failed converting integer {value} from type {from} to type {to}")]
pub struct ConvertIntError {
    from: String,
    to: String,
    value: String,
}

impl AddFieldCallContext for Box<ConvertIntError> {
    fn add_context(self, call_info: &FieldCallInfo) -> Box<Error> {
        Box::new(Error::ConvertInteger {
            span: call_info.ast().ident.span,
            type_name: call_info.type_name().to_owned(),
            field_name: call_info.field_name().to_owned(),
            from: self.from,
            to: self.to,
            value: self.value,
        })
    }
}

/// Convert between integer types.
///
/// The difference between `To::try_from(value)` and `convert_int<To, _>(value)` is that
/// `convert_int` returns an error type (`ConvertIntegerError`) from which we can get details of
/// the failed conversion.
pub fn convert_int<To, From>(value: From) -> StdResult<To, Box<ConvertIntError>>
where
    To: ToString + TryFrom<From>,
    From: Copy + ToString,
{
    To::try_from(value).map_err(|_| {
        Box::new(ConvertIntError {
            from: any::type_name::<From>().to_owned(),
            to: any::type_name::<To>().to_owned(),
            value: value.to_string(),
        })
    })
}

/// Trait for types that provide an infallible method to get an absolute value.
///
/// Note that `abs()` methods on built-in signed integer types are not infallible because they
/// return Self, which can't represent the absolute value of Self::MIN.
pub trait InfallibleAbs {
    type Output;

    /// Get the absolute value of `self`.
    fn infallible_abs(&self) -> Self::Output;
}

impl InfallibleAbs for i64 {
    type Output = u64;

    fn infallible_abs(&self) -> Self::Output {
        self.abs_diff(0)
    }
}

/// Get the absolute value of a number.
///
/// Unlike e.g. `T::abs`, which returns a `T`, `abs::<T>` is infallible because it returns
/// an unsigned type big enough to hold the result even when the input value is `T::MIN`.
pub fn abs<T>(num: T) -> <T as InfallibleAbs>::Output
where
    T: InfallibleAbs,
{
    num.infallible_abs()
}

/// Get the absolute value of a number as a `usize`.
///
/// This function can fail e.g. when converting the absolute value of an i64 into a usize on a
/// 32-bit platform.
pub fn abs_usize<T>(value: T) -> StdResult<usize, Box<ConvertIntError>>
where
    T: InfallibleAbs,
    usize: TryFrom<<T as InfallibleAbs>::Output>,
    <T as InfallibleAbs>::Output: ToString + Copy,
{
    convert_int::<usize, _>(abs(value))
}

/// Trait for creating trait objects that are both `ExactSizeIterator` and `DoubleEndedIterator`.
pub trait ExactSizeDoubleEndedIterator: ExactSizeIterator + DoubleEndedIterator {}

impl<I, T> ExactSizeDoubleEndedIterator for T where
    T: ExactSizeIterator<Item = I> + DoubleEndedIterator<Item = I>
{
}

/// Trait for providing iterator utilities.
pub trait IterUtils: Iterator {
    /// Return the `n`th element or the length of the sequence if less than `n`.
    ///
    /// Consumes elements of the iterator up to and including the nth.
    ///
    /// If `n` is out of bounds then return the length of the sequence in the `Err` variant.
    fn nth_or_len(&mut self, n: usize) -> StdResult<<Self as Iterator>::Item, usize> {
        self.advance(n)?;
        self.next().ok_or(n)
    }

    fn nth_back_or_len(&mut self, n: usize) -> StdResult<<Self as Iterator>::Item, usize>
    where
        Self: DoubleEndedIterator,
    {
        self.advance_back(n)?;
        self.next_back().ok_or(n)
    }

    /// Advance the iterator `n` times.
    ///
    /// If `n` is out of bounds then return the length of the sequence in the `Err` variant.
    ///
    /// This has the same behaviour as the (as of writing) unstable
    /// `std::iter::Iterator::advance_by(n)`
    ///
    /// The default implementation advances the iterator one element at a time.
    fn advance(&mut self, n: usize) -> StdResult<(), usize> {
        for i in 0..n {
            _ = self.next().ok_or(i)?
        }
        Ok(())
    }

    /// Advance the iterator `n` times from the back.
    ///
    /// If `n` is out of bounds then return the length of the sequence in the `Err` variant.
    ///
    /// This has the same behaviour as the (as of writing) unstable
    /// `std::iter::DoubleEndedIterator::advance_back_by(n)`
    ///
    /// The default implementation advances the iterator one element at a time.
    fn advance_back(&mut self, n: usize) -> StdResult<(), usize>
    where
        Self: DoubleEndedIterator,
    {
        for i in 0..n {
            _ = self.next_back().ok_or(i)?
        }
        Ok(())
    }
}

impl<T> IterUtils for T where T: Iterator {}

/// Helper enum for categorizing integers as either negative or nonnegative.
///
/// This is useful for algorithms that operate on list indeces and whose behaviour depends on
/// whether those indeces are negative or not.
#[must_use]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CategorizedInt {
    NonNegative(usize),
    Negative(usize),
}

impl TryFrom<i64> for CategorizedInt {
    type Error = Box<ConvertIntError>;

    fn try_from(operand: i64) -> StdResult<Self, Self::Error> {
        if operand.is_negative() {
            Ok(Self::Negative(abs_usize(operand)?))
        } else {
            Ok(Self::NonNegative(abs_usize(operand)?))
        }
    }
}
