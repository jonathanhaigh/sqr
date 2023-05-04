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

#[cfg(test)]
mod tests {

    use std::result::Result as StdResult;

    use pretty_assertions::assert_eq;

    use super::*;
    use CategorizedInt::*;

    // --------------------------------------------------------------------------------------------
    // return_none_or_err tests
    // --------------------------------------------------------------------------------------------

    fn return_none_or_err_test_helper(
        v: Result<Option<bool>, bool>,
    ) -> StdResult<Option<bool>, bool> {
        let res = return_none_or_err!(v);
        Ok(Some(res))
    }

    #[test]
    fn return_none_or_err_none() {
        assert_eq!(return_none_or_err_test_helper(Ok(None)), Ok(None));
    }

    #[test]
    fn return_none_or_err_err() {
        assert_eq!(return_none_or_err_test_helper(Err(false)), Err(false));
    }

    #[test]
    fn return_none_or_err_some() {
        assert_eq!(
            return_none_or_err_test_helper(Ok(Some(true))),
            Ok(Some(true))
        );
    }

    // --------------------------------------------------------------------------------------------
    // convert int tests
    // --------------------------------------------------------------------------------------------

    macro_rules! convert_int_test {
        ($name:ident, $to:ty, $from:ty, $num:expr, $expected:expr) => {
            #[test]
            fn $name() {
                assert_eq!(convert_int::<$to, $from>($num).unwrap(), $expected);
            }
        };
    }

    macro_rules! convert_int_test_err {
        ($name:ident, $to:ty, $from:ty, $num:expr) => {
            #[test]
            fn $name() {
                assert!(convert_int::<$to, $from>($num).is_err());
            }
        };
    }

    // i64 to usize
    //
    convert_int_test!(convert_i64_0_to_usize, usize, i64, 0i64, 0usize);

    #[cfg(target_pointer_width = "64")]
    convert_int_test!(
        convert_i64_max_to_usize,
        usize,
        i64,
        i64::MAX,
        9_223_372_036_854_775_807usize
    );

    #[cfg(target_pointer_width = "32")]
    convert_int_test_err!(convert_i64_max_to_usize, usize, i64, i64::MAX);

    convert_int_test_err!(convert_i64_min_to_usize, usize, i64, i64::MIN);
    convert_int_test_err!(convert_i64_minus1_to_usize, usize, i64, -1i64);

    // usize to i64
    //
    convert_int_test!(convert_usize_0_to_i64, i64, usize, 0, 0i64);

    #[cfg(target_pointer_width = "32")]
    convert_int_test!(
        convert_usize_max_to_i64,
        i64,
        usize,
        usize::MAX,
        4_294_967_295i64
    );

    #[cfg(target_pointer_width = "64")]
    convert_int_test_err!(convert_usize_max_to_i64, i64, usize, usize::MAX);

    // --------------------------------------------------------------------------------------------
    // abs tests
    // --------------------------------------------------------------------------------------------

    macro_rules! abs_test {
        ($name:ident, $value:expr, $abs:expr) => {
            #[test]
            fn $name() {
                assert_eq!(abs($value), $abs);
            }
        };
    }

    abs_test!(abs_0i64, 0i64, 0u64);
    abs_test!(abs_1i64, 1i64, 1u64);
    abs_test!(abs_minus1i64, -1i64, 1u64);
    abs_test!(abs_i64_max, i64::MAX, 9_223_372_036_854_775_807u64);
    abs_test!(abs_i64_min, i64::MIN, 9_223_372_036_854_775_808u64);

    // --------------------------------------------------------------------------------------------
    // abs_usize tests
    // --------------------------------------------------------------------------------------------
    macro_rules! abs_usize_test {
        ($name:ident, $value:expr, $abs:expr) => {
            #[test]
            fn $name() {
                assert_eq!(abs_usize($value).unwrap(), $abs);
            }
        };
    }

    macro_rules! abs_usize_test_err {
        ($name:ident, $value:expr, $abs:expr) => {
            #[test]
            fn $name() {
                assert!(abs_usize($value).is_err());
            }
        };
    }

    abs_usize_test!(abs_usize_0i64, 0i64, 0usize);
    abs_usize_test!(abs_usize_1i64, 1i64, 1usize);
    abs_usize_test!(abs_usize_minus1i64, -1i64, 1usize);
    abs_usize_test!(abs_usize_i64_32max, 2_147_483_647i64, 2_147_483_647usize);
    abs_usize_test!(abs_usize_i64_32min, -2_147_483_648i64, 2_147_483_648usize);

    #[cfg(target_pointer_width = "64")]
    abs_usize_test!(abs_usize_i64_max, i64::MAX, 9_223_372_036_854_775_807usize);

    #[cfg(target_pointer_width = "32")]
    abs_usize_test_err!(abs_usize_i64_max, i64::MAX);

    #[cfg(target_pointer_width = "64")]
    abs_usize_test!(abs_usize_i64_min, i64::MIN, 9_223_372_036_854_775_808usize);

    #[cfg(target_pointer_width = "32")]
    abs_usize_test_err!(abs_usize_i64_min, i64::MIN);

    // --------------------------------------------------------------------------------------------
    // IterTools::nth_or_len tests
    // --------------------------------------------------------------------------------------------
    macro_rules! nth_or_len_test {
        ($name:ident, $seq_len:expr, $n: expr, $nth:expr) => {
            #[test]
            fn $name() {
                assert_eq!((0..$seq_len).nth_or_len($n).unwrap(), $nth);
            }
        };
    }

    macro_rules! nth_or_len_test_err {
        ($name:ident, $seq_len:expr, $n: expr) => {
            #[test]
            fn $name() {
                assert_eq!((0..$seq_len).nth_or_len($n).unwrap_err(), $seq_len);
            }
        };
    }
    nth_or_len_test!(nth_or_len_0th_of_10, 10, 0, 0);
    nth_or_len_test!(nth_or_len_1th_of_10, 10, 1, 1);
    nth_or_len_test!(nth_or_len_9th_of_10, 10, 9, 9);
    nth_or_len_test_err!(nth_or_len_10th_of_10, 10, 10);
    nth_or_len_test_err!(nth_or_len_100th_of_10, 10, 100);

    // --------------------------------------------------------------------------------------------
    // IterTools::nth_back_or_len tests
    // --------------------------------------------------------------------------------------------
    macro_rules! nth_back_or_len_test {
        ($name:ident, $seq_len:expr, $n: expr, $nth:expr) => {
            #[test]
            fn $name() {
                assert_eq!((0..$seq_len).nth_back_or_len($n).unwrap(), $nth);
            }
        };
    }

    macro_rules! nth_back_or_len_test_err {
        ($name:ident, $seq_len:expr, $n: expr) => {
            #[test]
            fn $name() {
                assert_eq!((0..$seq_len).nth_back_or_len($n).unwrap_err(), $seq_len);
            }
        };
    }
    nth_back_or_len_test!(nth_back_or_len_0th_of_10, 10, 0, 9);
    nth_back_or_len_test!(nth_back_or_len_1th_of_10, 10, 1, 8);
    nth_back_or_len_test!(nth_back_or_len_9th_of_10, 10, 9, 0);
    nth_back_or_len_test_err!(nth_back_or_len_10th_of_10, 10, 10);
    nth_back_or_len_test_err!(nth_back_or_len_100th_of_10, 10, 100);

    // --------------------------------------------------------------------------------------------
    // IterTools::advance tests
    // --------------------------------------------------------------------------------------------
    macro_rules! advance_test {
        ($name:ident, $seq_len:expr, $n:expr, $next:expr) => {
            #[test]
            fn $name() {
                let mut it = 0..$seq_len;
                assert!(it.advance($n).is_ok());
                assert_eq!(it.next(), $next);
            }
        };
    }

    macro_rules! advance_test_err {
        ($name:ident, $seq_len:expr, $n:expr) => {
            #[test]
            fn $name() {
                let mut it = 0..$seq_len;
                assert_eq!(it.advance($n).unwrap_err(), $seq_len);
                assert_eq!(it.next(), None);
            }
        };
    }

    advance_test!(advance_0th_of_10, 10, 0, Some(0));
    advance_test!(advance_1th_of_10, 10, 1, Some(1));
    advance_test!(advance_9th_of_10, 10, 9, Some(9));
    advance_test!(advance_10th_of_10, 10, 10, None);
    advance_test_err!(advance_11th_of_10, 10, 11);
    advance_test_err!(advance_100th_of_10, 10, 100);

    // --------------------------------------------------------------------------------------------
    // IterTools::advance_back tests
    // --------------------------------------------------------------------------------------------
    macro_rules! advance_back_test {
        ($name:ident, $seq_len:expr, $n:expr, $next:expr) => {
            #[test]
            fn $name() {
                let mut it = 0..$seq_len;
                assert!(it.advance_back($n).is_ok());
                assert_eq!(it.next_back(), $next);
            }
        };
    }

    macro_rules! advance_back_test_err {
        ($name:ident, $seq_len:expr, $n:expr) => {
            #[test]
            fn $name() {
                let mut it = 0..$seq_len;
                assert_eq!(it.advance_back($n).unwrap_err(), $seq_len);
                assert_eq!(it.next_back(), None);
            }
        };
    }

    advance_back_test!(advance_back_0th_of_10, 10, 0, Some(9));
    advance_back_test!(advance_back_1th_of_10, 10, 1, Some(8));
    advance_back_test!(advance_back_9th_of_10, 10, 9, Some(0));
    advance_back_test!(advance_back_10th_of_10, 10, 10, None);
    advance_back_test_err!(advance_back_11th_of_10, 10, 11);
    advance_back_test_err!(advance_back_100th_of_10, 10, 100);

    // --------------------------------------------------------------------------------------------
    // CategorizedInt::try_from tests
    // --------------------------------------------------------------------------------------------

    macro_rules! categorized_int_try_from_test {
        ($name:ident, $value:expr, $expected:expr) => {
            #[test]
            fn $name() {
                assert_eq!(CategorizedInt::try_from($value).unwrap(), $expected);
            }
        };
    }

    macro_rules! categorized_int_try_from_test_err {
        ($name:ident, $value:expr) => {
            #[test]
            fn $name() {
                assert!(CategorizedInt::try_from($value).is_err());
            }
        };
    }

    categorized_int_try_from_test!(categorized_int_try_from_0, 0, NonNegative(0));
    categorized_int_try_from_test!(categorized_int_try_from_1, 1, NonNegative(1));
    categorized_int_try_from_test!(
        categorized_int_try_from_i64_i32_max,
        2_147_483_647i64,
        NonNegative(2_147_483_647usize)
    );

    #[cfg(target_pointer_width = "64")]
    categorized_int_try_from_test!(
        categorized_int_try_from_i64_max,
        i64::MAX,
        NonNegative(9_223_372_036_854_775_807usize)
    );

    #[cfg(target_pointer_width = "32")]
    categorized_int_try_from_test_err!(categorized_int_try_from_i64_max, i64::MAX);

    categorized_int_try_from_test!(categorized_int_try_from_minus1, -1, Negative(1));
    categorized_int_try_from_test!(
        categorized_int_try_from_i64_i32_min,
        -2_147_483_648i64,
        Negative(2_147_483_648usize)
    );

    #[cfg(target_pointer_width = "64")]
    categorized_int_try_from_test!(
        categorized_int_try_from_i64_min,
        i64::MIN,
        Negative(9_223_372_036_854_775_808usize)
    );

    #[cfg(target_pointer_width = "32")]
    categorized_int_try_from_test_err!(categorized_int_try_from_i64_min, i64::MIN);
}
