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

pub trait TryAs<T> {
    type Error;
    fn try_as(&self) -> StdResult<T, Self::Error>;
}

pub trait TryAsRef<T: ?Sized> {
    type Error;
    fn try_as_ref(&self) -> StdResult<&T, Self::Error>;
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

impl InfallibleAbs for i128 {
    type Output = u128;

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

impl TryFrom<i128> for CategorizedInt {
    type Error = Box<ConvertIntError>;

    fn try_from(operand: i128) -> StdResult<Self, Self::Error> {
        if operand.is_negative() {
            Ok(Self::Negative(abs_usize(operand)?))
        } else {
            Ok(Self::NonNegative(abs_usize(operand)?))
        }
    }
}

#[cfg(test)]
mod tests {

    use std::fmt;
    use std::result::Result as StdResult;

    use pretty_assertions::assert_eq;
    use rstest::rstest;

    use super::*;
    use crate::test_util::{none_on_32_bit_arch, none_on_64_bit_arch};
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

    #[rstest]
    #[case::ok_none(Ok(None), Ok(None))]
    #[case::err(Err(false), Err(false))]
    #[case::ok_some(Ok(Some(true)), Ok(Some(true)))]
    fn test_return_none_or_err(
        #[case] input: StdResult<Option<bool>, bool>,
        #[case] expected: StdResult<Option<bool>, bool>,
    ) {
        assert_eq!(return_none_or_err_test_helper(input), expected);
    }

    // --------------------------------------------------------------------------------------------
    // convert int tests
    // --------------------------------------------------------------------------------------------

    #[rstest]
    #[case::i64_0_to_usize(0i64, Some(0usize))]
    #[case::i64_max_to_usize(i64::MAX, none_on_32_bit_arch(9_223_372_036_854_775_807usize))]
    #[case::i64_min_to_usize(i64::MIN, Option::<usize>::None)]
    #[case::i64_minus1_to_usize(-1i64, Option::<usize>::None)]
    #[case::usize_0_to_i64(0usize, Some(0i64))]
    #[case::usize_max_to_i64(usize::MAX, none_on_64_bit_arch(4_294_967_295i64))]
    #[case::i128_0_to_usize(0i128, Some(0usize))]
    #[case::i128_max_to_usize(i128::MAX, Option::<usize>::None)]
    #[case::i128_min_to_usize(i128::MIN, Option::<usize>::None)]
    #[case::i128_minus1_to_usize(-1i128, Option::<usize>::None)]
    #[case::usize_0_to_i128(0usize, Some(0i128))]
    fn test_convert_int<To, Input>(#[case] input: Input, #[case] expected: Option<To>)
    where
        To: Copy + ToString + TryFrom<Input> + PartialEq + fmt::Debug,
        Input: Copy + ToString,
    {
        match expected {
            Some(output) => assert_eq!(convert_int::<To, Input>(input).unwrap(), output),
            None => assert!(convert_int::<To, Input>(input).is_err()),
        }
    }

    // --------------------------------------------------------------------------------------------
    // abs tests
    // --------------------------------------------------------------------------------------------

    #[rstest]
    #[case::zero(0i128, 0u128)]
    #[case::one(1i128, 1u128)]
    #[case::minus_one(-1i128, 1u128)]
    #[case::max(i128::MAX, 170_141_183_460_469_231_731_687_303_715_884_105_727u128)]
    #[case::max(i128::MIN, 170_141_183_460_469_231_731_687_303_715_884_105_728u128)]
    fn test_abs<To, Input>(#[case] input: Input, #[case] output: To)
    where
        To: fmt::Debug,
        Input: InfallibleAbs,
        <Input as InfallibleAbs>::Output: PartialEq<To> + fmt::Debug,
    {
        assert_eq!(abs(input), output);
    }

    // --------------------------------------------------------------------------------------------
    // abs_usize tests
    // --------------------------------------------------------------------------------------------

    #[rstest]
    #[case::i128_0(0, Some(0usize))]
    #[case::i128_1(1, Some(1usize))]
    #[case::i128_minus1(-1, Some(1usize))]
    #[case::i128_32_max(2_147_483_647, Some(2_147_483_647usize))]
    #[case::i128_32_min(-2_147_483_648, Some(2_147_483_648usize))]
    #[case::i128_64_max(9_223_372_036_854_775_807i128, Some(9_223_372_036_854_775_807usize))]
    #[case::i128_64_min(-9_223_372_036_854_775_808i128, Some(9_223_372_036_854_775_808usize))]
    #[case::i128_max(i128::MAX, None)]
    #[case::i128_min(i128::MIN, None)]
    fn test_abs_usize<Input>(#[case] input: Input, #[case] expected: Option<usize>)
    where
        Input: InfallibleAbs,
        usize: TryFrom<<Input as InfallibleAbs>::Output>,
        <Input as InfallibleAbs>::Output: ToString + Copy,
    {
        match expected {
            Some(output) => assert_eq!(abs_usize(input).unwrap(), output),
            None => assert!(abs_usize(input).is_err()),
        }
    }

    // --------------------------------------------------------------------------------------------
    // IterTools::nth_or_len tests
    // --------------------------------------------------------------------------------------------
    #[rstest]
    #[case::get_0th_of_10(10, 0, Ok(0))]
    #[case::get_1th_of_10(10, 1, Ok(1))]
    #[case::get_9th_of_10(10, 9, Ok(9))]
    #[case::get_10th_of_10(10, 10, Err(10))]
    #[case::get_100th_of_10(10, 100, Err(10))]
    #[case::get_0th_of_0(0, 0, Err(0))]
    #[case::get_1th_of_0(0, 1, Err(0))]
    fn test_nth_or_len(
        #[case] seq_len: usize,
        #[case] n: usize,
        #[case] expecting: StdResult<usize, usize>,
    ) {
        assert_eq!((0..seq_len).nth_or_len(n), expecting);
    }

    // --------------------------------------------------------------------------------------------
    // IterTools::nth_back_or_len tests
    // --------------------------------------------------------------------------------------------
    #[rstest]
    #[case::get_0th_of_10(10, 0, Ok(9))]
    #[case::get_1th_of_10(10, 1, Ok(8))]
    #[case::get_9th_of_10(10, 9, Ok(0))]
    #[case::get_10th_of_10(10, 10, Err(10))]
    #[case::get_100th_of_10(10, 100, Err(10))]
    #[case::get_0th_of_0(0, 0, Err(0))]
    #[case::get_1th_of_0(0, 1, Err(0))]
    fn test_nth_back_or_len(
        #[case] seq_len: usize,
        #[case] n: usize,
        #[case] expecting: StdResult<usize, usize>,
    ) {
        assert_eq!((0..seq_len).nth_back_or_len(n), expecting);
    }

    // --------------------------------------------------------------------------------------------
    // IterTools::advance tests
    // --------------------------------------------------------------------------------------------

    #[rstest]
    #[case::advance_0th_of_10(10, 0, Ok(Some(0)))]
    #[case::advance_1th_of_10(10, 1, Ok(Some(1)))]
    #[case::advance_9th_of_10(10, 9, Ok(Some(9)))]
    #[case::advance_10th_of_10(10, 10, Ok(None))]
    #[case::advance_11th_of_10(10, 11, Err(10))]
    #[case::advance_100th_of_10(10, 100, Err(10))]
    fn test_advance(
        #[case] seq_len: usize,
        #[case] n: usize,
        #[case] next: StdResult<Option<usize>, usize>,
    ) {
        let mut it = 0..seq_len;
        let advance_res = it.advance(n);
        assert_eq!(advance_res.map(|_| it.next()), next);
    }

    // --------------------------------------------------------------------------------------------
    // IterTools::advance_back tests
    // --------------------------------------------------------------------------------------------
    #[rstest]
    #[case::advance_back_0th_of_10(10, 0, Ok(Some(9)))]
    #[case::advance_back_1th_of_10(10, 1, Ok(Some(8)))]
    #[case::advance_back_9th_of_10(10, 9, Ok(Some(0)))]
    #[case::advance_back_10th_of_10(10, 10, Ok(None))]
    #[case::advance_back_11th_of_10(10, 11, Err(10))]
    #[case::advance_back_100th_of_10(10, 100, Err(10))]
    fn test_advance_back(
        #[case] seq_len: usize,
        #[case] n: usize,
        #[case] next_back: StdResult<Option<usize>, usize>,
    ) {
        let mut it = 0..seq_len;
        let advance_back_res = it.advance_back(n);
        assert_eq!(advance_back_res.map(|_| it.next_back()), next_back);
    }

    // --------------------------------------------------------------------------------------------
    // CategorizedInt::try_from tests
    // --------------------------------------------------------------------------------------------

    #[rstest]
    #[case::i128_0(0i128, Some(NonNegative(0)))]
    #[case::i128_1(1i128, Some(NonNegative(1)))]
    #[case::i128_minus1(-1i128, Some(Negative(1)))]
    #[case::i128_32_max(2_147_483_647i128, Some(NonNegative(2_147_483_647usize)))]
    #[case::i128_32_min(-2_147_483_648i128, Some(Negative(2_147_483_648usize)))]
    #[case::i128_64_max(
        9_223_372_036_854_775_807i128,
        none_on_32_bit_arch(NonNegative(9_223_372_036_854_775_807usize))
    )]
    #[case::i128_64_min(-9_223_372_036_854_775_808i128,
        none_on_32_bit_arch(Negative(9_223_372_036_854_775_808usize))
    )]
    #[case::i128_max(i128::MAX, None)]
    #[case::i128_min(i128::MIN, None)]
    fn test_categorized_int_try_from<T>(#[case] from: T, #[case] expected: Option<CategorizedInt>)
    where
        CategorizedInt: TryFrom<T>,
        <CategorizedInt as TryFrom<T>>::Error: fmt::Debug,
    {
        match expected {
            Some(cat_int) => assert_eq!(CategorizedInt::try_from(from).unwrap(), cat_int),
            None => assert!(CategorizedInt::try_from(from).is_err()),
        }
    }
}
