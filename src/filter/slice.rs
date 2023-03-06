//! Items relating to slice filters in the query (e.g. `path.children[1:-1:2]`).

use std::cmp;
use std::collections::VecDeque;

use crate::ast;
use crate::error::{AddFieldCallContext, Error, Result};
use crate::fieldcall::FieldCallInfo;
use crate::filter::SequenceToSequenceFilter;
use crate::sqvalue::{
    SqBValue, SqBValueDoubleEndedIterator, SqBValueExactSizeDoubleEndedIterator,
    SqBValueExactSizeIterator, SqBValueIterator, SqBValueSequence,
};
use crate::util::{CategorizedInt, IterUtils};

/// Filter used for a slice filter in the query (e.g. `path.children[1:-1:2]`).
#[must_use]
#[derive(Debug)]
pub struct SliceFilter<'a> {
    call_info: &'a FieldCallInfo<'a>,
    slice: &'a ast::Slice,
}

impl<'a> SliceFilter<'a> {
    pub fn new(call_info: &'a FieldCallInfo<'a>, slice: &'a ast::Slice) -> Self {
        Self { call_info, slice }
    }

    fn start(&self) -> Result<CategorizedInt> {
        let raw_start = match &self.slice.opt_start {
            Some(int_lit) => int_lit.value,
            None => {
                if let CategorizedInt::Negative(_) = self.step()? {
                    -1
                } else {
                    0
                }
            }
        };

        CategorizedInt::try_from(raw_start).map_err(|e| e.add_context(self.call_info))
    }

    fn stop(&self) -> Result<Option<CategorizedInt>> {
        match &self.slice.opt_stop {
            Some(int_lit) => Ok(Some(
                CategorizedInt::try_from(int_lit.value)
                    .map_err(|e| e.add_context(self.call_info))?,
            )),
            None => Ok(None),
        }
    }

    fn step(&self) -> Result<CategorizedInt> {
        let raw_step = self
            .slice
            .opt_step
            .as_ref()
            .map(|int_lit| int_lit.value)
            .unwrap_or(1);

        let step = CategorizedInt::try_from(raw_step).map_err(|e| e.add_context(self.call_info))?;

        if let CategorizedInt::NonNegative(0) = step {
            return Err(Box::new(Error::SliceStepZero {
                span: self.slice.opt_step.as_ref().unwrap().span,
                type_name: self.call_info.type_name().to_owned(),
                field_name: self.call_info.field_name().to_owned(),
            }));
        }

        Ok(step)
    }
}

impl<'a> SequenceToSequenceFilter for SliceFilter<'a> {
    fn filter<'s>(&self, seq: SqBValueSequence<'s>) -> Result<SqBValueSequence<'s>> {
        match seq {
            SqBValueSequence::Iterator(it) => IteratorSliceFilter::new(self).filter(it),
            SqBValueSequence::ExactSizeIterator(it) => {
                ExactSizeIteratorSliceFilter::new(self).filter(it)
            }
            SqBValueSequence::DoubleEndedIterator(it) => {
                DoubleEndedIteratorSliceFilter::new(self).filter(it)
            }
            SqBValueSequence::ExactSizeDoubleEndedIterator(it) => {
                ExactSizeDoubleEndedIteratorSliceFilter::new(self).filter(it)
            }
        }
    }
}

#[must_use]
#[derive(Debug)]
struct IteratorSliceFilter<'a> {
    parent: &'a SliceFilter<'a>,
}

impl<'a> IteratorSliceFilter<'a> {
    fn new(parent: &'a SliceFilter<'a>) -> Self {
        Self { parent }
    }

    fn filter<'s>(&self, iter: SqBValueIterator<'s>) -> Result<SqBValueSequence<'s>> {
        use CategorizedInt::*;

        let start = self.parent.start()?;
        let stop = self.parent.stop()?;
        let step = self.parent.step()?;

        Ok(match (start, stop, step) {
            (NonNegative(ustart), Some(NonNegative(ustop)), NonNegative(ustep)) => {
                self.with_nonneg_start_nonneg_stop_nonneg_step(iter, ustart, ustop, ustep)
            }
            (NonNegative(ustart), Some(NonNegative(ustop)), Negative(uminus_step)) => {
                self.with_nonneg_start_nonneg_stop_neg_step(iter, ustart, ustop, uminus_step)
            }
            (NonNegative(ustart), None, Negative(uminus_step)) => {
                self.with_nonneg_start_no_stop_neg_step(iter, ustart, uminus_step)
            }
            (NonNegative(ustart), None, NonNegative(ustep)) => {
                self.with_nonneg_start_no_stop_nonneg_step(iter, ustart, ustep)
            }
            (NonNegative(ustart), Some(Negative(uminus_stop)), NonNegative(ustep)) => {
                self.with_nonneg_start_neg_stop_nonneg_step(iter, ustart, uminus_stop, ustep)
            }
            (NonNegative(ustart), Some(Negative(uminus_stop)), Negative(uminus_step)) => {
                self.with_nonneg_start_neg_stop_neg_step(iter, ustart, uminus_stop, uminus_step)
            }

            (Negative(uminus_start), Some(NonNegative(ustop)), NonNegative(ustep)) => {
                self.with_neg_start_nonneg_stop_nonneg_step(iter, uminus_start, ustop, ustep)
            }
            (Negative(uminus_start), Some(NonNegative(ustop)), Negative(uminus_step)) => {
                self.with_neg_start_nonneg_stop_neg_step(iter, uminus_start, ustop, uminus_step)
            }
            (Negative(uminus_start), None, Negative(uminus_step)) => {
                self.with_neg_start_no_stop_neg_step(iter, uminus_start, uminus_step)
            }
            (Negative(uminus_start), None, NonNegative(ustep)) => {
                self.with_neg_start_no_stop_nonneg_step(iter, uminus_start, ustep)
            }
            (Negative(uminus_start), Some(Negative(uminus_stop)), NonNegative(ustep)) => {
                self.with_neg_start_neg_stop_nonneg_step(iter, uminus_start, uminus_stop, ustep)
            }
            (Negative(uminus_start), Some(Negative(uminus_stop)), Negative(uminus_step)) => {
                self.with_neg_start_neg_stop_neg_step(iter, uminus_start, uminus_stop, uminus_step)
            }
        })
    }

    fn with_nonneg_start_nonneg_stop_nonneg_step<'s, I>(
        &self,
        iter: I,
        start: usize,
        stop: usize,
        step: usize,
    ) -> SqBValueSequence<'s>
    where
        I: Iterator<Item = Result<SqBValue>> + 's,
    {
        assert_ne!(step, 0);

        SqBValueSequence::Iterator(Box::new(
            iter.skip(start)
                .take(stop.saturating_sub(start))
                .step_by(step),
        ))
    }

    fn with_nonneg_start_nonneg_stop_neg_step<'s, I>(
        &self,
        iter: I,
        start: usize,
        stop: usize,
        minus_step: usize,
    ) -> SqBValueSequence<'s>
    where
        I: Iterator<Item = Result<SqBValue>> + 's,
    {
        assert_ne!(minus_step, 0);

        SqBValueSequence::ExactSizeDoubleEndedIterator(Box::new(
            iter.skip(stop + 1)
                .take(start.saturating_sub(stop))
                .collect::<Vec<_>>()
                .into_iter()
                .rev()
                .step_by(minus_step),
        ))
    }

    fn with_nonneg_start_no_stop_nonneg_step<'s, I>(
        &self,
        iter: I,
        start: usize,
        step: usize,
    ) -> SqBValueSequence<'s>
    where
        I: Iterator<Item = Result<SqBValue>> + 's,
    {
        assert_ne!(step, 0);

        SqBValueSequence::Iterator(Box::new(iter.skip(start).step_by(step)))
    }

    fn with_nonneg_start_no_stop_neg_step<'s, I>(
        &self,
        iter: I,
        start: usize,
        minus_step: usize,
    ) -> SqBValueSequence<'s>
    where
        I: Iterator<Item = Result<SqBValue>> + 's,
    {
        assert_ne!(minus_step, 0);

        SqBValueSequence::ExactSizeDoubleEndedIterator(Box::new(
            iter.take(start + 1)
                .collect::<Vec<_>>()
                .into_iter()
                .rev()
                .step_by(minus_step),
        ))
    }

    fn with_nonneg_start_neg_stop_nonneg_step<'s>(
        &self,
        iter: SqBValueIterator<'s>,
        start: usize,
        minus_stop: usize,
        step: usize,
    ) -> SqBValueSequence<'s> {
        assert_ne!(step, 0);
        assert_ne!(minus_stop, 0);

        let mut buff = Vec::new();
        let mut remainder = 0usize;
        for (index, elem) in iter.skip(start).enumerate() {
            remainder = index % step;
            if remainder == 0 {
                buff.push(elem);
            }
        }

        if minus_stop <= remainder {
            // TODO: return a Vec?
            return SqBValueSequence::ExactSizeDoubleEndedIterator(Box::new(buff.into_iter()));
        }

        let to_remove = (minus_stop - remainder + step - 1) / step;

        if to_remove >= buff.len() {
            return SqBValueSequence::empty();
        }

        let new_len = buff.len() - to_remove;

        buff.truncate(new_len);
        // TODO: return a Vec?
        SqBValueSequence::ExactSizeDoubleEndedIterator(Box::new(buff.into_iter()))
    }

    fn with_nonneg_start_neg_stop_neg_step<'s, I>(
        &self,
        iter: I,
        start: usize,
        minus_stop: usize,
        minus_step: usize,
    ) -> SqBValueSequence<'s>
    where
        I: Iterator<Item = Result<SqBValue>> + 's,
    {
        assert_ne!(minus_step, 0);
        assert_ne!(minus_stop, 0);

        let mut buff = VecDeque::new();

        let mut len = 0usize;
        for (index, elem) in iter.enumerate() {
            // Use push_front() and pop_back() to get the elements in reverse order.
            if index <= start {
                buff.push_front(elem);
            }
            len += 1;

            if minus_stop <= len {
                if buff.len() == 1 {
                    return SqBValueSequence::empty();
                }
                buff.pop_back();
            }
        }
        SqBValueSequence::ExactSizeDoubleEndedIterator(Box::new(
            buff.into_iter().step_by(minus_step),
        ))
    }

    fn with_neg_start_nonneg_stop_nonneg_step<'s, I>(
        &self,
        iter: I,
        minus_start: usize,
        stop: usize,
        step: usize,
    ) -> SqBValueSequence<'s>
    where
        I: Iterator<Item = Result<SqBValue>> + 's,
    {
        assert_ne!(step, 0);
        assert_ne!(minus_start, 0);

        let mut buff = VecDeque::new();
        let mut len = 0usize;

        for (index, elem) in iter.enumerate() {
            if index < stop {
                buff.push_back(elem);
            }
            len += 1;

            if len > minus_start {
                buff.pop_front();
            }
        }

        SqBValueSequence::ExactSizeDoubleEndedIterator(Box::new(buff.into_iter().step_by(step)))
    }

    fn with_neg_start_nonneg_stop_neg_step<'s>(
        &self,
        iter: SqBValueIterator<'s>,
        minus_start: usize,
        stop: usize,
        minus_step: usize,
    ) -> SqBValueSequence<'s> {
        assert_ne!(minus_step, 0);
        assert_ne!(minus_start, 0);

        SqBValueSequence::Iterator(Box::new(
            iter.skip(stop + 1)
                .collect::<Vec<_>>()
                .into_iter()
                .rev()
                .skip(minus_start - 1)
                .step_by(minus_step),
        ))
    }

    fn with_neg_start_no_stop_nonneg_step<'s>(
        &self,
        iter: SqBValueIterator<'s>,
        minus_start: usize,
        step: usize,
    ) -> SqBValueSequence<'s> {
        assert_ne!(step, 0);
        assert_ne!(minus_start, 0);

        let mut buff = VecDeque::new();
        for elem in iter {
            if buff.len() == minus_start {
                buff.pop_front();
            }
            buff.push_back(elem);
        }

        SqBValueSequence::ExactSizeDoubleEndedIterator(Box::new(buff.into_iter().step_by(step)))
    }

    fn with_neg_start_no_stop_neg_step<'s>(
        &self,
        iter: SqBValueIterator<'s>,
        minus_start: usize,
        minus_step: usize,
    ) -> SqBValueSequence<'s> {
        assert_ne!(minus_step, 0);
        assert_ne!(minus_start, 0);
        SqBValueSequence::ExactSizeDoubleEndedIterator(Box::new(
            iter.collect::<Vec<_>>()
                .into_iter()
                .rev()
                .skip(minus_start - 1)
                .step_by(minus_step),
        ))
    }

    fn with_neg_start_neg_stop_nonneg_step<'s>(
        &self,
        iter: SqBValueIterator<'s>,
        minus_start: usize,
        minus_stop: usize,
        step: usize,
    ) -> SqBValueSequence<'s> {
        assert_ne!(step, 0);
        assert_ne!(minus_start, 0);
        assert_ne!(minus_stop, 0);

        if minus_stop >= minus_start {
            return SqBValueSequence::empty();
        }

        let mut buff = VecDeque::new();

        for elem in iter {
            if buff.len() == minus_start {
                buff.pop_front();
            }
            buff.push_back(elem);
        }

        let buff_len = buff.len();
        if buff_len <= minus_stop {
            return SqBValueSequence::empty();
        }

        SqBValueSequence::ExactSizeDoubleEndedIterator(Box::new(
            buff.into_iter().take(buff_len - minus_stop).step_by(step),
        ))
    }

    fn with_neg_start_neg_stop_neg_step<'s>(
        &self,
        iter: SqBValueIterator<'s>,
        minus_start: usize,
        minus_stop: usize,
        minus_step: usize,
    ) -> SqBValueSequence<'s> {
        assert_ne!(minus_step, 0);
        assert_ne!(minus_start, 0);
        assert_ne!(minus_stop, 0);

        if minus_start >= minus_stop {
            return SqBValueSequence::empty();
        }

        let mut buff = VecDeque::new();
        let mut len = 0usize;
        for elem in iter {
            if len + 1 >= minus_stop {
                buff.pop_front();
            }
            len += 1;
            buff.push_back(elem);
        }

        SqBValueSequence::ExactSizeDoubleEndedIterator(Box::new(
            buff.into_iter()
                .rev()
                .skip(minus_start - 1)
                .step_by(minus_step),
        ))
    }
}

#[must_use]
#[derive(Debug)]
struct ExactSizeIteratorSliceFilter<'a> {
    parent: &'a SliceFilter<'a>,
}

impl<'a> ExactSizeIteratorSliceFilter<'a> {
    fn new(parent: &'a SliceFilter<'a>) -> Self {
        Self { parent }
    }

    fn filter<'s>(&self, iter: SqBValueExactSizeIterator<'s>) -> Result<SqBValueSequence<'s>> {
        use CategorizedInt::*;

        if iter.len() == 0 {
            return Ok(SqBValueSequence::ExactSizeIterator(iter));
        }

        let start = self.parent.start()?;
        let opt_stop = self.parent.stop()?;
        let step = self.parent.step()?;

        let start_before_0 = is_index_before_0(iter.len(), start);
        let ustart = match (&start, &step, start_before_0) {
            (Negative(_), NonNegative(_), true) => 0usize,
            (Negative(_), Negative(_), true) => {
                return Ok(SqBValueSequence::empty());
            }
            (Negative(minus_ustart), _, false) => iter.len() - minus_ustart,
            (NonNegative(ustart), _, _) => *ustart,
        };

        let stop_before_0 = opt_stop
            .map(|s| is_index_before_0(iter.len(), s))
            .unwrap_or(false);
        let opt_ustop = match (opt_stop, step, stop_before_0) {
            (Some(Negative(_)), NonNegative(_), true) => {
                return Ok(SqBValueSequence::empty());
            }
            (Some(Negative(_)), Negative(_), true) => None,
            (Some(Negative(minus_stop)), _, false) => Some(iter.len() - minus_stop),
            (Some(NonNegative(stop)), _, _) => Some(stop),
            (None, _, _) => None,
        };

        Ok(match (opt_ustop, step) {
            (Some(ustop), NonNegative(ustep)) => {
                self.with_stop_nonneg_step(iter, ustart, ustop, ustep)
            }
            (Some(ustop), Negative(uminus_step)) => {
                self.with_stop_neg_step(iter, ustart, ustop, uminus_step)
            }
            (None, NonNegative(ustep)) => self.with_no_stop_nonneg_step(iter, ustart, ustep),
            (None, Negative(uminus_step)) => self.with_no_stop_neg_step(iter, ustart, uminus_step),
        })
    }

    fn with_stop_nonneg_step<'s>(
        &self,
        iter: SqBValueExactSizeIterator<'s>,
        start: usize,
        stop: usize,
        step: usize,
    ) -> SqBValueSequence<'s> {
        assert_ne!(step, 0);
        assert_ne!(iter.len(), 0);

        SqBValueSequence::ExactSizeIterator(Box::new(
            iter.skip(start)
                .take(stop.saturating_sub(start))
                .step_by(step),
        ))
    }

    fn with_stop_neg_step<'s>(
        &self,
        iter: SqBValueExactSizeIterator<'s>,
        start: usize,
        stop: usize,
        minus_step: usize,
    ) -> SqBValueSequence<'s> {
        assert_ne!(minus_step, 0);
        assert_ne!(iter.len(), 0);

        let start = cmp::min(start, iter.len() - 1);
        let stop = cmp::min(stop, iter.len() - 1);

        if stop >= start {
            return SqBValueSequence::empty();
        }
        let step_offset = (start - stop - 1) % minus_step;
        let stop = stop + step_offset;

        SqBValueSequence::ExactSizeDoubleEndedIterator(Box::new(
            iter.skip(stop + 1)
                .take(start - stop)
                .step_by(minus_step)
                .collect::<Vec<_>>()
                .into_iter()
                .rev(),
        ))
    }

    fn with_no_stop_nonneg_step<'s>(
        &self,
        iter: SqBValueExactSizeIterator<'s>,
        start: usize,
        step: usize,
    ) -> SqBValueSequence<'s> {
        assert_ne!(step, 0);
        assert_ne!(iter.len(), 0);
        SqBValueSequence::ExactSizeIterator(Box::new(iter.skip(start).step_by(step)))
    }

    fn with_no_stop_neg_step<'s>(
        &self,
        iter: SqBValueExactSizeIterator<'s>,
        start: usize,
        minus_step: usize,
    ) -> SqBValueSequence<'s> {
        assert_ne!(minus_step, 0);
        assert_ne!(iter.len(), 0);

        if iter.len() == 0 {
            return SqBValueSequence::ExactSizeIterator(iter);
        }

        let start = cmp::min(start, iter.len() - 1);
        let skip = start % minus_step;

        SqBValueSequence::ExactSizeDoubleEndedIterator(Box::new(
            iter.skip(skip)
                .take(start + 1)
                .step_by(minus_step)
                .collect::<Vec<_>>()
                .into_iter()
                .rev(),
        ))
    }
}

#[must_use]
#[derive(Debug)]
struct DoubleEndedIteratorSliceFilter<'a> {
    parent: &'a SliceFilter<'a>,
}

impl<'a> DoubleEndedIteratorSliceFilter<'a> {
    fn new(parent: &'a SliceFilter<'a>) -> Self {
        Self { parent }
    }

    fn filter<'s>(&self, iter: SqBValueDoubleEndedIterator<'s>) -> Result<SqBValueSequence<'s>> {
        use CategorizedInt::*;

        let start = self.parent.start()?;
        let stop = self.parent.stop()?;
        let step = self.parent.step()?;

        Ok(match (start, stop, step) {
            (NonNegative(ustart), Some(NonNegative(ustop)), NonNegative(ustep)) => {
                self.with_nonneg_start_nonneg_stop_nonneg_step(iter, ustart, ustop, ustep)
            }
            (NonNegative(ustart), Some(NonNegative(ustop)), Negative(uminus_step)) => {
                self.with_nonneg_start_nonneg_stop_neg_step(iter, ustart, ustop, uminus_step)
            }
            (NonNegative(ustart), None, Negative(uminus_step)) => {
                self.with_nonneg_start_no_stop_neg_step(iter, ustart, uminus_step)
            }
            (NonNegative(ustart), None, NonNegative(ustep)) => {
                self.with_nonneg_start_no_stop_nonneg_step(iter, ustart, ustep)
            }
            (NonNegative(ustart), Some(Negative(uminus_stop)), NonNegative(ustep)) => {
                self.with_nonneg_start_neg_stop_nonneg_step(iter, ustart, uminus_stop, ustep)
            }
            (NonNegative(ustart), Some(Negative(uminus_stop)), Negative(uminus_step)) => {
                self.with_nonneg_start_neg_stop_neg_step(iter, ustart, uminus_stop, uminus_step)
            }
            (Negative(uminus_start), Some(NonNegative(ustop)), NonNegative(ustep)) => {
                self.with_neg_start_nonneg_stop_nonneg_step(iter, uminus_start, ustop, ustep)
            }
            (Negative(uminus_start), Some(NonNegative(ustop)), Negative(uminus_step)) => {
                self.with_neg_start_nonneg_stop_neg_step(iter, uminus_start, ustop, uminus_step)
            }
            (Negative(uminus_start), None, Negative(uminus_step)) => {
                self.with_neg_start_no_stop_neg_step(iter, uminus_start, uminus_step)
            }
            (Negative(uminus_start), None, NonNegative(ustep)) => {
                self.with_neg_start_no_stop_nonneg_step(iter, uminus_start, ustep)
            }
            (Negative(uminus_start), Some(Negative(uminus_stop)), NonNegative(ustep)) => {
                self.with_neg_start_neg_stop_nonneg_step(iter, uminus_start, uminus_stop, ustep)
            }
            (Negative(uminus_start), Some(Negative(uminus_stop)), Negative(uminus_step)) => {
                self.with_neg_start_neg_stop_neg_step(iter, uminus_start, uminus_stop, uminus_step)
            }
        })
    }

    fn with_nonneg_start_nonneg_stop_nonneg_step<'s>(
        &self,
        iter: SqBValueDoubleEndedIterator<'s>,
        start: usize,
        stop: usize,
        step: usize,
    ) -> SqBValueSequence<'s> {
        // We can't do any better than with just an Iterator
        IteratorSliceFilter::new(self.parent)
            .with_nonneg_start_nonneg_stop_nonneg_step(iter, start, stop, step)
    }

    fn with_nonneg_start_nonneg_stop_neg_step<'s>(
        &self,
        iter: SqBValueDoubleEndedIterator<'s>,
        start: usize,
        stop: usize,
        minus_step: usize,
    ) -> SqBValueSequence<'s> {
        // We can't do any better than with just an Iterator
        IteratorSliceFilter::new(self.parent)
            .with_nonneg_start_nonneg_stop_neg_step(iter, start, stop, minus_step)
    }

    fn with_nonneg_start_no_stop_nonneg_step<'s>(
        &self,
        iter: SqBValueDoubleEndedIterator<'s>,
        start: usize,
        step: usize,
    ) -> SqBValueSequence<'s> {
        // We can't do any better than with just an Iterator
        IteratorSliceFilter::new(self.parent)
            .with_nonneg_start_no_stop_nonneg_step(iter, start, step)
    }

    fn with_nonneg_start_no_stop_neg_step<'s>(
        &self,
        iter: SqBValueDoubleEndedIterator<'s>,
        start: usize,
        minus_step: usize,
    ) -> SqBValueSequence<'s> {
        // We can't do any better than with just an Iterator
        IteratorSliceFilter::new(self.parent)
            .with_nonneg_start_no_stop_neg_step(iter, start, minus_step)
    }

    fn with_nonneg_start_neg_stop_nonneg_step<'s>(
        &self,
        mut iter: SqBValueDoubleEndedIterator<'s>,
        start: usize,
        minus_stop: usize,
        step: usize,
    ) -> SqBValueSequence<'s> {
        assert_ne!(step, 0);
        assert_ne!(minus_stop, 0);

        if let Err(_) = iter.advance_back(minus_stop) {
            return SqBValueSequence::empty();
        }
        SqBValueSequence::Iterator(Box::new(iter.skip(start).step_by(step)))
    }

    fn with_nonneg_start_neg_stop_neg_step<'s>(
        &self,
        iter: SqBValueDoubleEndedIterator<'s>,
        start: usize,
        minus_stop: usize,
        minus_step: usize,
    ) -> SqBValueSequence<'s> {
        // We can't do any better than with just an Iterator
        IteratorSliceFilter::new(self.parent)
            .with_nonneg_start_neg_stop_neg_step(iter, start, minus_stop, minus_step)
    }

    fn with_neg_start_nonneg_stop_nonneg_step<'s>(
        &self,
        iter: SqBValueDoubleEndedIterator<'s>,
        minus_start: usize,
        stop: usize,
        step: usize,
    ) -> SqBValueSequence<'s> {
        // We can't do any better than with just an Iterator
        IteratorSliceFilter::new(self.parent).with_neg_start_nonneg_stop_nonneg_step(
            iter,
            minus_start,
            stop,
            step,
        )
    }

    fn with_neg_start_nonneg_stop_neg_step<'s>(
        &self,
        mut iter: SqBValueDoubleEndedIterator<'s>,
        minus_start: usize,
        stop: usize,
        minus_step: usize,
    ) -> SqBValueSequence<'s> {
        assert_ne!(minus_step, 0);
        assert_ne!(minus_start, 0);

        if let Err(_) = iter.advance(stop + 1) {
            return SqBValueSequence::empty();
        }

        SqBValueSequence::Iterator(Box::new(
            iter.rev().skip(minus_start - 1).step_by(minus_step),
        ))
    }

    fn with_neg_start_no_stop_nonneg_step<'s>(
        &self,
        iter: SqBValueDoubleEndedIterator<'s>,
        minus_start: usize,
        step: usize,
    ) -> SqBValueSequence<'s> {
        assert_ne!(step, 0);
        assert_ne!(minus_start, 0);

        SqBValueSequence::ExactSizeDoubleEndedIterator(Box::new(
            iter.rev()
                .take(minus_start)
                .collect::<Vec<_>>()
                .into_iter()
                .rev()
                .step_by(step),
        ))
    }

    fn with_neg_start_no_stop_neg_step<'s>(
        &self,
        iter: SqBValueDoubleEndedIterator<'s>,
        minus_start: usize,
        minus_step: usize,
    ) -> SqBValueSequence<'s> {
        assert_ne!(minus_step, 0);
        assert_ne!(minus_start, 0);

        SqBValueSequence::Iterator(Box::new(
            iter.rev().skip(minus_start - 1).step_by(minus_step),
        ))
    }

    fn with_neg_start_neg_stop_nonneg_step<'s>(
        &self,
        iter: SqBValueDoubleEndedIterator<'s>,
        minus_start: usize,
        minus_stop: usize,
        step: usize,
    ) -> SqBValueSequence<'s> {
        assert_ne!(step, 0);
        assert_ne!(minus_start, 0);
        assert_ne!(minus_stop, 0);

        SqBValueSequence::DoubleEndedIterator(Box::new(
            iter.rev()
                .skip(minus_stop)
                .take(minus_start.saturating_sub(minus_stop))
                .collect::<Vec<_>>()
                .into_iter()
                .rev()
                .step_by(step),
        ))
    }

    fn with_neg_start_neg_stop_neg_step<'s>(
        &self,
        iter: SqBValueDoubleEndedIterator<'s>,
        minus_start: usize,
        minus_stop: usize,
        minus_step: usize,
    ) -> SqBValueSequence<'s> {
        assert_ne!(minus_step, 0);
        assert_ne!(minus_start, 0);
        assert_ne!(minus_stop, 0);

        SqBValueSequence::Iterator(Box::new(
            iter.rev()
                .skip(minus_start - 1)
                .take(minus_stop.saturating_sub(minus_start))
                .step_by(minus_step),
        ))
    }
}

#[must_use]
#[derive(Debug)]
struct ExactSizeDoubleEndedIteratorSliceFilter<'a> {
    parent: &'a SliceFilter<'a>,
}

impl<'a> ExactSizeDoubleEndedIteratorSliceFilter<'a> {
    fn new(parent: &'a SliceFilter<'a>) -> Self {
        Self { parent }
    }

    fn filter<'s>(
        &self,
        iter: SqBValueExactSizeDoubleEndedIterator<'s>,
    ) -> Result<SqBValueSequence<'s>> {
        use CategorizedInt::*;
        // TODO: remove duplication with ExactSizeIteratorSliceFilter

        if iter.len() == 0 {
            return Ok(SqBValueSequence::ExactSizeDoubleEndedIterator(iter));
        }

        let start = self.parent.start()?;
        let opt_stop = self.parent.stop()?;
        let step = self.parent.step()?;

        let start_before_0 = is_index_before_0(iter.len(), start);
        let ustart = match (&start, &step, start_before_0) {
            (Negative(_), NonNegative(_), true) => 0usize,
            (Negative(_), Negative(_), true) => {
                return Ok(SqBValueSequence::empty());
            }
            (Negative(minus_ustart), _, false) => iter.len() - minus_ustart,
            (NonNegative(ustart), _, _) => *ustart,
        };

        let stop_before_0 = opt_stop
            .map(|s| is_index_before_0(iter.len(), s))
            .unwrap_or(false);
        let opt_ustop = match (opt_stop, step, stop_before_0) {
            (Some(Negative(_)), NonNegative(_), true) => {
                return Ok(SqBValueSequence::empty());
            }
            (Some(Negative(_)), Negative(_), true) => None,
            (Some(Negative(minus_stop)), _, false) => Some(iter.len() - minus_stop),
            (Some(NonNegative(stop)), _, _) => Some(stop),
            (None, _, _) => None,
        };

        Ok(match (opt_ustop, step) {
            (Some(ustop), NonNegative(ustep)) => {
                self.with_stop_nonneg_step(iter, ustart, ustop, ustep)
            }
            (Some(ustop), Negative(uminus_step)) => {
                self.with_stop_neg_step(iter, ustart, ustop, uminus_step)
            }
            (None, NonNegative(ustep)) => self.with_no_stop_nonneg_step(iter, ustart, ustep),
            (None, Negative(uminus_step)) => self.with_no_stop_neg_step(iter, ustart, uminus_step),
        })
    }

    fn with_stop_nonneg_step<'s>(
        &self,
        iter: SqBValueExactSizeDoubleEndedIterator<'s>,
        start: usize,
        stop: usize,
        step: usize,
    ) -> SqBValueSequence<'s> {
        assert_ne!(step, 0);
        assert_ne!(iter.len(), 0);

        SqBValueSequence::ExactSizeDoubleEndedIterator(Box::new(
            iter.skip(start)
                .take(stop.saturating_sub(start))
                .step_by(step),
        ))
    }

    fn with_stop_neg_step<'s>(
        &self,
        iter: SqBValueExactSizeDoubleEndedIterator<'s>,
        start: usize,
        stop: usize,
        minus_step: usize,
    ) -> SqBValueSequence<'s> {
        assert_ne!(minus_step, 0);
        assert_ne!(iter.len(), 0);

        let uminus_start = iter.len() - cmp::min(start, iter.len() - 1);
        let uminus_stop = iter.len() - cmp::min(stop, iter.len() - 1);

        SqBValueSequence::ExactSizeDoubleEndedIterator(Box::new(
            iter.rev()
                .skip(uminus_start - 1)
                .take(uminus_stop.saturating_sub(uminus_start))
                .step_by(minus_step),
        ))
    }

    fn with_no_stop_nonneg_step<'s>(
        &self,
        iter: SqBValueExactSizeDoubleEndedIterator<'s>,
        start: usize,
        step: usize,
    ) -> SqBValueSequence<'s> {
        assert_ne!(step, 0);
        assert_ne!(iter.len(), 0);
        SqBValueSequence::ExactSizeDoubleEndedIterator(Box::new(iter.skip(start).step_by(step)))
    }

    fn with_no_stop_neg_step<'s>(
        &self,
        iter: SqBValueExactSizeDoubleEndedIterator<'s>,
        start: usize,
        minus_step: usize,
    ) -> SqBValueSequence<'s> {
        assert_ne!(minus_step, 0);
        assert_ne!(iter.len(), 0);

        let uminus_start = iter.len() - cmp::min(start, iter.len() - 1);

        SqBValueSequence::ExactSizeDoubleEndedIterator(Box::new(
            iter.rev().skip(uminus_start - 1).step_by(minus_step),
        ))
    }
}

/// Get whether a given index into a sequence refers to an element before the start or not.
fn is_index_before_0(seq_len: usize, index: CategorizedInt) -> bool {
    if let CategorizedInt::Negative(minus_index) = index {
        minus_index > seq_len
    } else {
        false
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;
    use strum::IntoEnumIterator;

    use super::*;
    use crate::filter::test_util::{
        fake_field_call_ast, fake_int_literal, gen_test_sequence, SequenceType,
    };
    use crate::schema;

    // --------------------------------------------------------------------------------------------
    // SliceFilter tests
    // --------------------------------------------------------------------------------------------
    fn fake_slice(start: Option<i64>, stop: Option<i64>, step: Option<i64>) -> ast::Slice {
        ast::Slice {
            span: (0, 0).into(),
            opt_start: start.map(fake_int_literal),
            opt_stop: stop.map(fake_int_literal),
            opt_step: step.map(fake_int_literal),
        }
    }

    fn test_slice_case(
        start: Option<i64>,
        stop: Option<i64>,
        step: Option<i64>,
        seq_type: SequenceType,
        seq_len: usize,
    ) -> std::result::Result<(), String> {
        let field_call_ast = fake_field_call_ast();
        let call_info = FieldCallInfo::new(&field_call_ast, schema::root_field());
        let seq = gen_test_sequence(seq_type, seq_len);
        let slice_ast = fake_slice(start, stop, step);
        let filter = SliceFilter::new(&call_info, &slice_ast);
        let got = filter
            .filter(seq)
            .unwrap()
            .map(|f| i64::try_from(f.unwrap().get_primitive(&call_info).unwrap()).unwrap())
            .collect::<Vec<_>>();

        let expected = slyce::Slice {
            start: start.map(|i| isize::try_from(i).unwrap()).into(),
            end: stop.map(|i| isize::try_from(i).unwrap()).into(),
            step: step.map(|i| isize::try_from(i).unwrap()).into(),
        }
        .apply(
            (0..i64::try_from(seq_len).unwrap())
                .collect::<Vec<_>>()
                .as_slice(),
        )
        .copied()
        .collect::<Vec<_>>();

        if got != expected {
            Err(format!(
                    "got != expected for [{:?}:{:?}:{:?}] seq_type={:?}, seq_len={:?}, got={:?}, expected={:?}",
                    start, stop, step, seq_type, seq_len, got, expected
                    ))
        } else {
            Ok(())
        }
    }

    #[test]
    fn test_slice() -> std::result::Result<(), String> {
        let mut starts = (-5..5).map(Some).collect::<Vec<_>>();
        starts.push(None);

        let mut stops = (-5..5).map(Some).collect::<Vec<_>>();
        stops.push(None);

        let mut steps = (-5..-1).chain(1..5).map(Some).collect::<Vec<_>>();
        steps.push(None);

        // element types in the input iterators to itertools::multi_cartesian_product must be the
        // same, so convert the SequenceTypes to i64s and wrap in Some.
        let seq_types = SequenceType::iter()
            .map(|v| Some(v as i64))
            .collect::<Vec<_>>();

        // element types in the input iterators to itertools::multi_cartesian_product must be the
        // same, so wrap the sequence lengths in Some.
        let seq_lens = (0..5).map(Some).collect::<Vec<_>>();

        let errors = vec![
            starts.into_iter(),
            stops.into_iter(),
            steps.into_iter(),
            seq_types.into_iter(),
            seq_lens.into_iter(),
        ]
        .into_iter()
        .multi_cartesian_product()
        .filter_map(|case| {
            test_slice_case(
                case[0],
                case[1],
                case[2],
                SequenceType::from_repr(usize::try_from(case[3].unwrap()).unwrap()).unwrap(),
                usize::try_from(case[4].unwrap()).unwrap(),
            )
            .err()
        })
        .collect::<Vec<_>>();

        let num_errors = errors.len();
        if num_errors == 0 {
            Ok(())
        } else {
            for error in errors {
                println!("{}", error);
            }
            Err(format!("Failed slice tests: {}", num_errors))
        }
    }
}
