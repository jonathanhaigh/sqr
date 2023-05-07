// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

//! Code for filtering sequences of results based on filter specifications in the query.

use std::fmt;

use crate::error::Result;
pub use crate::filter::comparison::ComparisonFilter;
pub use crate::filter::identity::IdentityFilter;
pub use crate::filter::index::IndexFilter;
pub use crate::filter::slice::SliceFilter;
use crate::sqvalue::{SqBValue, SqBValueSequence};

mod comparison;
mod identity;
mod index;
mod slice;

/// An object for filtering sequences of fields based on a filter specification in the query.
///
/// # Variants
/// There are two variants of filter that have different signatures:
/// - `SequenceToSingle`: filters a list into a single item;
/// - `SequenceToSequence`: filters a sequence into another sequence.
///
/// # Lifetimes
/// - `'a`: the lifetime of the underlying filter trait object.
#[must_use]
#[derive(Debug)]
pub enum Filter<'a> {
    SequenceToSingle(Box<dyn SequenceToSingleFilter + 'a>),
    SequenceToSequence(Box<dyn SequenceToSequenceFilter + 'a>),
}

impl<'a> Default for Filter<'a> {
    fn default() -> Self {
        Self::SequenceToSequence(Box::new(IdentityFilter::new()))
    }
}

/// A filter that filters a sequence into another sequence.
pub trait SequenceToSequenceFilter: fmt::Debug {
    /// Filter a sequence into another sequence.
    ///
    /// Note that the lifetime of the returned sequence is the intersection of the lifetimes of the
    /// filter and the original sequence. This allows the returned sequence to hold references to
    /// the filter.
    fn filter<'a>(&'a self, seq: SqBValueSequence<'a>) -> Result<SqBValueSequence<'a>>;
}

/// A filter that filters a sequence into a single value.
pub trait SequenceToSingleFilter: fmt::Debug {
    /// Filter a sequence into a single element.
    fn filter(&self, seq: SqBValueSequence) -> Result<SqBValue>;
}
