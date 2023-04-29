// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

//! Functions to generate the results of an SQ query.

use std::cell::Cell;
use std::collections::HashMap;

use miette::SourceSpan;
use ser::{Serialize, SerializeMap, SerializeSeq, Serializer};
use serde::ser;

use crate::ast;
use crate::error::{Error, Result};
use crate::fieldcall::{FieldAccessKind, FieldCallInfoTree, FieldSequenceType};
use crate::filter;
use crate::sqvalue::{SqBValue, SqBValueSequence};

type StdResult<T, E> = std::result::Result<T, E>;

/// Trait for accessing an a `Cell<Option<Error>>`.
///
/// We're going to generate our results as we serialize them so that we don't have to keep all the
/// results in memory at once. Unfortunately, we can't return arbitrary errors from a Serde
/// `serialize()` method, so we can't directly return errors containing the span of the query that
/// caused the error.
///
/// To work around this, we embed a `Cell<Option<Error>>` into the objects that we'll serialize. If
/// an error happens during serialization, we can set this error object to pass it back to the
/// caller. The error must be wrapped in a `Cell<T>` because `serialize()` methods take self by
/// shared reference so woudn't be able to mutate a plain `Option<Error>`.
trait WithErrorCell {
    /// For implementers to provide access to their embedded `Cell<Option<Error>>`
    fn error_cell(&self) -> &Cell<Option<Box<Error>>>;

    /// Set the error in the `Cell<Option<Error>>` and create and return an error with the same
    /// message that is suitable to be returned from a `serialize()` method.
    ///
    /// # Parameters:
    /// `e`: The `Error` to set.
    ///
    /// # Type Parameters:
    /// `S`: The type of `Serializer` passed to the `serialize()` method.
    fn set_and_convert_error<S: Serializer>(&self, e: Box<Error>) -> S::Error {
        let msg = e.to_string();
        self.error_cell().set(Some(e));
        ser::Error::custom(msg)
    }

    /// Retrieve any error stored in the `Cell<Option<Error>>`.
    fn take_error(&self) -> Option<Box<Error>> {
        self.error_cell().take()
    }
}

/// A boxed SQ value that can be serialized.
struct SerializableSqBValue<'a> {
    call_tree: &'a FieldCallInfoTree<'a>,
    value: SqBValue,
    error: &'a Cell<Option<Box<Error>>>,
}

impl<'a> WithErrorCell for SerializableSqBValue<'a> {
    fn error_cell(&self) -> &'a Cell<Option<Box<Error>>> {
        self.error
    }
}

impl<'a> SerializableSqBValue<'a> {
    /// Get the result of a field call with a return sequence type of `Single`.
    ///
    /// # Parameters
    /// - `child_call_tree`: the `FieldCallInfoTree` for the child being accessed.
    fn get_child_single<S: Serializer>(
        &self,
        child_call_tree: &'a FieldCallInfoTree<'a>,
    ) -> StdResult<SerializableSqBValue<'a>, S::Error> {
        let child = self
            .value
            .get_single(&child_call_tree.info)
            .map_err(|e| self.set_and_convert_error::<S>(e))?;
        Ok(SerializableSqBValue {
            call_tree: child_call_tree,
            value: child,
            error: self.error,
        })
    }

    /// Get the result of a field call with a return sequence type of `Option`.
    ///
    /// # Parameters
    /// - `child_call_tree`: the `FieldCallInfoTree` for the child being accessed.
    fn get_child_option<S: Serializer>(
        &self,
        child_call_tree: &'a FieldCallInfoTree<'a>,
    ) -> StdResult<Option<SerializableSqBValue<'a>>, S::Error> {
        match self
            .value
            .get_option(&child_call_tree.info)
            .map_err(|e| self.set_and_convert_error::<S>(e))?
        {
            Some(child) => Ok(Some(SerializableSqBValue {
                call_tree: child_call_tree,
                value: child,
                error: self.error,
            })),
            None => Ok(None),
        }
    }

    /// Get the result of a field call with a return sequence type of `Sequence`.
    ///
    /// # Parameters
    /// - `child_call_tree`: the `FieldCallInfoTree` for the child being accessed.
    fn get_child_sequence<S: Serializer>(
        &self,
        child_call_tree: &'a FieldCallInfoTree<'a>,
    ) -> StdResult<SerializableSqBValueSequence, S::Error> {
        let child_seq = self
            .value
            .get_sequence(&child_call_tree.info)
            .map_err(|e| self.set_and_convert_error::<S>(e))?;
        Ok(SerializableSqBValueSequence {
            call_tree: child_call_tree,
            sequence: Cell::new(child_seq),
            error: self.error,
        })
    }

    /// Serialize a field access as a standalone value (not part of a map).
    ///
    /// # Parameters
    /// - `child_call_tree`: the `FieldCallInfoTree` for the child to serialize.
    /// - `serializer`: the `Serializer` to be used to serialize the child.
    fn serialize_child_data<S: Serializer>(
        &self,
        child_call_tree: &FieldCallInfoTree<'a>,
        serializer: S,
    ) -> StdResult<S::Ok, S::Error> {
        match child_call_tree.info.return_sequence_type() {
            FieldSequenceType::Single => self
                .get_child_single::<S>(child_call_tree)?
                .serialize(serializer),
            FieldSequenceType::Option => self
                .get_child_option::<S>(child_call_tree)?
                .serialize(serializer),
            FieldSequenceType::Sequence => self
                .get_child_sequence::<S>(child_call_tree)?
                .serialize(serializer),
        }
    }

    /// Serialize a field access as part of a map.
    /// # Parameters
    /// - `child_call_tree`: the `FieldCallInfoTree` for the child to serialize.
    /// - `map`: the `Serializer::SerializeMap` to be used to serialize the child.
    fn serialize_child_data_to_map<S: Serializer>(
        &self,
        child_call_tree: &'a FieldCallInfoTree<'a>,
        map: &mut S::SerializeMap,
    ) -> StdResult<(), S::Error> {
        match child_call_tree.info.return_sequence_type() {
            FieldSequenceType::Single => map.serialize_entry(
                &child_call_tree.info.field_name(),
                &self.get_child_single::<S>(child_call_tree)?,
            ),
            FieldSequenceType::Option => map.serialize_entry(
                &child_call_tree.info.field_name(),
                &self.get_child_option::<S>(child_call_tree)?,
            ),
            FieldSequenceType::Sequence => map.serialize_entry(
                &child_call_tree.info.field_name(),
                &self.get_child_sequence::<S>(child_call_tree)?,
            ),
        }
    }
}

impl<'a> Serialize for SerializableSqBValue<'a> {
    /// Serialize a field call
    fn serialize<S: Serializer>(&self, serializer: S) -> StdResult<S::Ok, S::Error> {
        // If this is a leaf node then we have no children to serialize so we convert the SQValue
        // into a primitive value and serialize that.
        if self.call_tree.children.is_empty() {
            return self
                .value
                .get_primitive(&self.call_tree.info)
                .map_err(|e| self.set_and_convert_error::<S>(e))?
                .serialize(serializer);
        }

        // If a field is accessed using the pullup operator then we serialize the result here in
        // place of its parent.
        if self.call_tree.children.len() == 1
            && self.call_tree.children[0].info.access_kind() == FieldAccessKind::Pullup
        {
            let child_call_tree = self.call_tree.children.first().unwrap();
            return self.serialize_child_data(child_call_tree, serializer);
        }

        // If we get to here then we need to serialize a map of children.

        // Keep track of fields that we've already seen so that we can detect duplicates.
        let mut seen_fields: HashMap<&str, SourceSpan> = HashMap::new();

        let mut map = serializer.serialize_map(Some(self.call_tree.children.len()))?;
        for child_call_tree in self.call_tree.children.as_slice() {
            let name = child_call_tree.info.field_name();
            // Pullup access is only allowed if the field access has no siblings
            if child_call_tree.info.access_kind() == FieldAccessKind::Pullup {
                return Err(
                    self.set_and_convert_error::<S>(Box::new(Error::PullupWithSiblings {
                        span: child_call_tree.info.ast().opt_field_access.unwrap().span,
                    })),
                );
            }
            if let Some(prev_span) = seen_fields.insert(name, child_call_tree.info.ast().ident.span)
            {
                return Err(
                    self.set_and_convert_error::<S>(Box::new(Error::DuplicateField {
                        span: child_call_tree.info.ast().ident.span,
                        prev_span,
                        type_name: self.value.get_type_name().to_owned(),
                        field_name: name.to_owned(),
                    })),
                );
            }
            self.serialize_child_data_to_map::<S>(child_call_tree, &mut map)?;
        }
        map.end()
    }
}

/// A sequence of SQ values that can be serialized
struct SerializableSqBValueSequence<'a, 's> {
    call_tree: &'a FieldCallInfoTree<'a>,
    sequence: Cell<SqBValueSequence<'s>>,
    error: &'a Cell<Option<Box<Error>>>,
}

impl<'a, 's> SerializableSqBValueSequence<'a, 's> {
    fn take_seq(&self) -> SqBValueSequence<'s> {
        self.sequence
            .replace(SqBValueSequence::Iterator(Box::new(std::iter::empty())))
    }
}

impl<'a, 's> WithErrorCell for SerializableSqBValueSequence<'a, 's> {
    fn error_cell(&self) -> &'a Cell<Option<Box<Error>>> {
        self.error
    }
}

impl<'a, 's> Serialize for SerializableSqBValueSequence<'a, 's> {
    /// Serialize the sequence of SQ values.
    fn serialize<S: Serializer>(&self, serializer: S) -> StdResult<S::Ok, S::Error> {
        let filter = self
            .call_tree
            .info
            .filter()
            .map_err(|e| self.set_and_convert_error::<S>(e))?;

        let filtered_sequence = match &filter {
            filter::Filter::SequenceToSingle(f) => {
                // If we have a filter that filters a sequence into a single element then we should
                // get that value then switch to the serialization routine for a single SQ value.
                let seq = self.take_seq();
                let value = f
                    .filter(seq)
                    .map_err(|e| self.set_and_convert_error::<S>(e))?;
                return SerializableSqBValue {
                    call_tree: self.call_tree,
                    value,
                    error: self.error,
                }
                .serialize(serializer);
            }
            filter::Filter::SequenceToSequence(f) => f
                .filter(self.take_seq())
                .map_err(|e| self.set_and_convert_error::<S>(e))?,
        };

        let mut seq = serializer.serialize_seq(None)?;
        for item in filtered_sequence {
            let item = item.map_err(|e| self.set_and_convert_error::<S>(e))?;

            seq.serialize_element(&SerializableSqBValue {
                call_tree: self.call_tree,
                value: item,
                error: self.error,
            })?;
        }
        seq.end()
    }
}

/// Generate and serialize the results of a query.
///
/// # Parameters
/// - `query_ast`: the AST node representing the whole query.
/// - `serializer`: the serializer used to serialize the results.
/// - `system_root`: the root SQ value where all queries begin.
pub fn generate_results<S: Serializer>(
    query_ast: &ast::Query,
    serializer: S,
    system_root: SqBValue,
) -> Result<()> {
    let error: Cell<Option<Box<Error>>> = Cell::new(None);
    let ser_val = SerializableSqBValue {
        call_tree: &FieldCallInfoTree::new(query_ast)?,
        value: system_root,
        error: &error,
    };
    match ser_val.serialize(serializer) {
        Err(e) => {
            // `serialize()` can't return arbitrary error types, so to get errors with spans of the
            // query where the error occurred, we need to access them from where they are saved
            // inside the object being serialized.
            if let Some(e) = error.take() {
                Err(e)
            } else {
                Err(Box::new(Error::Serialize { msg: e.to_string() }))
            }
        }
        Ok(_) => Ok(()),
    }
}
