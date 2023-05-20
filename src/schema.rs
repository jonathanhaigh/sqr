// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

//! Provides access to the SQ type and field schema.

use phf::{phf_map, phf_ordered_map};

use crate::fieldcall::FieldSequenceType;
use crate::primitive::{Primitive, PrimitiveKind};

type TypeMap = phf::Map<&'static str, TypeSchema>;
type FieldMap = phf::Map<&'static str, FieldSchema>;
type ParamMap = phf::OrderedMap<&'static str, ParamSchema>;

/// Schema of an SQ type
#[derive(Debug)]
pub struct TypeSchema {
    name: &'static str,
    doc: &'static str,
    primitive_coercion: PrimitiveKind,
    fields: FieldMap,
}

impl TypeSchema {
    /// Get the name of the SQ type.
    pub fn name(&self) -> &str {
        self.name
    }

    /// Get the documentation for the SQ type.
    pub fn doc(&self) -> &str {
        self.doc
    }

    /// Get the primitive type to which the SQ type can be coerced.
    pub fn primitive_coercion(&self) -> PrimitiveKind {
        self.primitive_coercion
    }

    /// Get the schema of a field of the SQ type.
    ///
    /// # Parameters
    /// - `name` the name of the field to get the schema for.
    ///
    /// # Returns
    /// - The requested field's schema if the field name was valid.
    /// - `None` if the field name was invalid.
    pub fn get_field(&self, name: &str) -> Option<&FieldSchema> {
        self.fields.get(name)
    }

    /// Get an iterator over the SQ type's fields' schemas.
    pub fn fields(&self) -> phf::map::Values<&str, FieldSchema> {
        self.fields.values()
    }
}

/// Schema of a field of an SQ type.
#[derive(Debug)]
pub struct FieldSchema {
    name: &'static str,
    doc: &'static str,
    parent_type: &'static str,
    return_type: &'static str,
    return_sequence_type: FieldSequenceType,
    params: ParamMap,
}

impl FieldSchema {
    /// Get the name of the field.
    pub fn name(&self) -> &str {
        self.name
    }

    /// Get the documentation for the field.
    pub fn doc(&self) -> &str {
        self.doc
    }

    /// Get the schema of the SQ type that owns the field.
    pub fn parent_type(&self) -> &TypeSchema {
        TYPES.get(self.parent_type).unwrap()
    }

    /// Get the schema of the SQ type that the field returns.
    pub fn return_type(&self) -> &TypeSchema {
        TYPES.get(self.return_type).unwrap()
    }

    /// Get the sequence type that the field returns.
    ///
    /// # Return
    /// - `Single`: if the field returns a single value.
    /// - `Option`: if the field returns an optional value.
    /// - `Sequence`: if the field returns a sequence of values.
    pub fn return_sequence_type(&self) -> FieldSequenceType {
        self.return_sequence_type
    }

    /// Get an iterator over the schemas of the field's parameters.
    pub fn params(&self) -> phf::ordered_map::Values<&str, ParamSchema> {
        self.params.values()
    }

    /// Get the schema of a parameter of the field.
    ///
    /// # Parameters
    /// - `name`: the name of the parameter to get the schema for.
    ///
    /// # Return
    /// - The parameter's schema if `name` is valid parameter name.
    /// - `None` if `name` is not a valid parameter name.
    pub fn param_by_name(&self, name: &str) -> Option<&ParamSchema> {
        self.params.get(name)
    }
}

/// Schema of a parameter of a field of an SQ type.
#[derive(Debug)]
pub struct ParamSchema {
    index: usize,
    name: &'static str,
    doc: &'static str,
    ty: PrimitiveKind,
    required: bool,
    default: DefaultValue,
}

impl ParamSchema {
    /// Get the index of the parameter.
    ///
    /// This is the position in a arg pack that the arg for the parameter must take when not
    /// named.
    pub fn index(&self) -> usize {
        self.index
    }

    /// Get the name of the parameter.
    pub fn name(&self) -> &str {
        self.name
    }

    /// Get the documentation for the parameter.
    pub fn doc(&self) -> &str {
        self.doc
    }

    /// Get the schema for the SQ primitive type of the parameter.
    ///
    /// This is an SQ primitive type (i.e. Int, Float, Bool or Str).
    pub fn ty(&self) -> PrimitiveKind {
        self.ty
    }

    /// Get whether the parameter is mandatory.
    pub fn required(&self) -> bool {
        self.required
    }

    /// Get the default value for the parameter if it has one.
    pub fn default(&self) -> Option<Primitive> {
        self.default.to_primitive()
    }
}

/// A representation of a parameter's default value
#[derive(Clone, Debug, PartialEq)]
pub enum DefaultValue {
    Int(i128),
    Float(f64),
    Str(&'static str),
    Bool(bool),
    Null,
}

impl DefaultValue {
    /// Convert a default value schema into a `Primitive` containing its value.
    pub fn to_primitive(&self) -> Option<Primitive> {
        match self {
            DefaultValue::Int(i) => Some(Primitive::Int(*i)),
            DefaultValue::Float(f) => Some(Primitive::Float(*f)),
            DefaultValue::Str(s) => Some(Primitive::Str(s.to_string())),
            DefaultValue::Bool(b) => Some(Primitive::Bool(*b)),
            DefaultValue::Null => None,
        }
    }
}

/// Get an SQ type's schema.
///
/// # Parameters
/// - name: the name of the SQ type.
///
/// # Return
/// - The type's schema if `name` is a valid SQ type name.
/// - `None` if `name` is an invalid SQ type name.
pub fn get_type(name: &str) -> Option<&'static TypeSchema> {
    TYPES.get(name)
}

/// Get an iterator over all SQ type schemas.
pub fn types() -> phf::map::Values<'static, &'static str, TypeSchema> {
    TYPES.values()
}

/// Get the schema of the SQ root type (`SqRoot`).
pub fn root_type() -> &'static TypeSchema {
    TYPES.get(ROOT_TYPE_NAME).unwrap()
}

/// Get a field schema representing access of the SQ root type (`SqRoot`).
///
/// Such a field doesn't really exist - the root type isn't accessed via a field call - but it
/// makes some code simpler if all SQ type values can have a field schema associated with them.
///
/// Note that the `name()`, `doc()` and `parent_type()` methods don't really make sense for the
/// root field and shouldn't be exposed to users.
pub fn root_field() -> &'static FieldSchema {
    &DUMMY_ROOT_FIELD
}

static DUMMY_ROOT_FIELD: FieldSchema = FieldSchema {
    name: "root",
    doc: "root field",
    parent_type: ROOT_TYPE_NAME,
    return_type: ROOT_TYPE_NAME,
    return_sequence_type: FieldSequenceType::Single,
    params: phf_ordered_map!(),
};

// the schema data is generated in `build.rs` and included here.
include!(concat!(env!("OUT_DIR"), "/schema.gen.rs"));
