// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

//! Types related to field calls.

use std::fmt;

use crate::ast;
use crate::error::{Error, OptResult, Result};
use crate::filter;
use crate::primitive::Primitive;
use crate::schema;
use crate::schema::{FieldSchema, TypeSchema};
use crate::util::{return_none_or_err, TryAsRef};

/// The sequence type returned by a field call.
#[must_use]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum FieldSequenceType {
    /// For field calls that return a single value.
    Single,
    /// For field calls that return an optional value.
    Option,
    /// For field calls that return a sequence of values.
    Sequence,
}

impl fmt::Display for FieldSequenceType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

/// The access type of a field.
#[must_use]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum FieldAccessKind {
    /// A normal field access.
    Normal,
    ///  A field access that uses the pullup operator (`<`) to hoist the returned data into its
    ///  parent's place in the output.
    Pullup,
}

/// Information about a field access.
///
/// # Lifetimes
/// * `'a`: the lifetime of the AST that a `FieldCallInfo` references.
#[must_use]
#[derive(Clone, Debug)]
pub struct FieldCallInfo<'a> {
    ast: &'a ast::FieldCall,
    schema: &'static FieldSchema,
}

impl<'a> FieldCallInfo<'a> {
    /// Create a new `FieldCallInfo` object.
    ///
    /// # Parameters
    /// - `ast`: the AST node representing the field call.
    /// - `schema`: the schema for the field.
    pub fn new(ast: &'a ast::FieldCall, schema: &'static FieldSchema) -> Self {
        // TODO: We could return a Result here and do some checks:
        // * That all args are valid.
        // * That required args are present. This is currently done at the time a field is called
        //   when generating results, but we could catch errors earlier (before results start
        //   streaming to the serializer).
        // * That filters are valid. This is currently done at the time the filter is used when
        //   generating results, but we could catch errors earlier.
        Self { ast, schema }
    }

    /// Get the AST node corresponding to the field access.
    pub fn ast(&self) -> &'a ast::FieldCall {
        self.ast
    }

    /// Get the schema of the field being accessed.
    pub fn schema(&self) -> &'static FieldSchema {
        self.schema
    }

    /// Get the name of the field being accessed.
    pub fn field_name(&self) -> &'static str {
        self.schema.name()
    }

    /// Get the access kind of the field being accessed.
    ///
    /// I.e. whether the field access was a normal or pullup access.
    pub fn access_kind(&self) -> FieldAccessKind {
        self.ast
            .opt_field_access
            .map(|fa| fa.kind)
            .unwrap_or(FieldAccessKind::Normal)
    }

    /// Get the return type of the field access.
    pub fn return_type(&self) -> &'static TypeSchema {
        self.schema.return_type()
    }

    /// Get the return sequence type of the field access.
    ///
    /// I.e. whether the field returns a single value, an optional value or a sequence of values.
    pub fn return_sequence_type(&self) -> FieldSequenceType {
        self.schema.return_sequence_type()
    }

    /// Get the schema for the type on which the field is being accessed.
    pub fn type_schema(&self) -> &'static TypeSchema {
        self.schema.parent_type()
    }

    /// Get the name of the type on which the field is being accessed.
    pub fn type_name(&self) -> &'static str {
        self.type_schema().name()
    }

    /// Get an argument passed to the field call.
    ///
    /// # Parameters
    /// - `index`: the zero-based index of the parameter for which an argument is requested.
    /// - `name`: the name of the parameter for which an argument is requested.
    ///
    /// # Type Parameters
    /// - `T` - the required type of the argument. This should be the underlying Rust data type
    /// (`i64`, `f64`, `bool` or `str`).
    ///
    /// # Returns
    /// - A reference to the value of the argument if it is found in the AST.
    /// - `Error::ArgMissing`: if the argument is not found.
    /// - `Error::ArgTypeMismatch`: if the type of the argument does not match `T`.
    pub fn opt_arg<T: ?Sized>(&self, index: usize, name: &str) -> OptResult<&T>
    where
        Primitive: TryAsRef<T>,
    {
        let arg_pack = return_none_or_err!(Ok(self.ast.opt_arg_pack.as_ref()));

        let literal = if index < arg_pack.pos_args.len() {
            &arg_pack.pos_args[index]
        } else {
            let opt_named_arg = &arg_pack
                .named_args
                .iter()
                .find(|arg| arg.ident.name == name);

            if let Some(named_arg) = opt_named_arg {
                &named_arg.literal
            } else {
                return Err(self.arg_missing_error(name));
            }
        };

        match literal.value.try_as_ref() {
            Some(v) => Ok(Some(v)),
            None => Err(self.arg_type_mismatch_error(name, literal)),
        }
    }

    /// Get an optional argument passed to the field call.
    ///
    /// This method acts like the `arg` method, except that it returns an `Option<&T>` rather than
    /// a `&T` and it returns `Ok(None)` rather than an error if the argument is not found.
    pub fn arg<T: ?Sized>(&self, index: usize, name: &str) -> Result<&T>
    where
        Primitive: TryAsRef<T>,
    {
        match self.opt_arg(index, name) {
            Ok(Some(v)) => Ok(v),
            Ok(None) => Err(self.arg_missing_error(name)),
            Err(e) => Err(e),
        }
    }

    /// Get the filter specified in the field call.
    pub fn filter(&self) -> Result<filter::Filter> {
        let Some(filter_pack) = self.ast.opt_filter_pack.as_ref() else {return Ok(filter::Filter::default())};
        let Some(filter) = filter_pack.opt_filter.as_ref() else {return Ok(filter::Filter::default())};

        Ok(match filter {
            ast::Filter::Index(int_literal) => filter::Filter::SequenceToSingle(Box::new(
                filter::IndexFilter::new(self, int_literal),
            )),
            ast::Filter::Slice(slice) => {
                filter::Filter::SequenceToSequence(Box::new(filter::SliceFilter::new(self, slice)))
            }
            ast::Filter::Comparison(comparison) => filter::Filter::SequenceToSequence(Box::new(
                filter::ComparisonFilter::new(self, comparison)?,
            )),
        })
    }

    /// Generate an `ArgMissing` error corresponding to a missing argument of this field call.
    ///
    /// # Parameters
    /// - `name`: The name of the missing argument.
    pub fn arg_missing_error(&self, name: &str) -> Box<Error> {
        Box::new(Error::ArgMissing {
            span: self.ast.span,
            type_name: self.type_name().to_owned(),
            field_name: self.field_name().to_owned(),
            param_name: name.to_owned(),
        })
    }

    /// Create an `ArgTypeMismatch` error corresponding to an argument of this field call.
    ///
    /// # Parameters
    /// - `name`: the name of the argument.
    /// - `got`: the AST node of value that has an invalid type.
    pub fn arg_type_mismatch_error(&self, name: &str, got: &ast::Literal) -> Box<Error> {
        Box::new(Error::ArgTypeMismatch {
            span: got.span,
            type_name: self.type_name().to_owned(),
            field_name: self.field_name().to_owned(),
            param_name: name.to_owned(),
            expecting: self
                .schema
                .param_by_name(name)
                .unwrap()
                .ty()
                .name()
                .to_owned(),
            got: got.value.kind_name().to_owned(),
        })
    }

    /// Create a `System` error for an error in the system layer corresponding to this field call.
    ///
    /// # Parameters
    /// - `source`: the error returned by the system layer.
    pub fn field_call_error(&self, source: anyhow::Error) -> Box<Error> {
        Box::new(Error::System {
            span: self.ast.ident.span,
            type_name: self.type_name().to_owned(),
            field_name: self.field_name().to_owned(),
            source,
        })
    }
}

/// A tree with information for each field call.
///
/// An SQ query can be represented as a tree of field calls. For example, the query
/// `path("/").children(same_filesystem=true) {stem extension children[:10] { stem extension } int(7)`
/// is represented by the tree
/// ```plaintext
///                      root
///                ┌──────┴─────┐
///             path("/")     int(7)
///                │
///   children(same_filesystem=true)
///     ┌──────┼──────┐
///   stem extension children[:10]
///             ┌──────┴─────┐
///            stem       extension
/// ```
/// A `FieldCallInfoTree` represents a node in such a tree, and contains information about a field
/// call and a collection of child nodes.
///
/// # Lifetime Parameters
/// - `'a`: The lifetime of the underlying AST data passed to the struct (by reference) in the
/// `new()` associated function.
#[derive(Debug)]
pub struct FieldCallInfoTree<'a> {
    pub info: FieldCallInfo<'a>,
    pub children: Vec<FieldCallInfoTree<'a>>,
}

impl<'a> FieldCallInfoTree<'a> {
    /// Create a `FieldCallInfoTree` from an `ast::Query` AST node.
    ///
    /// This function will create the whole tree, not just a single node.
    pub fn new(query: &'a ast::Query) -> Result<Self> {
        let root_field_schema = schema::root_field();
        Ok(Self {
            info: FieldCallInfo::new(&query.dummy_field_call, root_field_schema),
            children: query
                .field_trees
                .iter()
                .map(|ft| {
                    Self::new_subtree(
                        ft.dot_expression.field_calls.as_slice(),
                        ft.opt_brace_expression.as_ref(),
                        root_field_schema,
                    )
                })
                .collect::<Result<Vec<FieldCallInfoTree<'a>>>>()?,
        })
    }

    /// Create a `FieldCallInfoTree` from an `ast::DotExpression` AST node.
    ///
    /// This function will create the whole tree, not just a single node.
    pub fn from_dot_expression(
        dot_expression: &'a ast::DotExpression,
        parent_field_schema: &'static FieldSchema,
    ) -> Result<Self> {
        Self::new_subtree(
            dot_expression.field_calls.as_slice(),
            None,
            parent_field_schema,
        )
    }

    /// Create a new subtree.
    fn new_subtree(
        field_calls: &'a [ast::FieldCall],
        opt_brace_expression: Option<&'a ast::BraceExpression>,
        parent_field_schema: &'static FieldSchema,
    ) -> Result<Self> {
        assert!(!field_calls.is_empty());

        let field_call = &field_calls[0];
        let type_schema = parent_field_schema.return_type();
        let field_schema = type_schema
            .get_field(&field_call.ident.name)
            .ok_or_else(|| Error::InvalidField {
                span: field_call.ident.span,
                type_name: type_schema.name().to_owned(),
                field_name: field_call.ident.name.to_owned(),
            })?;

        let children = if field_calls.len() > 1 {
            vec![Self::new_subtree(
                &field_calls[1..],
                opt_brace_expression,
                field_schema,
            )?]
        } else {
            match opt_brace_expression {
                Some(be) => be
                    .field_trees
                    .iter()
                    .map(|ft| {
                        Self::new_subtree(
                            ft.dot_expression.field_calls.as_slice(),
                            ft.opt_brace_expression.as_ref(),
                            field_schema,
                        )
                    })
                    .collect::<Result<Vec<FieldCallInfoTree<'a>>>>()?,
                None => vec![],
            }
        };

        Ok(Self {
            info: FieldCallInfo::new(field_call, field_schema),
            children,
        })
    }
}
