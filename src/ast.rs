// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

//! Structures defining the AST generated during query parsing.
//!
//! Each struct has a `span: SourceSpan` member pointing to the section of the query that the AST
//! node represents.

use miette::SourceSpan;

use crate::fieldcall::FieldAccessKind;
use crate::primitive::Primitive;

/// AST node representing the `field_access` grammar item.
///
/// I.e. the optional pullup operator (`<`) preceding a field name.
#[must_use]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct FieldAccess {
    pub span: SourceSpan,
    pub kind: FieldAccessKind,
}

/// AST node representing the `literal` grammar item: a primitive SQ value literal.
///
/// E.g. `-23`, `2.5`, `true`, `"a string"`.
#[must_use]
#[derive(Clone, Debug, PartialEq)]
pub struct Literal {
    pub span: SourceSpan,
    pub value: Primitive,
}

/// AST node representing the `ident` grammar item: an identifier.
///
/// E.g. a field name or argument name.
#[must_use]
#[derive(Clone, Debug, PartialEq)]
pub struct Ident {
    pub span: SourceSpan,
    pub name: String,
}

/// AST node representing the `named_arg` grammar item: a named argument to a field call.
///
/// E.g. `param1 = "a value"`
#[must_use]
#[derive(Clone, Debug, PartialEq)]
pub struct NamedArg {
    pub span: SourceSpan,
    pub ident: Ident,
    pub literal: Literal,
}

/// A collection of `pos_arg` grammar items in an `arg_pack`.
///
/// Positional arguments are represented simply by a `Literal` AST node representing the argument's
/// value.
pub type PosArgs = Vec<Literal>;

/// A collection of `named_arg` grammar items in an `arg_pack`.
pub type NamedArgs = Vec<NamedArg>;

/// AST node representing the `arg_pack` grammar item: a pack of arguments for a field call.
///
/// E.g. `()`, `(true, 2.5, arg_x = "a value")`
#[must_use]
#[derive(Clone, Debug, PartialEq)]
pub struct ArgPack {
    pub span: SourceSpan,
    pub pos_args: PosArgs,
    pub named_args: NamedArgs,
}

/// AST node representing the `filter_pack` grammar item: a filter specification for a field call.
///
/// E.g. `[]`, `1[10]`, `[1::-1]` or `[exists=true]`.
#[must_use]
#[derive(Clone, Debug, PartialEq)]
pub struct FilterPack {
    pub span: SourceSpan,
    pub opt_filter: Option<Filter>,
}

/// AST node representing the contents of a `filter_pack`: `index | slice | comparison`
///
/// E.g. `10`, `1::-1` or `exists=true`.
#[must_use]
#[derive(Clone, Debug, PartialEq)]
pub enum Filter {
    Index(IntLiteral),
    Slice(Slice),
    Comparison(Comparison),
}

/// AST node representing an `int_literal` grammar item: an integer literal.
#[must_use]
#[derive(Clone, Debug, PartialEq)]
pub struct IntLiteral {
    pub span: SourceSpan,
    pub value: i128,
}

/// AST node representing a `slice` grammar item in a `filter_pack`: a Python style slice.
///
/// E.g. `1:-1:2`.
#[must_use]
#[derive(Clone, Debug, PartialEq)]
pub struct Slice {
    pub span: SourceSpan,
    pub opt_start: Option<IntLiteral>,
    pub opt_stop: Option<IntLiteral>,
    pub opt_step: Option<IntLiteral>,
}

/// AST node representing a `comparison` grammar item in a `filter_pack`.
///
/// E.g. `x<=10` or `extension = ".rs"`.
#[must_use]
#[derive(Clone, Debug, PartialEq)]
pub struct Comparison {
    pub span: SourceSpan,
    pub opt_dot_expression: Option<DotExpression>,
    pub op: ComparisonOperator,
    pub literal: Literal,
}

/// AST node representing a `comparison_operator` grammar item in a `comparison`.
///
/// I.e. `>`, `>=`, `<`, `<=` or `=`.
#[must_use]
#[derive(Clone, Debug, PartialEq)]
pub struct ComparisonOperator {
    pub span: SourceSpan,
    pub kind: ComparisonOperatorKind,
}

/// An enumeration of the names of the different comparison operators.
#[must_use]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ComparisonOperatorKind {
    Greater,
    GreaterOrEqual,
    Less,
    LessOrEqual,
    Equal,
    NotEqual,
}

/// AST node representing the `field_call` grammar item.
///
/// E.g. `<path`, `children(recurse=true)[0]`, `int(99)`.
#[must_use]
#[derive(Clone, Debug, PartialEq)]
pub struct FieldCall {
    pub span: SourceSpan,
    pub ident: Ident,
    pub opt_field_access: Option<FieldAccess>,
    pub opt_arg_pack: Option<ArgPack>,
    pub opt_filter_pack: Option<FilterPack>,
}

/// AST node representing the `dot_expression` grammar item.
///
/// E.g. `path.<children`, `path.children(recurse=true).extension`
#[must_use]
#[derive(Clone, Debug, PartialEq)]
pub struct DotExpression {
    pub span: SourceSpan,
    pub field_calls: Vec<FieldCall>,
}

/// AST node representing the `field_tree` grammar item.
///
/// E.g. `path.<children`, `path {extension filename}`, `path.children {extension}`
#[must_use]
#[derive(Clone, Debug, PartialEq)]
pub struct FieldTree {
    pub span: SourceSpan,
    pub dot_expression: DotExpression,
    pub opt_brace_expression: Option<BraceExpression>,
}

/// AST node representing the `brace_expression` grammar item.
///
/// E.g. `{extension}`, `{extension children(recurse=true)}`.
#[must_use]
#[derive(Clone, Debug, PartialEq)]
pub struct BraceExpression {
    pub span: SourceSpan,
    pub field_trees: Vec<FieldTree>,
}

/// AST node representing the `query` grammar item: a full SQ query.
///
/// E.g. `path {extension stem} int(17)`
#[must_use]
#[derive(Clone, Debug, PartialEq)]
pub struct Query {
    pub span: SourceSpan,
    pub field_trees: Vec<FieldTree>,
    pub dummy_field_call: FieldCall,
}
