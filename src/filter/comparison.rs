// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

//! Items relating to comparison filters in the query.

use crate::ast;
use crate::error::{Error, OptResult, Result};
use crate::fieldcall::FieldCallInfoTree;
use crate::fieldcall::{FieldCallInfo, FieldSequenceType};
use crate::filter::SequenceToSequenceFilter;
use crate::primitive::Primitive;
use crate::sqvalue::{SqBValue, SqBValueSequence};
use crate::util::return_none_or_err;

/// Filter corresponding to a comparison filter in the query. E.g. `path.children[extension="rs"]`.
#[derive(Debug)]
#[must_use]
pub struct ComparisonFilter<'a> {
    call_info: &'a FieldCallInfo<'a>,
    comparison_ast: &'a ast::Comparison,
    opt_dot_expr_tree: Option<FieldCallInfoTree<'a>>,
}

type Comparator<'a> = Box<dyn Fn(&SqBValue) -> Result<bool> + 'a>;

impl<'a> ComparisonFilter<'a> {
    /// Create a new filter corresponding to a comparison filter in the query.
    ///
    /// # Parameters:
    /// - `call_info`: info about the field call that contains the filter.
    /// - `comparison_ast`: AST node of the comparison filter.
    pub fn new(call_info: &'a FieldCallInfo, comparison_ast: &'a ast::Comparison) -> Result<Self> {
        let opt_dot_expr_tree = match &comparison_ast.opt_dot_expression {
            Some(de) => Some(FieldCallInfoTree::from_dot_expression(
                de,
                call_info.schema(),
            )?),
            None => None,
        };

        let operand_type = match &opt_dot_expr_tree {
            None => call_info.return_type().primitive_coercion(),
            Some(dot_expr_tree) => {
                let mut last_descendant = dot_expr_tree;
                while !last_descendant.children.is_empty() {
                    assert_eq!(last_descendant.children.len(), 1);
                    last_descendant = &last_descendant.children[0];
                }
                last_descendant.info.return_type().primitive_coercion()
            }
        };

        if operand_type != comparison_ast.literal.value.kind() {
            let lhs_span = if let Some(de) = &comparison_ast.opt_dot_expression {
                de.span
            } else {
                call_info.ast().ident.span
            };
            return Err(Box::new(Error::ComparisonTypeMismatch {
                span: call_info.ast().span,
                type_name: call_info.type_name().to_owned(),
                field_name: call_info.field_name().to_owned(),
                lhs_span,
                lhs: operand_type,
                rhs_span: comparison_ast.literal.span,
                rhs: comparison_ast.literal.value.kind(),
            }));
        }

        Ok(Self {
            call_info,
            comparison_ast,
            opt_dot_expr_tree,
        })
    }

    fn evaluate_dot_expression(
        parent: &SqBValue,
        call_info_tree: &FieldCallInfoTree,
    ) -> OptResult<Primitive> {
        // TODO: the field calls we do here might also be done in the normal (non-comparison) call
        // tree. Consider caching the results somewhere.
        let info = &call_info_tree.info;
        let child = match info.return_sequence_type() {
            FieldSequenceType::Single => parent.get_single(&call_info_tree.info)?,
            FieldSequenceType::Option => {
                return_none_or_err!(parent.get_option(&call_info_tree.info))
            }
            FieldSequenceType::Sequence => {
                return Err(Box::new(Error::NonSingleFieldInComparison {
                    span: info.ast().ident.span,
                    type_name: info.type_name().to_owned(),
                    field_name: info.field_name().to_owned(),
                    sequence_type: info.return_sequence_type(),
                }))
            }
        };
        if call_info_tree.children.is_empty() {
            child.get_primitive(info).map(Some)
        } else {
            assert!(call_info_tree.children.len() == 1);
            Self::evaluate_dot_expression(&child, &call_info_tree.children[0])
        }
    }

    fn left_operand(&self, value: &SqBValue) -> OptResult<Primitive> {
        match &self.opt_dot_expr_tree {
            Some(dot_expr_tree) => Self::evaluate_dot_expression(value, dot_expr_tree),
            None => value.get_primitive(self.call_info).map(Some),
        }
    }

    fn get_comparator(&'a self) -> Comparator<'a> {
        use ast::ComparisonOperatorKind::*;
        match &self.comparison_ast.op.kind {
            Less => Box::new(|v| {
                Ok(self.left_operand(v)?.as_ref() < Some(&self.comparison_ast.literal.value))
            }),
            LessOrEqual => Box::new(|v| {
                Ok(self.left_operand(v)?.as_ref() <= Some(&self.comparison_ast.literal.value))
            }),
            Greater => Box::new(|v| {
                Ok(self.left_operand(v)?.as_ref() > Some(&self.comparison_ast.literal.value))
            }),
            GreaterOrEqual => Box::new(|v| {
                Ok(self.left_operand(v)?.as_ref() >= Some(&self.comparison_ast.literal.value))
            }),
            Equal => Box::new(|v| {
                Ok(self.left_operand(v)?.as_ref() == Some(&self.comparison_ast.literal.value))
            }),
            NotEqual => Box::new(|v| {
                Ok(self.left_operand(v)?.as_ref() != Some(&self.comparison_ast.literal.value))
            }),
        }
    }
}

impl<'a> SequenceToSequenceFilter for ComparisonFilter<'a> {
    fn filter<'s>(&'s self, seq: SqBValueSequence<'s>) -> Result<SqBValueSequence<'s>> {
        let comparator = self.get_comparator();
        Ok(SqBValueSequence::Iterator(Box::new(seq.filter_map(
            move |result| {
                let value = match result {
                    Ok(v) => v,
                    Err(e) => return Some(Err(e)),
                };
                match comparator(&value) {
                    Err(e) => Some(Err(e)),
                    Ok(false) => None,
                    Ok(true) => Some(Ok(value)),
                }
            },
        ))))
    }
}
