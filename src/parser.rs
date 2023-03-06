//! Types and methods for parsing a stream of tokens into an AST.
use std::string::String;

use miette::SourceSpan;
use serde_json;

use crate::ast;
use crate::error::{Error, OptResult, Result};
use crate::fieldcall::FieldAccessKind;
use crate::lexer::{Token, TokenKind};
use crate::primitive::Primitive::{Bool, Float, Int, Str};
use crate::util::return_none_or_err;

/// An SQ query parser.
#[must_use]
pub struct Parser<'q> {
    query: &'q str,
    tokens: &'q [Token],
    offset: usize,
    expecting: Vec<TokenKind>,
}

impl<'q> Parser<'q> {
    /// Create a new `Parser`.
    ///
    /// # Parameters
    /// - `query`: the input SQ Query.
    /// - `tokens`: the tokens that make up the query (produced by the `lexer` module).
    pub fn new(query: &'q str, tokens: &'q [Token]) -> Self {
        Self {
            query,
            tokens,
            offset: 0,
            expecting: Vec::<TokenKind>::new(),
        }
    }

    /// Parse the query into an `ast::Query` AST node.
    pub fn parse(&mut self) -> Result<ast::Query> {
        match self.parse_query()? {
            Some(q) => Ok(q),
            None => Err(self.unexpected_token_error()),
        }
    }

    /// GRAMMAR: query: field_tree_list Eof
    ///
    /// # Examples
    /// ```plaintext
    /// field0.field01 field1(a1="abc").field10[4] { field100 field101(9) } field2
    /// ╰─────────────────────────────────┬──────────────────────────────────────╯│
    ///                             field_tree_list                              Eof
    /// ```
    fn parse_query(&mut self) -> OptResult<ast::Query> {
        let field_trees = return_none_or_err!(self.parse_field_tree_list());
        let _ = self.expect_token(TokenKind::Eof)?;
        let span = Self::merge_spans(
            field_trees.first().unwrap().span,
            field_trees.last().unwrap().span,
        );
        let dummy_field_call = ast::FieldCall {
            span: (0, 0).into(),
            ident: ast::Ident {
                span: (0, 0).into(),
                name: "root".to_owned(),
            },
            opt_field_access: None,
            opt_arg_pack: None,
            opt_filter_pack: None,
        };
        Ok(Some(ast::Query {
            span,
            field_trees,
            dummy_field_call,
        }))
    }
    /// GRAMMAR: field_tree_list: field_tree field_tree*
    ///
    /// # Examples
    /// ```plaintext
    /// field0.field01 field1(a1="abc").field10[4] { field100 field101(9) } field2
    /// ╰─────┬──────╯ ╰───────────────────┬──────────────────────────────╯ ╰──┬─╯
    ///  field_tree                   field_tree                           field_tree
    /// ```
    fn parse_field_tree_list(&mut self) -> OptResult<Vec<ast::FieldTree>> {
        let mut field_trees = vec![return_none_or_err!(self.parse_field_tree())];

        while let Some(field_tree) = self.parse_field_tree()? {
            field_trees.push(field_tree);
        }

        Ok(Some(field_trees))
    }

    /// GRAMMAR: field_tree: dot_expression brace_expression?
    ///
    /// # Examples
    /// ```plaintext
    /// field1(a1="abc").field10[4]
    /// ╰───────────┬─────────────╯
    ///       dot_expression
    /// ```
    ///
    /// ```plaintext
    /// field1(a1="abc").field10[4] { field100 field101(9) }
    /// ╰───────────┬─────────────╯ ╰───────────┬──────────╯
    ///       dot_expression             brace_expression
    /// ```
    fn parse_field_tree(&mut self) -> OptResult<ast::FieldTree> {
        let dot_expression = return_none_or_err!(self.parse_dot_expression());
        let opt_brace_expression = self.parse_brace_expression()?;
        let span = match &opt_brace_expression {
            Some(brace_expression) => Self::merge_spans(dot_expression.span, brace_expression.span),
            None => dot_expression.span,
        };
        Ok(Some(ast::FieldTree {
            span,
            dot_expression,
            opt_brace_expression,
        }))
    }

    /// GRAMMAR: brace_expression: LBrace field_tree_list RBrace
    ///
    /// # Examples
    /// ```plaintext
    /// { field0.field00 {field000} field1(9).field10 }
    /// │ ╰───────────────────┬─────────────────────╯ │
    /// LBrace         field_tree_list              RBrace
    /// ```
    fn parse_brace_expression(&mut self) -> OptResult<ast::BraceExpression> {
        let lbrace = return_none_or_err!(self.accept_token(TokenKind::LBrace));
        let field_trees = self
            .parse_field_tree_list()?
            .ok_or_else(|| self.unexpected_token_error())?;
        let rbrace = self.expect_token(TokenKind::RBrace)?;
        let span = Self::merge_spans(lbrace.span, rbrace.span);

        Ok(Some(ast::BraceExpression { span, field_trees }))
    }

    /// GRAMMAR: dot_expression: field_call (Dot field_call)*
    ///
    /// # Examples
    /// ```plaintext
    /// field0(7, x=true)
    /// ╰───────┬───────╯
    ///    field_call
    /// ```
    ///
    /// ```plaintext
    /// field0(7, x=true) . field1[1:-2:2] . field2
    /// ╰───────┬───────╯ │ ╰──────┬─────╯ │ ╰──┬─╯
    ///
    ///    field_call     │  field_call    │  field_call
    ///                   Dot              Dot
    /// ```
    fn parse_dot_expression(&mut self) -> OptResult<ast::DotExpression> {
        self.parse_dot_expression_impl(Self::parse_field_call)
    }

    /// GRAMMAR: dot_expression_in_comparison:
    ///     field_call_in_comparison (Dot field_call_in_comparison)*
    ///
    /// A `dot_expression_in_comparison` is like a `dot_expression`, except its field calls cannot
    /// contain a `field_access` prefix or a `filter_pack`.
    ///
    /// # Examples
    /// ```plaintext
    /// field0(7, x=true)
    /// ╰───────┬───────╯
    ///    field_call
    /// ```
    ///
    /// ```plaintext
    /// field0(7, x=true) . field1 . field2
    /// ╰───────┬───────╯ │ ╰─┬──╯ │ ╰──┬─╯
    ///    field_call    Dot  │   Dot   field_call
    ///                     field_call
    /// ```
    fn parse_dot_expression_in_comparison(&mut self) -> OptResult<ast::DotExpression> {
        self.parse_dot_expression_impl(Self::parse_field_call_in_comparison)
    }

    fn parse_dot_expression_impl(&mut self, field_call_parser: fn(&mut Self) -> OptResult<ast::FieldCall>) -> OptResult<ast::DotExpression> {
        let mut field_calls = vec![return_none_or_err!(field_call_parser(self))];

        while self.accept_token(TokenKind::Dot)?.is_some() {
            field_calls.push(
                field_call_parser(self)?
                    .ok_or_else(|| self.unexpected_token_error())?,
            );
        }

        let span = Self::merge_spans(
            field_calls.first().unwrap().span,
            field_calls.last().unwrap().span,
        );

        Ok(Some(ast::DotExpression { span, field_calls }))
    }

    /// GRAMMAR: field_call: field_access? ident arg_pack? filter_pack?
    ///
    /// # Examples
    /// ```plaintext
    /// <field0(7, x=true)[x=17]
    /// │╰─┬──╯╰────┬────╯╰─┬──╯
    /// │ ident  arg_pack filter_pack
    /// field_access
    /// ```
    ///
    /// ```plaintext
    /// field0(7, x=true)
    /// ╰─┬──╯╰────┬────╯
    ///  ident  arg_pack
    /// ```
    ///
    /// ```plaintext
    /// field0[1:-2:-1]
    /// ╰─┬──╯╰───┬───╯
    /// ident filter_pack
    ///  ```
    fn parse_field_call(&mut self) -> OptResult<ast::FieldCall> {
        self.parse_field_call_impl(false)
    }

    /// GRAMMAR: field_call_in_comparison: ident arg_pack?
    ///
    /// # Examples
    /// ```plaintext
    /// field0(7, x=true)
    /// ╰─┬──╯╰────┬────╯
    ///  ident  arg_pack
    ///  ```
    fn parse_field_call_in_comparison(&mut self) -> OptResult<ast::FieldCall> {
        self.parse_field_call_impl(true)
    }

    fn parse_field_call_impl(&mut self, in_comparison: bool) -> OptResult<ast::FieldCall> {
        let opt_field_access = if in_comparison {
            None
        } else {
            self.parse_field_access()?
        };
        let opt_ident = self.parse_ident()?;

        let ident = match (opt_field_access, opt_ident) {
            (_, Some(ident)) => ident,
            (Some(_), None) => return Err(self.unexpected_token_error()),
            (None, None) => return Ok(None),
        };

        let opt_arg_pack = self.parse_arg_pack()?;
        let opt_filter_pack = if in_comparison {
            None
        } else {
            self.parse_filter_pack()?
        };

        let span = match (&opt_field_access, &opt_arg_pack, &opt_filter_pack) {
            (Some(field_access), None, None) => Self::merge_spans(field_access.span, ident.span),
            (Some(field_access), Some(arg_pack), None) => {
                Self::merge_spans(field_access.span, arg_pack.span)
            }
            (Some(field_access), _, Some(filter_pack)) => {
                Self::merge_spans(field_access.span, filter_pack.span)
            }
            (None, Some(arg_pack), None) => Self::merge_spans(ident.span, arg_pack.span),
            (None, _, Some(filter_pack)) => Self::merge_spans(ident.span, filter_pack.span),
            (None, None, None) => ident.span,
        };

        Ok(Some(ast::FieldCall {
            span,
            ident,
            opt_field_access,
            opt_arg_pack,
            opt_filter_pack,
        }))
    }

    /// GRAMMAR: field_access: Less?
    ///
    /// # Examples
    /// ```plaintext
    /// <
    /// │
    /// Less
    /// ```
    /// ```plaintext
    /// ```
    fn parse_field_access(&mut self) -> OptResult<ast::FieldAccess> {
        let tok = return_none_or_err!(self.accept_token(TokenKind::Less));
        Ok(Some(ast::FieldAccess {
            span: tok.span,
            kind: FieldAccessKind::Pullup,
        }))
    }

    /// GRAMMAR:
    /// - arg_pack:
    ///       LParen (
    ///           arg_list_with_pos_args |
    ///           arg_list_without_pos_args
    ///       )? RParen
    ///
    /// - arg_list_with_pos_args:
    ///       pos_arg
    ///       (Comma pos_arg)*
    ///       (Comma named_arg)*
    ///
    /// - arg_list_without_pos_args:
    ///       named_arg
    ///       (Comma named_arg)*
    ///
    /// # Examples
    /// ## `arg_pack`
    /// ```plaintext
    /// ( 1, false, 8.2, "a string", x=1, y="abc" )
    /// │ ╰───────────────────┬─────────────────╯ │
    /// LParen      arg_list_with_pos_args       RParen
    /// ```
    ///
    /// ```plaintext
    /// ( w=false, x=1, y="abc", z="a, string" )
    /// │ ╰───────────────────┬──────────────╯ │
    /// LParen    arg_list_without_pos_args   RParen
    /// ```
    ///
    /// ## `arg_list_with_pos_args`
    /// ```plaintext
    /// 1, false, 8.2, "a string", x=1, y="abc"
    /// ││ ╰─┬─╯│ ╰┬╯│ ╰────┬───╯│ ╰┬╯│ ╰──┬──╯
    /// ││   │  │  │ │   pos_arg │  │ │  named_arg
    /// ││   │  │  │ Comma    Comma │ Comma
    /// ││   │  │ pos_arg    named_arg
    /// ││   │ Comma
    /// ││ pos_arg
    /// │Comma
    /// pos_arg
    /// ```
    ///
    /// ## `arg_list_without_pos_args`
    /// ```plaintext
    /// w=false, x=1, y="abc", z="a string"
    /// ╰──┬──╯│ ╰┬╯│ ╰──┬──╯│ ╰─────┬────╯
    ///    │   │  │ Comma│ Comma  named_arg
    ///    │   │  │    named_arg
    ///    │   │ named_arg
    ///    │  Comma
    /// named_arg
    /// ```
    fn parse_arg_pack(&mut self) -> OptResult<ast::ArgPack> {
        // The code doesn't follow the same shape as the grammar (as written above) here because
        // the grammar gets a bit messy but it's simple to say in code or English: A list of args,
        // separated by commas, where args can be either positional or named, and all of the
        // positional args must come before all of the named args.
        let lparen = return_none_or_err!(self.accept_token(TokenKind::LParen));
        let mut pos_args = ast::PosArgs::new();
        let mut named_args = ast::NamedArgs::new();

        while let Some(()) = self.parse_arg(&mut pos_args, &mut named_args)? {}

        let rparen = self.expect_token(TokenKind::RParen)?;

        Ok(Some(ast::ArgPack {
            span: Self::merge_spans(lparen.span, rparen.span),
            pos_args,
            named_args,
        }))
    }

    /// Doesn't strictly represent a symbol in the grammar because it's easier to code this way.
    ///
    /// Parses the next arg in a arg list, whether it's the first arg or not, and whether it's
    /// positional or named.
    fn parse_arg(
        &mut self,
        pos_args: &mut ast::PosArgs,
        named_args: &mut ast::NamedArgs,
    ) -> OptResult<()> {
        // Require a Comma unless it's the first arg
        if pos_args.len() + named_args.len() > 0 {
            let _ = return_none_or_err!(self.accept_token(TokenKind::Comma));
        }

        // Only parse a pos arg if we haven't seen any named ones yet.
        if named_args.is_empty() {
            if let Some(lit) = self.parse_pos_arg()? {
                pos_args.push(lit);
                return Ok(Some(()));
            }
        }

        if let Some(named_arg) = self.parse_named_arg()? {
            if let Some(prev) = named_args
                .iter()
                .find(|na| na.ident.name == named_arg.ident.name)
            {
                return Err(Box::new(Error::RepeatedNamedArg {
                    span: named_arg.span,
                    name: named_arg.ident.name.clone(),
                    prev_span: prev.span,
                }));
            }
            named_args.push(named_arg);
            return Ok(Some(()));
        }

        if pos_args.len() + named_args.len() > 0 {
            // We've seen a comma without an arg after it.
            return Err(self.unexpected_token_error());
        }

        Ok(None)
    }

    /// GRAMMAR: pos_arg: literal
    ///
    /// # Examples
    /// ```plaintext
    /// 1
    /// │
    /// literal
    /// ```
    /// ```plaintext
    /// "a string"
    /// ╰────┬───╯
    ///   literal
    /// ```
    fn parse_pos_arg(&mut self) -> OptResult<ast::Literal> {
        self.parse_literal()
    }

    /// GRAMMAR: named_arg: ident Equal literal
    ///
    /// # Examples
    /// ```plaintext
    /// recurse = false
    /// ╰──┬──╯ │ ╰─┬─╯
    ///  ident  │  literal
    ///       Equal
    /// ```
    fn parse_named_arg(&mut self) -> OptResult<ast::NamedArg> {
        let ident = return_none_or_err!(self.parse_ident());
        let _ = self.expect_token(TokenKind::Equal)?;
        match self.parse_literal()? {
            None => Err(self.unexpected_token_error()),
            Some(literal) => Ok(Some(ast::NamedArg {
                span: Self::merge_spans(ident.span, literal.span),
                ident,
                literal,
            })),
        }
    }

    /// GRAMMAR:
    /// - filter_pack: LBrac (list_index | slice | comparison) RBrac
    /// - list_index: int_literal
    /// - slice: int_literal? Colon int_literal? (Colon int_literal?)?
    ///
    /// # Examples
    /// ## `filter_pack`
    /// ```plaintext
    /// [extension = "rs"]
    /// │╰──────┬───────╯│
    /// │   comparison   │
    /// LBracket       RBracket
    /// ```
    ///
    /// ```plaintext
    /// [-10]
    /// │╰┬╯│
    /// │ │ RBracket
    /// │ list_index
    /// LBracket
    /// ```
    ///
    /// ```plaintext
    /// [10:-10:-1]
    /// │╰───┬────╯│
    /// │  slice  RBracket
    /// LBracket
    /// ```
    ///
    /// ## `list_index`
    /// ```plaintext
    /// -10
    /// ╰┬╯
    /// int_literal
    /// ```
    ///
    /// ## `slice`
    /// ```plaintext
    /// 1:-10:-10
    /// ││╰┬╯│╰┬╯
    /// ││ │ │ int_literal
    /// ││ │ Colon
    /// ││ int literal
    /// │Colon
    /// int_literal
    /// ```
    ///
    /// ```plaintext
    /// ::
    /// ││
    /// │Colon
    /// Colon
    /// ```
    fn parse_filter_pack(&mut self) -> OptResult<ast::FilterPack> {
        // NOTE: parse both list_index and slice directly in this function to avoid
        // backtracking - both can start with an integer literal.

        let lbrac = return_none_or_err!(self.accept_token(TokenKind::LBracket));
        let opt_start = self.parse_int_literal()?;

        // slice
        if let Some(colon) = self.accept_token(TokenKind::Colon)? {
            let opt_stop = self.parse_int_literal()?;
            let opt_colon2 = self.accept_token(TokenKind::Colon)?;
            let opt_step = match opt_colon2 {
                Some(_) => self.parse_int_literal()?,
                None => None,
            };
            let rbrac = self.expect_token(TokenKind::RBracket)?;

            let slice_span_begin = if let Some(start) = &opt_start {
                start.span
            } else {
                colon.span
            };
            let slice_span_end = match (&opt_stop, &opt_colon2, &opt_step) {
                (_, _, Some(step)) => step.span,
                (_, Some(colon2), None) => colon2.span,
                (Some(stop), None, None) => stop.span,
                (None, None, None) => colon.span,
            };
            let slice_span = Self::merge_spans(slice_span_begin, slice_span_end);

            return Ok(Some(ast::FilterPack {
                span: Self::merge_spans(lbrac.span, rbrac.span),
                opt_filter: Some(ast::Filter::Slice(ast::Slice {
                    span: slice_span,
                    opt_start,
                    opt_stop,
                    opt_step,
                })),
            }));
        }

        // list_index
        if let Some(start) = opt_start {
            let rbrac = self.expect_token(TokenKind::RBracket)?;
            return Ok(Some(ast::FilterPack {
                span: Self::merge_spans(lbrac.span, rbrac.span),
                opt_filter: Some(ast::Filter::Index(start)),
            }));
        }

        // comparison
        if let Some(comparison) = self.parse_comparison()? {
            let rbrac = self.expect_token(TokenKind::RBracket)?;
            return Ok(Some(ast::FilterPack {
                span: Self::merge_spans(lbrac.span, rbrac.span),
                opt_filter: Some(ast::Filter::Comparison(comparison)),
            }));
        }

        // No filter
        let rbrac = self.expect_token(TokenKind::RBracket)?;
        Ok(Some(ast::FilterPack {
            span: Self::merge_spans(lbrac.span, rbrac.span),
            opt_filter: None,
        }))
    }

    /// GRAMMAR: comparison: dot_expression? comparison_operator literal
    ///
    /// # Examples
    /// ```plaintext
    /// parent.extension = "rs"
    /// ╰───┬───╯        │ ╰─┬─╯
    /// dot_expression   │  literal
    ///   comparison_operator
    /// ```
    fn parse_comparison(&mut self) -> OptResult<ast::Comparison> {
        let opt_dot_expression = self.parse_dot_expression_in_comparison()?;
        let opt_op = self.parse_comparison_operator()?;

        let op = match (&opt_dot_expression, opt_op) {
            (Some(_), None) => return Err(self.unexpected_token_error()),
            (None, None) => return Ok(None),
            (_, Some(op)) => op,
        };

        let first_span = opt_dot_expression
            .as_ref()
            .map(|de| de.span)
            .unwrap_or(op.span);

        let literal = self
            .parse_literal()?
            .ok_or_else(|| self.unexpected_token_error())?;

        Ok(Some(ast::Comparison {
            span: Self::merge_spans(first_span, literal.span),
            opt_dot_expression,
            op,
            literal,
        }))
    }

    /// GRAMMAR: comparison_operator: (LessOrEqual | Less | GreaterOrEqual | Greater | Equal)
    ///
    /// # Examples
    /// ```plaintext
    /// <
    /// │
    /// Less
    /// ```
    fn parse_comparison_operator(&mut self) -> OptResult<ast::ComparisonOperator> {
        if let Some(tok) = self.accept_token(TokenKind::Greater)? {
            return Ok(Some(ast::ComparisonOperator {
                span: tok.span,
                kind: ast::ComparisonOperatorKind::Greater,
            }));
        }
        if let Some(tok) = self.accept_token(TokenKind::GreaterOrEqual)? {
            return Ok(Some(ast::ComparisonOperator {
                span: tok.span,
                kind: ast::ComparisonOperatorKind::GreaterOrEqual,
            }));
        }
        if let Some(tok) = self.accept_token(TokenKind::Less)? {
            return Ok(Some(ast::ComparisonOperator {
                span: tok.span,
                kind: ast::ComparisonOperatorKind::Less,
            }));
        }
        if let Some(tok) = self.accept_token(TokenKind::LessOrEqual)? {
            return Ok(Some(ast::ComparisonOperator {
                span: tok.span,
                kind: ast::ComparisonOperatorKind::LessOrEqual,
            }));
        }
        if let Some(tok) = self.accept_token(TokenKind::Equal)? {
            return Ok(Some(ast::ComparisonOperator {
                span: tok.span,
                kind: ast::ComparisonOperatorKind::Equal,
            }));
        }
        if let Some(tok) = self.accept_token(TokenKind::NotEqual)? {
            return Ok(Some(ast::ComparisonOperator {
                span: tok.span,
                kind: ast::ComparisonOperatorKind::NotEqual,
            }));
        }
        Ok(None)
    }

    /// GRAMMAR: int_literal: Int
    ///
    /// # Examples
    /// ```plaintext
    /// -103
    /// ╰┬─╯
    /// Int
    /// ```
    fn parse_int_literal(&mut self) -> OptResult<ast::IntLiteral> {
        let (span, value) = return_none_or_err!(self.parse_int());
        Ok(Some(ast::IntLiteral { span, value }))
    }

    /// GRAMMAR: literal: (Int | Float | Str | bool)
    ///
    /// # Examples
    /// ```plaintext
    /// -103
    /// ╰┬─╯
    /// Int
    /// ```
    ///
    /// ```plaintext
    /// 1.4E7
    /// ╰─┬─╯
    /// Float
    /// ```
    ///
    /// ```plaintext
    /// false
    /// ╰─┬─╯
    ///  bool
    /// ```
    /// ```plaintext
    /// "abc"
    /// ╰─┬─╯
    ///  Str
    /// ```
    fn parse_literal(&mut self) -> OptResult<ast::Literal> {
        if let Some((span, int_val)) = self.parse_int()? {
            return Ok(Some(ast::Literal {
                span,
                value: Int(int_val),
            }));
        }
        if let Some((span, float_val)) = self.parse_float()? {
            return Ok(Some(ast::Literal {
                span,
                value: Float(float_val),
            }));
        }
        if let Some((span, string_val)) = self.parse_str()? {
            return Ok(Some(ast::Literal {
                span,
                value: Str(string_val),
            }));
        }
        if let Some((span, bool_val)) = self.parse_bool()? {
            return Ok(Some(ast::Literal {
                span,
                value: Bool(bool_val),
            }));
        }
        Ok(None)
    }

    // Float
    fn parse_float(&mut self) -> OptResult<(SourceSpan, f64)> {
        let tok = return_none_or_err!(self.accept_token(TokenKind::Float));
        match tok.text(self.query).parse::<f64>() {
            Ok(f) if f.is_infinite() => Err(Box::new(Error::ParseValue {
                span: tok.span,
                desc: "number does not fit in 64-bit float data type".to_owned(),
            })),
            Ok(f) if f.is_nan() => panic!("Parsed float {} as NaN", tok.text(self.query)),

            Ok(f) => Ok(Some((tok.span, f))),
            Err(e) => panic!("failed to parse {} as float: {}", tok.text(self.query), e,),
        }
    }

    // Int
    fn parse_int(&mut self) -> OptResult<(SourceSpan, i64)> {
        let tok = return_none_or_err!(self.accept_token(TokenKind::Int));
        match tok.text(self.query).parse::<i64>() {
            Ok(i) => Ok(Some((tok.span, i))),
            Err(e) => Err(Box::new(Error::ParseValue {
                span: tok.span,
                desc: format!("failed to parse integer literal: {}", e),
            })),
        }
    }

    // Str
    fn parse_str(&mut self) -> OptResult<(SourceSpan, String)> {
        let tok = return_none_or_err!(self.accept_token(TokenKind::Str));
        match serde_json::from_str::<String>(tok.text(self.query)) {
            Ok(s) => Ok(Some((tok.span, s))),
            Err(e) => Err(Box::new(Error::ParseValue {
                span: tok.span,
                desc: format!("failed to parse string literal: {}", e),
            })),
        }
    }

    /// GRAMMAR: bool: (BoolTrue | BoolFalse)
    ///
    /// # Examples
    /// ```plaintext
    /// true
    /// ╰─┬─╯
    ///  BoolTrue
    /// ```
    ///
    /// ```plaintext
    /// false
    /// ╰─┬─╯
    ///  BoolFalse
    /// ```
    fn parse_bool(&mut self) -> OptResult<(SourceSpan, bool)> {
        let opt_true_tok = self.accept_token(TokenKind::True)?;
        if let Some(true_tok) = opt_true_tok {
            return Ok(Some((true_tok.span, true)));
        }
        let opt_false_tok = self.accept_token(TokenKind::False)?;
        if let Some(false_tok) = opt_false_tok {
            return Ok(Some((false_tok.span, false)));
        }
        Ok(None)
    }

    /// GRAMMAR: ident: Ident
    ///
    /// # Examples
    /// ```plaintext
    /// children
    /// ╰──┬───╯
    ///  Ident
    /// ```
    fn parse_ident(&mut self) -> OptResult<ast::Ident> {
        let tok = return_none_or_err!(self.accept_token(TokenKind::Ident));
        Ok(Some(ast::Ident {
            span: tok.span,
            name: tok.text(self.query).to_owned(),
        }))
    }

    /// Get the next token in the token stream and increment the cursor.
    ///
    /// # Return
    /// - The next token if it exists.
    /// - `None` if there are no more tokens.
    fn shift_token(&mut self) -> Option<Token> {
        self.expecting.clear();
        let opt_tok = self.tokens.get(self.offset).copied();
        self.offset += 1;
        opt_tok
    }

    /// Get a copy of the next token in the stream without changing the cursor position.
    ///
    /// # Return
    /// - The next token if it exists.
    /// - `None` if there are no more tokens.
    fn peek_token(&self) -> Option<Token> {
        self.tokens.get(self.offset).copied()
    }

    /// If the next token in the stream has the given kind, return it and increment the cursor.
    ///
    /// # Return
    /// - The next token if it exists and matches the given kind.
    /// - `None` if there are no more tokens or the next token is not of the given kind.
    fn accept_token(&mut self, kind: TokenKind) -> OptResult<Token> {
        match self.peek_token() {
            Some(token) if token.kind == kind => Ok(self.shift_token()),
            _ => {
                self.expecting.push(kind);
                Ok(None)
            }
        }
    }

    /// Like `accept_token()`, but returns an error when `accept_token()` would return `None`.
    fn expect_token(&mut self, kind: TokenKind) -> Result<Token> {
        match self.accept_token(kind)? {
            Some(x) => Ok(x),
            None => Err(self.unexpected_token_error()),
        }
    }

    /// Get a span that covers the `left_span` and `right_span` and everything inbetween.
    fn merge_spans(left_span: SourceSpan, right_span: SourceSpan) -> SourceSpan {
        (
            left_span.offset(),
            right_span.offset() - left_span.offset() + right_span.len(),
        )
            .into()
    }

    /// Create an `UnexpectedToken` error with information about the current token and the list of
    /// token kinds that would have been accepted.
    fn unexpected_token_error(&self) -> Box<Error> {
        let (span, kind) = match self.peek_token() {
            Some(t) => (t.span, t.kind),
            None => ((self.query.len(), 0).into(), TokenKind::Eof),
        };
        Box::new(Error::UnexpectedToken {
            span,
            kind,
            expecting: self.expecting.clone(),
        })
    }
}

#[cfg(test)]
mod tests {

    use pretty_assertions::assert_eq;

    use super::*;
    use crate::lexer::lex;

    fn simple_dot_expression(span: SourceSpan, ident_name: &str) -> ast::DotExpression {
        ast::DotExpression {
            span,
            field_calls: vec![ast::FieldCall {
                span,
                ident: ast::Ident {
                    span,
                    name: ident_name.to_owned(),
                },
                opt_field_access: None,
                opt_arg_pack: None,
                opt_filter_pack: None,
            }],
        }
    }

    macro_rules! gen_parse_test {
        ($parse_fn:ident, (ok, $name:ident, $query:expr, $result:expr)) => {
            #[test]
            fn $name() {
                let tokens = lex($query).unwrap();
                let mut parser = Parser::new($query, &tokens);
                assert_eq!(parser.$parse_fn().unwrap(), Some($result));
            }
        };
        (
            $parse_fn:ident,
            (none, $name:ident, $query:expr $(,$expecting:ident)*)
        ) => {
            #[test]
            fn $name() {
                let tokens = lex($query).unwrap();
                let mut parser = Parser::new($query, &tokens);
                assert_eq!(parser.$parse_fn().unwrap(), None);
                $(
                    assert!(
                        parser.expecting.contains(&TokenKind::$expecting),
                        "expecting {:?} to be in parser.expecting ({:?})",
                        TokenKind::$expecting,
                        parser.expecting,
                        );
                 )*
            }
        };
        (
            $parse_fn:ident,
            (err, $name:ident, $query:expr $(,$expecting:ident)*)
        ) => {
            #[test]
            fn $name() {
                let tokens = lex($query).unwrap();
                let mut parser = Parser::new($query, &tokens);
                let result = parser.$parse_fn();
                assert!(
                    result.is_err(),
                    "expecting error, got {:?}",
                    result);
                $(
                    assert!(
                        parser.expecting.contains(&TokenKind::$expecting),
                        "expecting {:?} to be in parser.expecting ({:?})",
                        TokenKind::$expecting,
                        parser.expecting,
                        );
                 )*
            }
        };
    }

    macro_rules! gen_parse_tests {
        ($parse_fn:ident, $($tail:tt,)+) => {
            $(
                gen_parse_test!($parse_fn, $tail);
             )+
        };
    }

    gen_parse_tests!(
        parse_field_access,
        (
            ok,
            field_access_pullup,
            "<",
            ast::FieldAccess {
                span: (0, 1).into(),
                kind: FieldAccessKind::Pullup,
            }
        ),
        (none, field_access_none, "asdf", Less),
        (none, field_access_none_eof, "", Less),
    );

    gen_parse_tests!(
        parse_field_call,
        (
            ok,
            field_call_normal,
            "jkl",
            ast::FieldCall {
                span: (0, 3).into(),
                ident: ast::Ident {
                    span: (0, 3).into(),
                    name: "jkl".to_owned(),
                },
                opt_field_access: None,
                opt_arg_pack: None,
                opt_filter_pack: None,
            }
        ),
        (
            ok,
            field_call_pullup,
            "<an_id",
            ast::FieldCall {
                span: (0, 6).into(),
                ident: ast::Ident {
                    span: (1, 5).into(),
                    name: "an_id".to_owned(),
                },
                opt_field_access: Some(ast::FieldAccess {
                    span: (0, 1).into(),
                    kind: FieldAccessKind::Pullup,
                }),
                opt_arg_pack: None,
                opt_filter_pack: None,
            }
        ),
        (
            ok,
            field_call_with_pos_params,
            "id(1.2, 5)",
            ast::FieldCall {
                span: (0, 10).into(),
                ident: ast::Ident {
                    span: (0, 2).into(),
                    name: "id".to_owned(),
                },
                opt_field_access: None,
                opt_arg_pack: Some(ast::ArgPack {
                    span: (2, 8).into(),
                    pos_args: vec![
                        ast::Literal {
                            span: (3, 3).into(),
                            value: Float(1.2),
                        },
                        ast::Literal {
                            span: (8, 1).into(),
                            value: Int(5),
                        }
                    ],
                    named_args: ast::NamedArgs::new(),
                }),
                opt_filter_pack: None,
            }
        ),
        (
            ok,
            field_call_with_named_params,
            "id(p1=true, p2=-5.8)",
            ast::FieldCall {
                span: (0, 20).into(),
                ident: ast::Ident {
                    span: (0, 2).into(),
                    name: "id".to_owned(),
                },
                opt_field_access: None,
                opt_arg_pack: Some(ast::ArgPack {
                    span: (2, 18).into(),
                    pos_args: ast::PosArgs::new(),
                    named_args: vec![
                        ast::NamedArg {
                            span: (3, 7).into(),
                            ident: ast::Ident {
                                span: (3, 2).into(),
                                name: "p1".to_owned(),
                            },
                            literal: ast::Literal {
                                span: (6, 4).into(),
                                value: Bool(true),
                            },
                        },
                        ast::NamedArg {
                            span: (12, 7).into(),
                            ident: ast::Ident {
                                span: (12, 2).into(),
                                name: "p2".to_owned(),
                            },
                            literal: ast::Literal {
                                span: (15, 4).into(),
                                value: Float(-5.8),
                            },
                        },
                    ],
                }),
                opt_filter_pack: None,
            }
        ),
        (
            ok,
            field_call_with_pos_and_named_params,
            "id(true, named=42)",
            ast::FieldCall {
                span: (0, 18).into(),
                ident: ast::Ident {
                    span: (0, 2).into(),
                    name: "id".to_owned(),
                },
                opt_field_access: None,
                opt_arg_pack: Some(ast::ArgPack {
                    span: (2, 16).into(),
                    pos_args: vec![ast::Literal {
                        span: (3, 4).into(),
                        value: Bool(true),
                    }],
                    named_args: vec![ast::NamedArg {
                        span: (9, 8).into(),
                        ident: ast::Ident {
                            span: (9, 5).into(),
                            name: "named".to_owned(),
                        },
                        literal: ast::Literal {
                            span: (15, 2).into(),
                            value: Int(42),
                        },
                    }],
                }),
                opt_filter_pack: None,
            }
        ),
        (
            ok,
            field_call_with_filter_pack,
            "jkl[17]",
            ast::FieldCall {
                span: (0, 7).into(),
                ident: ast::Ident {
                    span: (0, 3).into(),
                    name: "jkl".to_owned(),
                },
                opt_field_access: None,
                opt_arg_pack: None,
                opt_filter_pack: Some(ast::FilterPack {
                    span: (3, 4).into(),
                    opt_filter: Some(ast::Filter::Index(ast::IntLiteral {
                        span: (4, 2).into(),
                        value: 17
                    }))
                }),
            }
        ),
        (none, field_call_none, "7.1", Less, Ident),
        (none, field_call_none_eof, "", Less, Ident),
        (err, field_call_no_id, "<", Ident),
        (err, field_call_wrong_id_token_type, "<7.1", Ident),
        (err, field_call_pos_after_named_arg, "x(a=1, 10)", Ident),
    );

    gen_parse_tests!(
        parse_float,
        (ok, float_0p1, "0.1", ((0, 3).into(), 0.1)),
        (ok, float_m2p3, "-2.3", ((0, 4).into(), -2.3)),
        (ok, float_6p, "6.", ((0, 2).into(), 6.0)),
        (ok, float_m7p, "-7.", ((0, 3).into(), -7.0)),
        (ok, float_p9, ".9", ((0, 2).into(), 0.9)),
        (ok, float_mp0, "-.0", ((0, 3).into(), -0.0)),
        (ok, float_2345p6789, "2345.6789", ((0, 9).into(), 2345.6789)),
        (
            ok,
            float_m2345p6789,
            "-2345.6789",
            ((0, 10).into(), -2345.6789)
        ),
        (ok, float_1p234e56, "1.234e56", ((0, 8).into(), 1.234e56)),
        (ok, float_m1p234e56, "-1.234e56", ((0, 9).into(), -1.234e56)),
        (ok, float_1p234em56, "1.234e-56", ((0, 9).into(), 1.234e-56)),
        (
            ok,
            float_m1p234em56,
            "-1.234e-56",
            ((0, 10).into(), -1.234e-56)
        ),
        (ok, float_1p234ep56, "1.234e+56", ((0, 9).into(), 1.234e+56)),
        (
            ok,
            float_m1p234ep56,
            "-1.234e+56",
            ((0, 10).into(), -1.234e+56)
        ),
        (ok, float_1p234bige56, "1.234E56", ((0, 8).into(), 1.234E56)),
        (
            ok,
            float_m1p234bige56,
            "-1.234E56",
            ((0, 9).into(), -1.234E56)
        ),
        (none, float_1234, "1234", Float),
        (none, float_m1234, "-1234", Float),
        (none, float_a1p0, "a1.0", Float),
        (none, float_e, "e", Float),
        (none, float_eof, "", Float),
        (err, float_overflow, "9.9e999999999999"),
    );

    gen_parse_tests!(
        parse_int,
        (
            ok,
            int_1234567890,
            "1234567890",
            ((0, 10).into(), 1234567890)
        ),
        (
            ok,
            int_m1234567890,
            "-1234567890",
            ((0, 11).into(), -1234567890)
        ),
        (none, int_a10, "a10", Int),
        (none, int_10p1, "10.1", Int),
        (err, int_overflow, "99999999999999999999"),
    );

    gen_parse_tests!(
        parse_str,
        (
            ok,
            str_basic,
            r#""a string""#,
            ((0, 10).into(), "a string".to_owned())
        ),
        (
            ok,
            str_dquotes,
            r#""\"d\"q\"""#,
            ((0, 10).into(), r#""d"q""#.to_owned())
        ),
        (
            ok,
            str_backslash,
            r#""\\b\\s\\""#,
            ((0, 10).into(), r#"\b\s\"#.to_owned())
        ),
        (
            ok,
            str_fwdslash,
            r#""\/f\/s\/""#,
            ((0, 10).into(), r#"/f/s/"#.to_owned())
        ),
        (
            ok,
            str_backspace,
            r#""\bb\bs\b""#,
            ((0, 10).into(), "\x08b\x08s\x08".to_owned())
        ),
        (
            ok,
            str_ff,
            r#""\ff\ff\f""#,
            ((0, 10).into(), "\x0cf\x0cf\x0c".to_owned())
        ),
        (
            ok,
            str_nl,
            r#""\nn\nl\n""#,
            ((0, 10).into(), "\nn\nl\n".to_owned())
        ),
        (
            ok,
            str_cr,
            r#""\rc\rr\r""#,
            ((0, 10).into(), "\rc\rr\r".to_owned())
        ),
        (
            ok,
            str_tab,
            r#""\tt\ta\t""#,
            ((0, 10).into(), "\tt\ta\t".to_owned())
        ),
        (
            ok,
            str_squotes,
            r#""'s'q'""#,
            ((0, 7).into(), "'s'q'".to_owned())
        ),
        (
            ok,
            str_unicode_bmp_raw,
            r#""Ͱ""#,
            ((0, 4).into(), "Ͱ".to_owned())
        ),
        (
            ok,
            str_unicode_bmp_esc,
            r#""\u0370""#,
            ((0, 8).into(), "Ͱ".to_owned())
        ),
        (
            ok,
            str_unicode_smp_raw,
            r#""𐆒""#,
            ((0, 6).into(), "𐆒".to_owned())
        ),
        (
            ok,
            str_unicode_smp_esc,
            r#""\ud800\udd92""#,
            ((0, 14).into(), "𐆒".to_owned())
        ),
        (
            ok,
            str_unicode_sip_raw,
            r#""𠀀""#,
            ((0, 6).into(), "𠀀".to_owned())
        ),
        (
            ok,
            str_unicode_sip_esc,
            r#""\ud840\udc00""#,
            ((0, 14).into(), "𠀀".to_owned())
        ),
        (
            ok,
            str_unicode_tip_raw,
            r#""𰀃""#,
            ((0, 6).into(), "𰀃".to_owned())
        ),
        (
            ok,
            str_unicode_tip_esc,
            r#""\ud880\udc03""#,
            ((0, 14).into(), "𰀃".to_owned())
        ),
        (err, str_invalid_unicode_escape, r#""\uxxxx""#),
    );

    gen_parse_tests!(
        parse_bool,
        (ok, bool_true, "true", ((0, 4).into(), true)),
        (ok, bool_false, "false", ((0, 5).into(), false)),
        (none, bool_true_with_suffix, "true_x", True, False),
        (none, bool_false_with_suffix, "false0", True, False),
    );

    gen_parse_tests!(
        parse_literal,
        (
            ok,
            literal_int,
            "1234",
            ast::Literal {
                span: (0, 4).into(),
                value: Int(1234)
            }
        ),
        (
            ok,
            literal_float,
            "1.234",
            ast::Literal {
                span: (0, 5).into(),
                value: Float(1.234)
            }
        ),
        (
            ok,
            literal_str,
            r#""xyz""#,
            ast::Literal {
                span: (0, 5).into(),
                value: Str("xyz".to_owned())
            }
        ),
        (
            ok,
            literal_bool,
            "true",
            ast::Literal {
                span: (0, 4).into(),
                value: Bool(true)
            }
        ),
        (none, literal_none, "asdf", Int, Float, Str, False, True),
    );

    gen_parse_tests!(
        parse_named_arg,
        (
            ok,
            named_arg_int,
            "x=99",
            ast::NamedArg {
                span: (0, 4).into(),
                ident: ast::Ident {
                    span: (0, 1).into(),
                    name: "x".to_owned()
                },
                literal: ast::Literal {
                    span: (2, 2).into(),
                    value: Int(99)
                },
            }
        ),
        (
            ok,
            named_arg_float,
            "x=9.9",
            ast::NamedArg {
                span: (0, 5).into(),
                ident: ast::Ident {
                    span: (0, 1).into(),
                    name: "x".to_owned(),
                },
                literal: ast::Literal {
                    span: (2, 3).into(),
                    value: Float(9.9),
                },
            }
        ),
        (
            ok,
            named_arg_str,
            r#"x="abc""#,
            ast::NamedArg {
                span: (0, 7).into(),
                ident: ast::Ident {
                    span: (0, 1).into(),
                    name: "x".to_owned(),
                },
                literal: ast::Literal {
                    span: (2, 5).into(),
                    value: Str("abc".to_owned()),
                },
            }
        ),
        (
            ok,
            named_arg_bool,
            r#"x=true"#,
            ast::NamedArg {
                span: (0, 6).into(),
                ident: ast::Ident {
                    span: (0, 1).into(),
                    name: "x".to_owned(),
                },
                literal: ast::Literal {
                    span: (2, 4).into(),
                    value: Bool(true),
                },
            }
        ),
        (none, named_arg_none, ")", Ident),
        (err, named_arg_no_equals, "x 10", Equal),
        (err, named_arg_no_value, "x=)", Int, Float, Str, False, True),
    );

    gen_parse_tests!(
        parse_dot_expression,
        (
            ok,
            dot_expression_single,
            "x",
            ast::DotExpression {
                span: (0, 1).into(),
                field_calls: vec![ast::FieldCall {
                    span: (0, 1).into(),
                    ident: ast::Ident {
                        span: (0, 1).into(),
                        name: "x".to_owned(),
                    },
                    opt_field_access: None,
                    opt_arg_pack: None,
                    opt_filter_pack: None,
                }],
            }
        ),
        (
            ok,
            dot_expression_double,
            "x.y",
            ast::DotExpression {
                span: (0, 3).into(),
                field_calls: vec![
                    ast::FieldCall {
                        span: (0, 1).into(),
                        ident: ast::Ident {
                            span: (0, 1).into(),
                            name: "x".to_owned(),
                        },
                        opt_field_access: None,
                        opt_arg_pack: None,
                        opt_filter_pack: None,
                    },
                    ast::FieldCall {
                        span: (2, 1).into(),
                        ident: ast::Ident {
                            span: (2, 1).into(),
                            name: "y".to_owned(),
                        },
                        opt_field_access: None,
                        opt_arg_pack: None,
                        opt_filter_pack: None,
                    },
                ],
            }
        ),
        (none, dot_expression_none_eof, "", Ident),
        (none, dot_expression_none_no_ident, ".()", Ident),
        (err, dot_expression_no_ident_after_dot, "x.()", Ident),
    );

    gen_parse_tests!(
        parse_field_tree,
        (
            ok,
            field_tree_single_ident,
            "x",
            ast::FieldTree {
                span: (0, 1).into(),
                dot_expression: ast::DotExpression {
                    span: (0, 1).into(),
                    field_calls: vec![ast::FieldCall {
                        span: (0, 1).into(),
                        ident: ast::Ident {
                            span: (0, 1).into(),
                            name: "x".to_owned(),
                        },
                        opt_field_access: None,
                        opt_arg_pack: None,
                        opt_filter_pack: None,
                    }],
                },
                opt_brace_expression: None,
            }
        ),
        (
            ok,
            field_tree_with_brace_expression,
            "x { y }",
            ast::FieldTree {
                span: (0, 7).into(),
                dot_expression: ast::DotExpression {
                    span: (0, 1).into(),
                    field_calls: vec![ast::FieldCall {
                        span: (0, 1).into(),
                        ident: ast::Ident {
                            span: (0, 1).into(),
                            name: "x".to_owned(),
                        },
                        opt_field_access: None,
                        opt_arg_pack: None,
                        opt_filter_pack: None,
                    }],
                },
                opt_brace_expression: Some(ast::BraceExpression {
                    span: (2, 5).into(),
                    field_trees: vec![ast::FieldTree {
                        span: (4, 1).into(),
                        dot_expression: ast::DotExpression {
                            span: (4, 1).into(),
                            field_calls: vec![ast::FieldCall {
                                span: (4, 1).into(),
                                ident: ast::Ident {
                                    span: (4, 1).into(),
                                    name: "y".to_owned(),
                                },
                                opt_field_access: None,
                                opt_arg_pack: None,
                                opt_filter_pack: None,
                            }],
                        },
                        opt_brace_expression: None,
                    }],
                }),
            }
        ),
    );

    gen_parse_tests!(
        parse_int_literal,
        (
            ok,
            int_literal,
            "-78",
            ast::IntLiteral {
                span: (0, 3).into(),
                value: -78,
            }
        ),
        (none, int_literal_eof, "", Int),
        (none, int_literal_bool, "true", Int),
        (err, int_literal_overflow, "99999999999999999999"),
    );

    gen_parse_tests!(
        parse_comparison_operator,
        (
            ok,
            comparison_operator_gt,
            ">",
            ast::ComparisonOperator {
                span: (0, 1).into(),
                kind: ast::ComparisonOperatorKind::Greater
            }
        ),
        (
            ok,
            comparison_operator_gte,
            ">=",
            ast::ComparisonOperator {
                span: (0, 2).into(),
                kind: ast::ComparisonOperatorKind::GreaterOrEqual
            }
        ),
        (
            ok,
            comparison_operator_lt,
            "<",
            ast::ComparisonOperator {
                span: (0, 1).into(),
                kind: ast::ComparisonOperatorKind::Less
            }
        ),
        (
            ok,
            comparison_operator_lte,
            "<=",
            ast::ComparisonOperator {
                span: (0, 2).into(),
                kind: ast::ComparisonOperatorKind::LessOrEqual
            }
        ),
        (
            none,
            comparison_operator_eof,
            "",
            Greater,
            GreaterOrEqual,
            Less,
            LessOrEqual,
            Equal
        ),
    );

    gen_parse_tests!(
        parse_comparison,
        (
            ok,
            comparison_id_gt_string,
            r#"x > "a string""#,
            ast::Comparison {
                span: (0, 14).into(),
                opt_dot_expression: Some(simple_dot_expression((0, 1).into(), "x")),
                op: ast::ComparisonOperator {
                    span: (2, 1).into(),
                    kind: ast::ComparisonOperatorKind::Greater
                },
                literal: ast::Literal {
                    span: (4, 10).into(),
                    value: Str("a string".to_owned())
                },
            }
        ),
        (
            ok,
            comparison_gt_string,
            r#"> "a string""#,
            ast::Comparison {
                span: (0, 12).into(),
                opt_dot_expression: None,
                op: ast::ComparisonOperator {
                    span: (0, 1).into(),
                    kind: ast::ComparisonOperatorKind::Greater
                },
                literal: ast::Literal {
                    span: (2, 10).into(),
                    value: Str("a string".to_owned())
                },
            }
        ),
        (
            ok,
            comparison_id_gte_int,
            "an_id >= -7",
            ast::Comparison {
                span: (0, 11).into(),
                opt_dot_expression: Some(simple_dot_expression((0, 5).into(), "an_id")),
                op: ast::ComparisonOperator {
                    span: (6, 2).into(),
                    kind: ast::ComparisonOperatorKind::GreaterOrEqual
                },
                literal: ast::Literal {
                    span: (9, 2).into(),
                    value: Int(-7)
                },
            }
        ),
        (
            ok,
            comparison_gte_int,
            ">= -7",
            ast::Comparison {
                span: (0, 5).into(),
                opt_dot_expression: None,
                op: ast::ComparisonOperator {
                    span: (0, 2).into(),
                    kind: ast::ComparisonOperatorKind::GreaterOrEqual
                },
                literal: ast::Literal {
                    span: (3, 2).into(),
                    value: Int(-7)
                },
            }
        ),
        (
            ok,
            comparison_id_lt_float,
            "another_id < 1.7e9",
            ast::Comparison {
                span: (0, 18).into(),
                opt_dot_expression: Some(simple_dot_expression((0, 10).into(), "another_id")),
                op: ast::ComparisonOperator {
                    span: (11, 1).into(),
                    kind: ast::ComparisonOperatorKind::Less
                },
                literal: ast::Literal {
                    span: (13, 5).into(),
                    value: Float(1.7e9)
                },
            }
        ),
        (
            ok,
            comparison_lt_float,
            "< 1.7e9",
            ast::Comparison {
                span: (0, 7).into(),
                opt_dot_expression: None,
                op: ast::ComparisonOperator {
                    span: (0, 1).into(),
                    kind: ast::ComparisonOperatorKind::Less
                },
                literal: ast::Literal {
                    span: (2, 5).into(),
                    value: Float(1.7e9)
                },
            }
        ),
        (
            ok,
            comparison_id_lte_bool,
            "id3 <= true",
            ast::Comparison {
                span: (0, 11).into(),
                opt_dot_expression: Some(simple_dot_expression((0, 3).into(), "id3")),
                op: ast::ComparisonOperator {
                    span: (4, 2).into(),
                    kind: ast::ComparisonOperatorKind::LessOrEqual
                },
                literal: ast::Literal {
                    span: (7, 4).into(),
                    value: Bool(true)
                },
            }
        ),
        (
            ok,
            comparison_lte_bool,
            "<= true",
            ast::Comparison {
                span: (0, 7).into(),
                opt_dot_expression: None,
                op: ast::ComparisonOperator {
                    span: (0, 2).into(),
                    kind: ast::ComparisonOperatorKind::LessOrEqual
                },
                literal: ast::Literal {
                    span: (3, 4).into(),
                    value: Bool(true)
                },
            }
        ),
        (
            ok,
            comparison_id_eq_int,
            "id4 = 9",
            ast::Comparison {
                span: (0, 7).into(),
                opt_dot_expression: Some(simple_dot_expression((0, 3).into(), "id4")),
                op: ast::ComparisonOperator {
                    span: (4, 1).into(),
                    kind: ast::ComparisonOperatorKind::Equal
                },
                literal: ast::Literal {
                    span: (6, 1).into(),
                    value: Int(9)
                },
            }
        ),
        (
            ok,
            comparison_eq_int,
            "= 9",
            ast::Comparison {
                span: (0, 3).into(),
                opt_dot_expression: None,
                op: ast::ComparisonOperator {
                    span: (0, 1).into(),
                    kind: ast::ComparisonOperatorKind::Equal
                },
                literal: ast::Literal {
                    span: (2, 1).into(),
                    value: Int(9)
                },
            }
        ),
        (
            ok,
            comparison_dot_expr_eq_int,
            "a(true).b = 9",
            ast::Comparison {
                span: (0, 13).into(),
                opt_dot_expression: Some(ast::DotExpression {
                    span: (0, 9).into(),
                    field_calls: vec![
                        ast::FieldCall {
                            span: (0, 7).into(),
                            ident: ast::Ident {
                                span: (0, 1).into(),
                                name: "a".to_owned()
                            },
                            opt_field_access: None,
                            opt_arg_pack: Some(ast::ArgPack {
                                span: (1, 6).into(),
                                pos_args: vec![ast::Literal {
                                    span: (2, 4).into(),
                                    value: Bool(true)
                                }],
                                named_args: vec![]
                            }),
                            opt_filter_pack: None,
                        },
                        ast::FieldCall {
                            span: (8, 1).into(),
                            ident: ast::Ident {
                                span: (8, 1).into(),
                                name: "b".into()
                            },
                            opt_field_access: None,
                            opt_arg_pack: None,
                            opt_filter_pack: None,
                        }
                    ]
                }),
                op: ast::ComparisonOperator {
                    span: (10, 1).into(),
                    kind: ast::ComparisonOperatorKind::Equal
                },
                literal: ast::Literal {
                    span: (12, 1).into(),
                    value: Int(9)
                },
            }
        ),
        (none, comparison_eof, "", Ident),
        (none, comparison_int, "-78", Ident),
        (
            err,
            comparison_id_eof,
            "id5",
            Greater,
            GreaterOrEqual,
            Less,
            LessOrEqual,
            Equal
        ),
        (
            err,
            comparison_id_gt_eof,
            "id5 >",
            Int,
            Float,
            True,
            False,
            Str
        ),
        (
            err,
            comparison_id_id,
            "id5 id6",
            Greater,
            GreaterOrEqual,
            Less,
            LessOrEqual,
            Equal
        ),
    );

    gen_parse_tests!(
        parse_filter_pack,
        (
            ok,
            filter_pack_index,
            "[-90]",
            ast::FilterPack {
                span: (0, 5).into(),
                opt_filter: Some(ast::Filter::Index(ast::IntLiteral {
                    span: (1, 3).into(),
                    value: -90
                }))
            }
        ),
        (
            ok,
            filter_pack_slice_colon,
            "[:]",
            ast::FilterPack {
                span: (0, 3).into(),
                opt_filter: Some(ast::Filter::Slice(ast::Slice {
                    span: (1, 1).into(),
                    opt_start: None,
                    opt_stop: None,
                    opt_step: None,
                }))
            }
        ),
        (
            ok,
            filter_pack_slice_int_colon,
            "[50:]",
            ast::FilterPack {
                span: (0, 5).into(),
                opt_filter: Some(ast::Filter::Slice(ast::Slice {
                    span: (1, 3).into(),
                    opt_start: Some(ast::IntLiteral {
                        span: (1, 2).into(),
                        value: 50
                    }),
                    opt_stop: None,
                    opt_step: None,
                }))
            }
        ),
        (
            ok,
            filter_pack_slice_colon_int,
            "[:4]",
            ast::FilterPack {
                span: (0, 4).into(),
                opt_filter: Some(ast::Filter::Slice(ast::Slice {
                    span: (1, 2).into(),
                    opt_start: None,
                    opt_stop: Some(ast::IntLiteral {
                        span: (2, 1).into(),
                        value: 4
                    }),
                    opt_step: None,
                }))
            }
        ),
        (
            ok,
            filter_pack_slice_int_colon_int,
            "[7:-3]",
            ast::FilterPack {
                span: (0, 6).into(),
                opt_filter: Some(ast::Filter::Slice(ast::Slice {
                    span: (1, 4).into(),
                    opt_start: Some(ast::IntLiteral {
                        span: (1, 1).into(),
                        value: 7
                    }),
                    opt_stop: Some(ast::IntLiteral {
                        span: (3, 2).into(),
                        value: -3
                    }),
                    opt_step: None,
                }))
            }
        ),
        (
            ok,
            filter_pack_slice_colon_colon,
            "[::]",
            ast::FilterPack {
                span: (0, 4).into(),
                opt_filter: Some(ast::Filter::Slice(ast::Slice {
                    span: (1, 2).into(),
                    opt_start: None,
                    opt_stop: None,
                    opt_step: None,
                }))
            }
        ),
        (
            ok,
            filter_pack_slice_int_colon_colon,
            "[1::]",
            ast::FilterPack {
                span: (0, 5).into(),
                opt_filter: Some(ast::Filter::Slice(ast::Slice {
                    span: (1, 3).into(),
                    opt_start: Some(ast::IntLiteral {
                        span: (1, 1).into(),
                        value: 1
                    }),
                    opt_stop: None,
                    opt_step: None,
                }))
            }
        ),
        (
            ok,
            filter_pack_slice_colon_int_colon,
            "[:2:]",
            ast::FilterPack {
                span: (0, 5).into(),
                opt_filter: Some(ast::Filter::Slice(ast::Slice {
                    span: (1, 3).into(),
                    opt_start: None,
                    opt_stop: Some(ast::IntLiteral {
                        span: (2, 1).into(),
                        value: 2
                    }),
                    opt_step: None,
                }))
            }
        ),
        (
            ok,
            filter_pack_slice_colon_colon_int,
            "[::3]",
            ast::FilterPack {
                span: (0, 5).into(),
                opt_filter: Some(ast::Filter::Slice(ast::Slice {
                    span: (1, 3).into(),
                    opt_start: None,
                    opt_stop: None,
                    opt_step: Some(ast::IntLiteral {
                        span: (3, 1).into(),
                        value: 3
                    }),
                }))
            }
        ),
        (
            ok,
            filter_pack_slice_int_colon_int_colon,
            "[6:7:]",
            ast::FilterPack {
                span: (0, 6).into(),
                opt_filter: Some(ast::Filter::Slice(ast::Slice {
                    span: (1, 4).into(),
                    opt_start: Some(ast::IntLiteral {
                        span: (1, 1).into(),
                        value: 6
                    }),
                    opt_stop: Some(ast::IntLiteral {
                        span: (3, 1).into(),
                        value: 7
                    }),
                    opt_step: None,
                }))
            }
        ),
        (
            ok,
            filter_pack_slice_int_colon_colon_int,
            "[8::9]",
            ast::FilterPack {
                span: (0, 6).into(),
                opt_filter: Some(ast::Filter::Slice(ast::Slice {
                    span: (1, 4).into(),
                    opt_start: Some(ast::IntLiteral {
                        span: (1, 1).into(),
                        value: 8
                    }),
                    opt_stop: None,
                    opt_step: Some(ast::IntLiteral {
                        span: (4, 1).into(),
                        value: 9
                    }),
                }))
            }
        ),
        (
            ok,
            filter_pack_slice_colon_int_colon_int,
            "[:10:11]",
            ast::FilterPack {
                span: (0, 8).into(),
                opt_filter: Some(ast::Filter::Slice(ast::Slice {
                    span: (1, 6).into(),
                    opt_start: None,
                    opt_stop: Some(ast::IntLiteral {
                        span: (2, 2).into(),
                        value: 10
                    }),
                    opt_step: Some(ast::IntLiteral {
                        span: (5, 2).into(),
                        value: 11
                    }),
                }))
            }
        ),
        (
            ok,
            filter_pack_slice_int_colon_int_colon_int,
            "[12:13:14]",
            ast::FilterPack {
                span: (0, 10).into(),
                opt_filter: Some(ast::Filter::Slice(ast::Slice {
                    span: (1, 8).into(),
                    opt_start: Some(ast::IntLiteral {
                        span: (1, 2).into(),
                        value: 12
                    }),
                    opt_stop: Some(ast::IntLiteral {
                        span: (4, 2).into(),
                        value: 13
                    }),
                    opt_step: Some(ast::IntLiteral {
                        span: (7, 2).into(),
                        value: 14
                    }),
                }))
            }
        ),
        (
            ok,
            filter_pack_comparison,
            "[ab = 10]",
            ast::FilterPack {
                span: (0, 9).into(),
                opt_filter: Some(ast::Filter::Comparison(ast::Comparison {
                    span: (1, 7).into(),
                    opt_dot_expression: Some(simple_dot_expression((1, 2).into(), "ab")),
                    op: ast::ComparisonOperator {
                        span: (4, 1).into(),
                        kind: ast::ComparisonOperatorKind::Equal
                    },
                    literal: ast::Literal {
                        span: (6, 2).into(),
                        value: Int(10)
                    },
                }))
            }
        ),
        (
            ok,
            filter_pack_comparison_no_ident,
            "[=true]",
            ast::FilterPack {
                span: (0, 7).into(),
                opt_filter: Some(ast::Filter::Comparison(ast::Comparison {
                    span: (1, 5).into(),
                    opt_dot_expression: None,
                    op: ast::ComparisonOperator {
                        span: (1, 1).into(),
                        kind: ast::ComparisonOperatorKind::Equal
                    },
                    literal: ast::Literal {
                        span: (2, 4).into(),
                        value: Bool(true)
                    },
                }))
            }
        ),
        (
            ok,
            filter_pack_none,
            "[ ]",
            ast::FilterPack {
                span: (0, 3).into(),
                opt_filter: None
            }
        ),
        (none, filter_pack_eof, "", LBracket),
        (none, filter_pack_float, "1.2", LBracket),
        (err, filter_pack_no_rbrac, "[", Int, Colon, Ident, RBracket),
    );
}
