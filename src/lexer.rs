// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

//! Types and functions related to lexing (tokenizing) an SQ query.
use fancy_regex::Regex;
use miette::SourceSpan;
use once_cell::sync::Lazy;

use crate::error::{Error, Result};

/// Enumeration of the kinds of tokens recognized by the lexer.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[must_use]
pub enum TokenKind {
    /// `false`.
    False,
    /// `true`,
    True,
    /// `:`
    Colon,
    /// `,`
    Comma,
    /// `.`
    Dot,
    /// End of the query
    Eof,
    /// A JSON double quoted string
    Str,
    /// `=`
    Equal,
    /// A floating point number literal
    Float,
    /// `>`
    Greater,
    /// `>=`
    GreaterOrEqual,
    /// An identifier
    Ident,
    /// An integer
    Int,
    /// `{`
    LBrace,
    /// `[`
    LBracket,
    /// `<`
    Less,
    /// `<=`
    LessOrEqual,
    /// `!=`
    NotEqual,
    /// `(`
    LParen,
    /// `}`
    RBrace,
    /// `]`
    RBracket,
    /// `)`
    RParen,
    /// A string of whitespace
    Whitespace,
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

/// A token from the input query.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[must_use]
pub struct Token {
    /// The region of the query that the token covers.
    pub span: SourceSpan,
    /// The kind of token.
    pub kind: TokenKind,
}

impl Token {
    #[must_use]
    /// Get the substring of the query that the token represents.
    ///
    /// # Parameters
    /// - `query`: the input query. This is required because `Token`s only contain indeces into the
    ///   query, not the part of (or a direct reference to) the query text itself.
    pub fn text<'q>(&self, query: &'q str) -> &'q str {
        let start = self.span.offset();
        let end = start + self.span.len();
        &query[start..end]
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{:?}({}, {})",
            self.kind,
            self.span.offset(),
            self.span.len(),
        )
    }
}

/// Compile the regex for a given kind of token.
///
/// # Panics
/// - If the regex can't be compiled.
fn token_regex_or_panic(kind: TokenKind, regex_str: &str) -> (TokenKind, Regex) {
    match Regex::new(regex_str) {
        Ok(r) => (kind, r),
        Err(e) => panic!("failed to compile regex for token kind {}: {}", kind, e),
    }
}

/// Match the next token in the query.
///
/// # Parameters
/// - `remaining`: the remaining unparsed part of the query.
///
/// # Returns
/// - `None` if a token could not be parsed.
/// - `Some(kind, len)` where `kind` is kind of token matched and `len` is the length of the match.
///
/// # Panics
/// - If executing a regex for a token kind fails.
fn match_token(remaining: &str) -> Option<(TokenKind, usize)> {
    static REGEX_MAP: Lazy<Vec<(TokenKind, Regex)>> = Lazy::new(|| {
        vec![
            token_regex_or_panic(TokenKind::LParen, r"^[(]"),
            token_regex_or_panic(TokenKind::RParen, r"^[)]"),
            token_regex_or_panic(TokenKind::LBrace, r"^[{]"),
            token_regex_or_panic(TokenKind::RBrace, r"^[}]"),
            token_regex_or_panic(TokenKind::LBracket, r"^\["),
            token_regex_or_panic(TokenKind::RBracket, r"^\]"),
            token_regex_or_panic(TokenKind::Comma, r"^,"),
            token_regex_or_panic(TokenKind::Colon, r"^:"),
            token_regex_or_panic(TokenKind::Str, "^\"(?:[^\"\\\\]|\\\\.)*\""),
            // Order matters here:
            // * Prefer to match "<=" than "<" then "=".
            // * Prefer to match ">=" than ">" then "=".
            token_regex_or_panic(TokenKind::LessOrEqual, r"^<="),
            token_regex_or_panic(TokenKind::Less, r"^<"),
            token_regex_or_panic(TokenKind::GreaterOrEqual, r"^>="),
            token_regex_or_panic(TokenKind::Greater, r"^>"),
            token_regex_or_panic(TokenKind::Equal, r"^="),
            token_regex_or_panic(TokenKind::NotEqual, r"^!="),
            // Order matters here:
            // * Prefer to match "true" and "false" before idents but only if
            //   it doesn't look like "true" or "false" is just the start of a
            //   longer ident (e.g. "true1", "false_id")
            token_regex_or_panic(TokenKind::True, r"^true(?![A-Za-z_0-9])"),
            token_regex_or_panic(TokenKind::False, r"^false(?![A-Za-z_0-9])"),
            token_regex_or_panic(TokenKind::Ident, r"^[A-Za-z_][A-Za-z_0-9]*"),
            // Order matters here:
            // * Prefer to match an Int to a Float, but only if there's no "."
            //   after the int.
            // * Prefer to match a Float to a Dot.
            token_regex_or_panic(TokenKind::Int, r"^[-]?[0-9]+(?![0-9.])"),
            token_regex_or_panic(
                TokenKind::Float,
                "^[-]?(?=[.]?[0-9])[0-9]*(?:[.][0-9]*)?(?:[Ee][+-]?[0-9]+)?",
            ),
            token_regex_or_panic(TokenKind::Dot, r"^[.]"),
            token_regex_or_panic(TokenKind::Whitespace, r"^\s+"),
        ]
    });

    for (kind, regex) in &*REGEX_MAP {
        match regex.find(remaining) {
            Ok(Some(m)) => return Some((*kind, m.end())),
            Ok(None) => continue,
            Err(e) => panic!("failed to run regex match for token kind {}: {}", *kind, e),
        }
    }

    None
}

/// Generate an `Error::LexError` for unexpected text at a given offset in the query.
fn lex_error(offset: usize) -> Box<Error> {
    Box::new(Error::Lex {
        span: (offset, 0).into(),
    })
}

/// Lex the query into tokens.
///
/// # Parameters
/// - `query`: the query to lex.
///
/// # Returns
/// - A `Vec<Token>` of tokens making up the query on success.
/// - An `Error::Lex` if lexing fails.
///
/// # Panics
/// - If regexes couldn't be compiled or executed.
pub fn lex(query: &str) -> Result<Vec<Token>> {
    let mut offset = 0;
    let mut tokens = Vec::<Token>::new();

    loop {
        if offset == query.len() {
            tokens.push(Token {
                span: (offset, 0).into(),
                kind: TokenKind::Eof,
            });
            return Ok(tokens);
        }

        assert!(offset < query.len());

        match match_token(&query[offset..]) {
            Some((TokenKind::Whitespace, len)) => {
                offset += len;
                continue;
            }
            Some((kind, len)) => {
                tokens.push(Token {
                    span: (offset, len).into(),
                    kind,
                });
                offset += len;
                continue;
            }
            None => return Err(lex_error(offset)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use TokenKind::*;

    use pretty_assertions::assert_eq;
    use rstest::rstest;

    #[rstest]
    #[case::boolfalse(vec![("false", False)])]
    #[case::false_prefixed_ident(vec![("false_id", Ident)])]
    #[case::booltrue(vec![("true", True)])]
    #[case::true_prefixed_ident(vec![("true_id", Ident)])]
    #[case::colon(vec![(":", Colon)])]
    #[case::comma(vec![(",", Comma)])]
    #[case::dot(vec![(".", Dot)])]
    #[case::str(vec![("\"a string\"", Str)])]
    #[case::str_with_quotes(vec![("\"a string with \\\"quotes\\\" \"", Str)])]
    #[case::str_with_backslash(vec![("\"a string with \\\\ backslash\"", Str)])]
    #[case::equal(vec![("=", Equal)])]
    #[case::not_equal(vec![("!=", NotEqual)])]
    #[case::float(vec![("12.34", Float)])]
    #[case::float_starts_with_dot(vec![(".56", Float)])]
    #[case::float_with_e(vec![("7.1e+6", Float)])]
    #[case::float_with_big_e(vec![("1.0E3", Float)])]
    #[case::greater(vec![(">", Greater)])]
    #[case::greaterorequal(vec![(">=", GreaterOrEqual)])]
    #[case::ident(vec![("an_identifier", Ident)])]
    #[case::ident_with_digits(vec![("var89", Ident)])]
    #[case::int(vec![("98", Int)])]
    #[case::int_neg(vec![("-110", Int)])]
    #[case::int_zero(vec![("0", Int)])]
    #[case::int_neg_zero(vec![("-0", Int)])]
    #[case::lbrace(vec![("{", LBrace)])]
    #[case::lbracket(vec![("[", LBracket)])]
    #[case::less(vec![("<", Less)])]
    #[case::lessorequal(vec![("<=", LessOrEqual)])]
    #[case::lparen(vec![("(", LParen)])]
    #[case::rbrace(vec![("}", RBrace)])]
    #[case::rbracket(vec![("]", RBracket)])]
    #[case::rparen(vec![(")", RParen)])]
    #[case::field_call(vec![
        ("a", Ident),
        (" ", Whitespace),
        (".", Dot),
        ("\t", Whitespace),
        ("b", Ident),
        ("\r", Whitespace),
        ("(", LParen),
        ("\n", Whitespace),
        ("5", Int),
        (",", Comma),
        ("6.1", Float),
        (",", Comma),
        ("x", Ident),
        ("=", Equal),
        ("\"some string\"", Str),
        (")", RParen),
        ("[", LBracket),
        ("y", Ident),
        ("<=", LessOrEqual),
        ("-10", Int),
        ("]", RBracket)
    ])]
    fn test_lex(#[case] fragments: Vec<(&str, TokenKind)>) {
        let query: String = fragments.iter().map(|x| x.0).collect();

        let mut expected: Vec<Token> = vec![];
        let mut offset = 0;
        for (fragment, kind) in fragments {
            // Whitespace is ignored by TokenIterator, so filter it out
            // here too.
            if kind != TokenKind::Whitespace {
                expected.push(Token {
                    span: (offset, fragment.len()).into(),
                    kind,
                });
            }
            offset += fragment.len();
        }
        expected.push(Token {
            span: (query.len(), 0).into(),
            kind: TokenKind::Eof,
        });

        let actual: Vec<Token> = lex(&query).unwrap();
        assert_eq!(actual, expected);
    }
}
