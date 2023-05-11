// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

use pretty_assertions::assert_eq;
use serde_json::Value as JsonValue;

use sqr::error::{ErrorKind, Result};
use sqr::parser::Parser;
use sqr::{lexer, results, system};

pub fn run_query(query: &str) -> Result<String> {
    let tokens = lexer::lex(query)?;

    let mut p = Parser::new(query, &tokens);
    let ast = p.parse()?;

    let buff = Vec::new();
    let mut serializer = serde_json::Serializer::pretty(buff);
    results::generate_results(&ast, &mut serializer, system::root())?;
    Ok(String::from_utf8(serializer.into_inner()).unwrap())
}

// Rust doesn't seem to see that this function is actually used.
#[allow(dead_code)]
pub fn test_query_ok(query: &str, expected: JsonValue) {
    assert_eq!(
        serde_json::from_str::<serde_json::Value>(run_query(query).unwrap().as_str()).unwrap(),
        expected,
    );
}

// Rust doesn't seem to see that this macro is actually used.
#[allow(unused_macros)]
macro_rules! test_simple_query_ok {
    ($name:ident, $($case_name:ident, $query:expr, $expected:expr;)*) => {
        // Put the test function in a new module to avoid "use" statements polluting the caller's
        // namespace
        mod $name {
            use rstest::rstest;
            use serde_json::Value as JsonValue;
            use serde_json::json;
            #[rstest]
            $(#[case::$case_name($query, $expected)])*
            fn test_query_ok(#[case] query: &str, #[case] expected: JsonValue) {
                $crate::integration_test_util::test_query_ok(query, expected);
            }
        }
    }
}
#[allow(unused_imports)]
pub(crate) use test_simple_query_ok;

// Rust doesn't seem to see that this function is actually used.
#[allow(dead_code)]
pub fn test_query_err(query: &str, kind: ErrorKind) {
    assert_eq!(run_query(query).unwrap_err().kind(), kind);
}

// Rust doesn't seem to see that this macro is actually used.
#[allow(unused_macros)]
macro_rules! test_simple_query_err {
    ($name:ident, $($case_name:ident, $query:expr, $expected:ident;)*) => {
        // Put the test function in a new module to avoid "use" statements polluting the caller's
        // namespace
        mod $name {
            use rstest::rstest;
            use sqr::error::ErrorKind;
            #[rstest]
            $(#[case::$case_name($query, ErrorKind::$expected)])*
            fn test_query_err(#[case] query: &str, #[case] expected: ErrorKind) {
                $crate::integration_test_util::test_query_err(query, expected);
            }
        }
    }
}

#[allow(unused_imports)]
pub(crate) use test_simple_query_err;
