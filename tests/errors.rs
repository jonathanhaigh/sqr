// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

mod integration_test_util;

use integration_test_util::test_simple_query_err;

test_simple_query_err!(
    errors,
    lex, "&", Lex;
    unexpected_token, "true", UnexpectedToken;
    int_too_big, "int(9223372036854775808)", ParseValue; // i64::MAX + 1
    int_too_small, "int(-9223372036854775809)", ParseValue; // i64::MAX + 1
    float_too_big, "float(10.0E+308)", ParseValue;
    float_too_small, "float(10.0E+308)", ParseValue;
    invalid_string, r#"string("\uxxxx")"#, ParseValue;
    repeated_named_arg, "int(value=10, value=20)", RepeatedNamedArg;
    arg_type_mismatch, "int(true)", ArgTypeMismatch;
    invalid_field, "xyzabc", InvalidField;
    invalid_field_on_root, "xyzabc", InvalidField;
    invalid_field_on_int, "int.xyzabc", InvalidField;
    duplicate_field_on_root, "int int", DuplicateField;
    duplicate_field_on_root_with_different_args, "int(1) int(2)", DuplicateField;
    duplicate_field_on_int, "bool { not not }", DuplicateField;
    pullup_with_siblings, "<int bool", PullupWithSiblings;
    system, "ints(0, 10, 0)", System;
    filter_index_out_of_bounds_nonneg, "ints(0, 10)[10]", FilterIndexOutOfBounds;
    filter_index_out_of_bounds_neg, "ints(0, 10)[-11]", FilterIndexOutOfBounds;
    slice_step_zero, "ints(0, 10)[0:1:0]", SliceStepZero;
    non_single_field_in_comparison, r#"path.children[children<"a"]"#, NonSingleFieldInComparison;
    comparison_type_mismatch, "ints(0, 10)[<true]", ComparisonTypeMismatch;
    // TODO: Serialize
    // TODO: ArgMissing (do we actually have any fields with required args?)
    // TODO: ToPrimitive (only ToPrimitive failure ATM is non-UTF-8 in a path)
    // TODO: ConvertInteger (can we get this failure on 64-bit architectures?),
);
