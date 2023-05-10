// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

mod integration_test_util;

use integration_test_util::test_simple_query_ok;

test_simple_query_ok!(
    sqroot_bool_ok,
    default_value, "<bool", "false";
    bool_false, "<bool(false)", "false";
    bool_true, "<bool(true)", "true";
);

test_simple_query_ok!(
    sqroot_int_ok,
    default_value, "<int", "0";
    one, "<int(1)", "1";
    minus_one, "<int(-1)", "-1";
    i64_max, "<int(9223372036854775807)", "9223372036854775807"; // i64::MAX
    i64_min, "<int(-9223372036854775808)", "-9223372036854775808"; // i64::MIN
);
