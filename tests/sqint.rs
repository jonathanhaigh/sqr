// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

use integration_test_util::test_simple_query_ok;

mod integration_test_util;

test_simple_query_ok!(
    sqint_to_primitive,
    u64_max, "<int(18446744073709551615)", json!(u64::MAX);
    i64_max, "<int(9223372036854775807)", json!(i64::MAX);
    i64_min, "<int(-9223372036854775808)", json!(i64::MIN);
);

test_simple_query_ok!(
    sqint_string,
    u64_max, "<int(18446744073709551615).<string", json!("18446744073709551615");
    i64_max, "<int(9223372036854775807).<string", json!("9223372036854775807");
    i64_min, "<int(-9223372036854775808).<string", json!("-9223372036854775808");
);
