// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

mod integration_test_util;

use integration_test_util::test_simple_query_ok;

test_simple_query_ok!(
    int_comparison,
    lt, "<ints(0, 10)[<5]", json!([0, 1, 2, 3, 4]);
    lte, "<ints(0, 10)[<=5]", json!([0, 1, 2, 3, 4, 5]);
    gt, "<ints(0, 10)[>5]", json!([6, 7, 8, 9]);
    gte, "<ints(0, 10)[>=5]", json!([5, 6, 7, 8, 9]);
    eq, "<ints(0, 10)[=5]", json!([5]);
    ne, "<ints(0, 10)[!=5]", json!([0, 1, 2, 3, 4, 6, 7, 8, 9]);
);

// Note that when compared as strings, "10" < "11" < "8" < "9".
test_simple_query_ok!(
    str_comparison,
    lt, r#"<ints(8, 12)[string<"9"]"#, json!([8, 10, 11]);
    lte, r#"<ints(8, 12)[string<="8"]"#, json!([8, 10, 11]);
    gt, r#"<ints(8, 12)[string>"10"]"#, json!([8, 9, 11]);
    gte, r#"<ints(8, 12)[string>="11"]"#, json!([8, 9, 11]);
    eq, r#"<ints(8, 12)[string="10"]"#, json!([10]);
    ne, r#"<ints(8, 12)[string!="10"]"#, json!([8, 9, 11]);
);

// TODO: test f64 comparisons when we have a way to generate a list of floats.
// TODO: test bool comparisons when we have a way to generate a list of bools.
