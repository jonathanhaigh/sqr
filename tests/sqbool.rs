// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

use integration_test_util::test_simple_query_ok;

mod integration_test_util;

test_simple_query_ok!(
    sqbool_not,
    not_true, "<bool(true).<not", json!(false);
    not_false, "<bool(false).<not", json!(true);

);
