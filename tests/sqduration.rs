// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

use rstest::rstest;
use serde_json::json;

use integration_test_util::test_query_ok;

mod integration_test_util;

#[rstest]
#[case::whole_secs(99_999, 0)]
#[case::half_sec(0, 500_000_000)]
#[case::half_millisec(0, 500_000)]
#[case::half_microsec(0, 500)]
#[case::nanosec(0, 1)]
fn test_duration(#[case] s: u64, #[case] ns: u64) {
    let query = format!(
        "<duration(s={}, ns={}) {{s subsec_ms subsec_us subsec_ns}}",
        s, ns
    );
    test_query_ok(
        &query,
        json!({
            "s": s,
            "subsec_ms": ns/1000000,
            "subsec_us": ns/1000,
            "subsec_ns": ns
        }),
    );
}
