// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

use rstest::rstest;
use serde_json::json;

use integration_test_util::{test_query_ok, TempFiles};

mod integration_test_util;

#[rstest]
#[case(0o0123, 0o123, false, false, false)]
#[case(0o4456, 0o456, true, false, false)]
#[case(0o2777, 0o777, false, true, false)]
#[case(0o1001, 0o001, false, false, true)]
fn sqfilemode(
    #[case] mode: u32,
    #[case] perms: u32,
    #[case] suid: bool,
    #[case] sgid: bool,
    #[case] sticky: bool,
) {
    let temp_files = TempFiles::new().file("file1").chmod("file1", mode);
    let query = format!(
        "<path(\"{}\").<file.<mode {{permissions suid sgid sticky}}",
        &temp_files[0]
    );
    test_query_ok(
        &query,
        json!({"permissions": perms, "suid": suid, "sgid": sgid, "sticky": sticky}),
    );
    temp_files.chmod("file1", 0o777);
}
