// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

use rstest::rstest;
use serde_json::json;

use integration_test_util::{get_query, get_query_as, test_query_ok, TempFiles};

mod integration_test_util;

#[test]
fn sqfile_inode() {
    let temp_files = TempFiles::new().file("file1").link("file1", "file2");
    let query1 = format!("<path(\"{}\").<file.<inode", &temp_files[0]);
    let query2 = format!("<path(\"{}\").<file.<inode", &temp_files[1]);
    assert_eq!(get_query_as::<i64>(&query1), get_query_as::<i64>(&query2));
}

#[rstest]
#[case::zero(0)]
#[case::one(1)]
#[case::four_k(4096)]
fn sqfile_size(#[case] len: i64) {
    let temp_files = TempFiles::new().file("file1").truncate("file1", len);
    let query = format!("<path(\"{}\").<file.<size", &temp_files[0]);
    test_query_ok(&query, json!(len));
}

#[test]
fn sqfile_type_file() {
    let temp_files = TempFiles::new().file("file1");
    let query = format!("<path(\"{}\").<file.<type", &temp_files[0]);
    test_query_ok(&query, json!("file"));
}

#[test]
fn sqfile_type_dir() {
    let temp_files = TempFiles::new().dir("dir1");
    let query = format!("<path(\"{}\").<file.<type", &temp_files[0]);
    test_query_ok(&query, json!("dir"));
}

#[test]
fn sqfile_type_symlink() {
    let temp_files = TempFiles::new().file("file1").symlink("file1", "symlink1");
    let query = format!(
        "<path(\"{}\").<file(follow_symlinks=false).<type",
        &temp_files[1]
    );
    test_query_ok(&query, json!("symlink"));
}

#[test]
fn sqfile_type_char_dev() {
    // Let's just assume, for now, that /dev/tty exists and is a char device.
    test_query_ok(
        "<path(\"/dev/tty\").<file(follow_symlinks=true).<type",
        json!("char"),
    );
}

#[test]
fn sqfile_type_block_dev() {
    // Let's just assume, for now, that /dev/loop0 exists and is a block device.
    test_query_ok(
        "<path(\"/dev/loop0\").<file(follow_symlinks=true).<type",
        json!("block"),
    );
}

#[test]
fn sqfile_type_fifo() {
    let temp_files = TempFiles::new().fifo("fifo1", 0777);
    let query = format!("<path(\"{}\").<file.<type", &temp_files[0]);
    test_query_ok(&query, json!("fifo"));
}

#[test]
fn sqfile_type_socket() {
    let temp_files = TempFiles::new().unix_socket("sock1");
    let query = format!("<path(\"{}\").<file.<type", &temp_files[0]);
    test_query_ok(&query, json!("socket"));
}

#[test]
fn sqfile_hard_link_count() {
    let mut temp_files = TempFiles::new().file("file1");
    let query1 = format!("<path(\"{}\").<file.<hard_link_count", &temp_files[0]);
    test_query_ok(&query1, json!(1));

    temp_files = temp_files.link("file1", "file2");
    let query2 = format!("<path(\"{}\").<file.<hard_link_count", &temp_files[1]);
    test_query_ok(&query1, json!(2));
    test_query_ok(&query2, json!(2));

    temp_files = temp_files.link("file1", "file3");
    let query3 = format!("<path(\"{}\").<file.<hard_link_count", &temp_files[2]);
    test_query_ok(&query1, json!(3));
    test_query_ok(&query2, json!(3));
    test_query_ok(&query3, json!(3));
}

#[rstest]
fn sqfile_mode(
    #[values(0, 1, 2, 4, 7)] extra: u32,
    #[values(0, 1, 2, 4, 7)] user: u32,
    #[values(0, 1, 2, 4, 7)] group: u32,
    #[values(0, 1, 2, 4, 7)] world: u32,
) {
    let mode = world + 8 * group + 64 * user + 512 * extra;
    let temp_files = TempFiles::new().file("file1").chmod("file1", mode);
    let query = format!("<path(\"{}\").<file.<mode", &temp_files[0]);
    test_query_ok(&query, json!(mode));
    temp_files.chmod("file1", 0777);
}

type SAndUs = (i64, i64);
const EPOCH: SAndUs = (0, 0);
const DAY_AND_A_BIT: SAndUs = (86_400, 123_456);
const YEAR_AND_A_BIT: SAndUs = (31_536_000, 789_012);
const CENTURY_AND_A_BIT: SAndUs = (3_153_600_000, 345_678);

#[rstest]
#[case::epoch_day_and_a_bit(EPOCH, DAY_AND_A_BIT)]
#[case::year_and_a_bit_epoch(YEAR_AND_A_BIT, EPOCH)]
#[case::two_centuries_and_a_bit_epoch(CENTURY_AND_A_BIT, CENTURY_AND_A_BIT)]
fn sqfile_atime_mtime(#[case] atime: (i64, i64), #[case] mtime: (i64, i64)) {
    let temp_files = TempFiles::new().file("file1").utimes("file1", atime, mtime);
    let atime_query = format!(
        "<path(\"{}\").<file.<atime.<duration_since_epoch {{s subsec_us}}",
        &temp_files[0]
    );
    let mtime_query = format!(
        "<path(\"{}\").<file.<mtime.<duration_since_epoch {{s subsec_us}}",
        &temp_files[0]
    );

    test_query_ok(&atime_query, json!({"s": atime.0, "subsec_us": atime.1}));
    test_query_ok(&mtime_query, json!({"s": mtime.0, "subsec_us": mtime.1}));
}

// NOTE: testing ctime is a bit tricky: there's no simple way to set the ctime of a file to an
// arbitrary value on Linux without changing the system clock.

// NOTE: testing block count is a bit tricky: creating a file with a specific block count would
// depend on the file system.

#[test]
fn sqfile_user() {
    let temp_files = TempFiles::new().file("file1");
    let query1 = format!("<path(\"{}\").<file.<user", &temp_files[0]);
    let query2 = format!("<user");
    assert_eq!(get_query(&query1).unwrap(), get_query(&query2).unwrap());
}

#[test]
fn sqfile_group() {
    let temp_files = TempFiles::new().file("file1");
    let query1 = format!("<path(\"{}\").<file.<group", &temp_files[0]);
    let query2 = format!("<group");
    assert_eq!(get_query(&query1).unwrap(), get_query(&query2).unwrap());
}
