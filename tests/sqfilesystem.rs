// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

use std::process::Command;

use serde::Deserialize;
use serde_json::json;

use integration_test_util::{get_query_as, test_query_ok};

mod integration_test_util;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum StatfsField {
    FundamentalBlockSize,
    AvailableBlocks,
    TotalBlocks,
}

impl StatfsField {
    pub fn escape_code(&self) -> &'static str {
        match self {
            Self::AvailableBlocks => "%a",
            Self::TotalBlocks => "%b",
            Self::FundamentalBlockSize => "%S",
        }
    }
}

// TODO: test SqFilesystem::id. The ID returned by "stat -f" is different (or encoded differently)
// to the ID returned by statvfs(3)...

fn get_statfs_field(path: &str, field: StatfsField) -> String {
    let field_escape = field.escape_code();
    let args = ["-f", "-c", &field_escape, path];
    let output = Command::new("stat")
        .args(args)
        .output()
        .expect("failed to call stat(1)");
    if !output.status.success() {
        panic!(
            "Command stat with args {:?}, failed with code {:?}, stdout {} and stderr {}",
            args,
            output.status.code(),
            String::from_utf8_lossy(output.stdout.as_slice()),
            String::from_utf8_lossy(output.stderr.as_slice())
        );
    }
    String::from_utf8(output.stdout).expect("Failed to decode stat(1) output as UTF-8")
}

fn get_statfs_u64_field(path: &str, field: StatfsField) -> u64 {
    let field_str = get_statfs_field(path, field).trim().to_owned();
    match field_str.parse::<u64>() {
        Err(e) => panic!(
            "Failed to parse statfs {:?} field ({}): {}",
            field, field_str, e
        ),
        Ok(v) => v,
    }
}

#[test]
fn filesystem_block_size() {
    let block_size = get_statfs_u64_field("/", StatfsField::FundamentalBlockSize);
    test_query_ok(r#"<path("/").<filesystem.<block_size"#, json!(block_size));
}

#[test]
fn filesystem_blocks() {
    let blocks = get_statfs_u64_field("/", StatfsField::TotalBlocks);
    test_query_ok(r#"<path("/").<filesystem.<blocks"#, json!(blocks));
}

// Ignore this test because the number of available blocks may change between the time we read
// using stat(1) and the time we run the query.
#[ignore]
#[test]
fn filesystem_blocks_available() {
    let blocks_available = get_statfs_u64_field("/", StatfsField::AvailableBlocks);
    test_query_ok(
        r#"<path("/").<filesystem.<blocks_available"#,
        json!(blocks_available),
    );
}

// Ignore this test because the number of available blocks may change between the time we read
// using stat(1) and the time we run the query.
#[ignore]
#[test]
fn filesystem_blocks_used() {
    let blocks = get_statfs_u64_field("/", StatfsField::TotalBlocks);
    let blocks_available = get_statfs_u64_field("/", StatfsField::AvailableBlocks);
    let blocks_used = blocks - blocks_available;
    test_query_ok(r#"<path("/").<filesystem.<blocks_used"#, json!(blocks_used));
}

#[test]
fn filesystem_blocks_used_plus_available_is_total() {
    #[derive(Debug, Deserialize, Eq, PartialEq)]
    struct BlockInfo {
        blocks_used: u64,
        blocks_available: u64,
        blocks: u64,
    }
    let ret = get_query_as::<BlockInfo>(
        r#"<path("/").<filesystem { blocks_used blocks_available blocks }"#,
    );
    assert_eq!(ret.blocks, ret.blocks_used + ret.blocks_available);
}

#[test]
fn filesystem_size() {
    let blocks = get_statfs_u64_field("/", StatfsField::TotalBlocks);
    let block_size = get_statfs_u64_field("/", StatfsField::FundamentalBlockSize);
    let size = blocks.checked_mul(block_size).unwrap();
    test_query_ok(r#"<path("/").<filesystem.<size"#, json!(size));
}

// Ignore this test because the number of available blocks may change between the time we read
// using stat(1) and the time we run the query.
#[ignore]
#[test]
fn filesystem_space_available() {
    let blocks_available = get_statfs_u64_field("/", StatfsField::AvailableBlocks);
    let block_size = get_statfs_u64_field("/", StatfsField::FundamentalBlockSize);
    let available = blocks_available.checked_mul(block_size).unwrap();
    test_query_ok(
        r#"<path("/").<filesystem.<space_available"#,
        json!(available),
    );
}

// Ignore this test because the number of available blocks may change between the time we read
// using stat(1) and the time we run the query.
#[ignore]
#[test]
fn filesystem_space_used() {
    let blocks = get_statfs_u64_field("/", StatfsField::TotalBlocks);
    let blocks_available = get_statfs_u64_field("/", StatfsField::AvailableBlocks);
    let blocks_used = blocks.checked_sub(blocks_available).unwrap();
    let block_size = get_statfs_u64_field("/", StatfsField::FundamentalBlockSize);
    let used = blocks_used.checked_mul(block_size).unwrap();
    test_query_ok(r#"<path("/").<filesystem.<space_used"#, json!(used));
}

#[test]
fn filesystem_space_used_plus_available_is_total() {
    #[derive(Debug, Deserialize, Eq, PartialEq)]
    struct SpaceInfo {
        space_used: u64,
        space_available: u64,
        size: u64,
    }
    let ret =
        get_query_as::<SpaceInfo>(r#"<path("/").<filesystem { space_used space_available size }"#);
    assert_eq!(ret.size, ret.space_used + ret.space_available);
}

#[test]
fn filesystem_percent_used() {
    #[derive(Debug, Deserialize)]
    struct UsedInfo {
        space_used: u64,
        size: u64,
        percent_used: f64,
    }
    let ret =
        get_query_as::<UsedInfo>(r#"<path("/").<filesystem { space_used size percent_used }"#);
    approx::assert_ulps_eq!(
        ret.percent_used,
        100f64 * (ret.space_used as f64 / ret.size as f64),
        max_ulps = 10
    );
}

#[test]
fn filesystem_percent_available() {
    #[derive(Debug, Deserialize)]
    struct AvailableInfo {
        space_available: u64,
        size: u64,
        percent_available: f64,
    }
    let ret = get_query_as::<AvailableInfo>(
        r#"<path("/").<filesystem { space_available size percent_available }"#,
    );
    approx::assert_ulps_eq!(
        ret.percent_available,
        100f64 * (ret.space_available as f64 / ret.size as f64),
        max_ulps = 10
    );
}

#[test]
fn filesystem_percent_used_plus_available_is_100() {
    #[derive(Debug, Deserialize)]
    struct PercentInfo {
        percent_used: f64,
        percent_available: f64,
    }
    let ret =
        get_query_as::<PercentInfo>(r#"<path("/").<filesystem { percent_used percent_available }"#);
    approx::assert_ulps_eq!(
        ret.percent_used + ret.percent_available,
        100f64,
        max_ulps = 10,
    );
}
