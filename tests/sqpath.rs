// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

use std::ffi::OsString;
use std::fs;
use std::os::unix::ffi::OsStringExt;
use std::path::PathBuf;

use rstest::rstest;
use serde_json::json;
use tempfile::TempDir;

use integration_test_util::{
    get_query, get_query_as, get_query_as_sorted_vec, test_query_err, test_query_ok,
    test_simple_query_ok, TempFiles,
};
use sqr::error::ErrorKind;

mod integration_test_util;

test_simple_query_ok!(
    sqpath_string,
    all_unicode_planes,
        "<path(\"\u{B123}\u{1B123}\u{2B123}\u{30123}\u{E0021}\u{F0001}\").<string",
        json!("\u{B123}\u{1B123}\u{2B123}\u{30123}\u{E0021}\u{F0001}");
);

#[rstest]
#[case::invalid_2octet_seq(vec![0xc3, 0x28])]
#[case::invalid_seq_identifier(vec![0xa0, 0xa1])]
#[case::invalid_2nd_of_3_octets(vec![0xe2, 0x28, 0xa1])]
#[case::invalid_3rd_of_3_octets(vec![0xe2, 0x82, 0x28])]
#[case::invalid_2nd_of_4_octets(vec![0xf0, 0x28, 0x8c, 0xbc])]
#[case::invalid_3rd_of_4_octets(vec![0xf0, 0x90, 0x28, 0xbc])]
#[case::invalid_4th_of_4_octets(vec![0xf0, 0x28, 0x8c, 0x28])]
fn sqpath_string_invalid_utf8_err(#[case] bytes: Vec<u8>) {
    let filename = PathBuf::from(OsString::from_vec(bytes));
    let temp_dir = TempDir::new().unwrap();
    fs::write(temp_dir.path().join(filename), "").unwrap();
    let query = format!(
        "path(\"{}\").children[0].string",
        temp_dir.path().to_str().unwrap()
    );
    test_query_err(&query, ErrorKind::System);
}

#[rstest]
#[case::invalid_2octet_seq(vec![0xc3, 0x28])]
#[case::invalid_seq_identifier(vec![0xa0, 0xa1])]
#[case::invalid_2nd_of_3_octets(vec![0xe2, 0x28, 0xa1])]
#[case::invalid_3rd_of_3_octets(vec![0xe2, 0x82, 0x28])]
#[case::invalid_2nd_of_4_octets(vec![0xf0, 0x28, 0x8c, 0xbc])]
#[case::invalid_3rd_of_4_octets(vec![0xf0, 0x90, 0x28, 0xbc])]
#[case::invalid_4th_of_4_octets(vec![0xf0, 0x28, 0x8c, 0x28])]
fn sqpath_string_replace_invalid_utf8(#[case] bytes: Vec<u8>) {
    let filename = PathBuf::from(OsString::from_vec(bytes));
    let temp_dir = TempDir::new().unwrap();
    fs::write(temp_dir.path().join(filename), "").unwrap();
    let query = format!(
        "path(\"{}\").children[0].string(replace_invalid=true)",
        temp_dir.path().to_str().unwrap()
    );
    assert!(get_query(&query).is_ok());
}

#[rstest]
#[case::invalid_2octet_seq(vec![0xc3, 0x28])]
#[case::invalid_seq_identifier(vec![0xa0, 0xa1])]
#[case::invalid_2nd_of_3_octets(vec![0xe2, 0x28, 0xa1])]
#[case::invalid_3rd_of_3_octets(vec![0xe2, 0x82, 0x28])]
#[case::invalid_2nd_of_4_octets(vec![0xf0, 0x28, 0x8c, 0xbc])]
#[case::invalid_3rd_of_4_octets(vec![0xf0, 0x90, 0x28, 0xbc])]
#[case::invalid_4th_of_4_octets(vec![0xf0, 0x28, 0x8c, 0x28])]
fn sqpath_os_string_invalid_utf8(#[case] bytes: Vec<u8>) {
    let filename = PathBuf::from(OsString::from_vec(bytes));
    let temp_dir = TempDir::new().unwrap();
    fs::write(temp_dir.path().join(filename), "").unwrap();
    let query = format!(
        "<path(\"{}\").<children[0].<os_string.<string(replace_invalid=true)",
        temp_dir.path().to_str().unwrap()
    );
    assert!(get_query(&query).is_ok());
}

test_simple_query_ok!(
    sqpath_parent,
    tar, r#"<path("/home/user1/file.tar").<parent"#, json!("/home/user1");
    tar_gz, r#"<path("home/user1/file.tar.gz").<parent"#, json!("home/user1");
    with_hidden_filename, r#"<path("/home/user1/.file").<parent"#, json!("/home/user1");
    usr_bin_sq, r#"<path("usr/bin/sq").<parent"#, json!("usr/bin");
    with_slash_suffix, r#"<path("/usr/bin/").<parent"#, json!("/usr");
    slash, r#"<path("/").<parent"#, json!(Option::<()>::None);
);

test_simple_query_ok!(
    sqpath_filename,
    tar, r#"<path("/home/user1/file.tar").<filename"#, json!("file.tar");
    tar_gz, r#"<path("home/user1/file.tar.gz").<filename"#, json!("file.tar.gz");
    hidden_filename, r#"<path("/home/user1/.file").<filename"#, json!(".file");
    usr_bin_sq, r#"<path("usr/bin/sq").<filename"#, json!("sq");
    with_slash_suffix, r#"<path("/usr/bin/").<filename"#, json!("bin");
    slash, r#"<path("/").<filename"#, json!(Option::<()>::None);
);

test_simple_query_ok!(
    sqpath_stem,
    tar, r#"<path("/home/user1/file.tar").<stem"#, json!("file");
    tar_gz, r#"<path("home/user1/file.tar.gz").<stem"#, json!("file.tar");
    hidden_stem, r#"<path("/home/user1/.file").<stem"#, json!(".file");
    usr_bin_sq, r#"<path("usr/bin/sq").<stem"#, json!("sq");
    with_slash_suffix, r#"<path("/usr/bin/").<stem"#, json!("bin");
    slash, r#"<path("/").<stem"#, json!(Option::<()>::None);
);

#[test]
fn sqpath_exists_true() {
    let temp_files = TempFiles::new().file("a_filename");
    let query = format!("<path(\"{}\").<exists", temp_files[0].to_owned());
    test_query_ok(&query, json!(true));
}

#[test]
fn sqpath_exists_false() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("a_filename");
    let query = format!("<path(\"{}\").<exists", file_path.to_str().unwrap());
    test_query_ok(&query, json!(false));
}

test_simple_query_ok!(
    sqpath_extension,
    tar, r#"<path("/home/user1/file.tar").<extension"#, json!("tar");
    tar_gz, r#"<path("home/user1/file.tar.gz").<extension"#, json!("gz");
    hidden_extension, r#"<path("/home/user1/.file").<extension"#, json!(Option::<()>::None);
    usr_bin_sq, r#"<path("usr/bin/sq").<extension"#, json!(Option::<()>::None);
    with_slash_suffix, r#"<path("/usr/bin/").<extension"#, json!(Option::<()>::None);
    slash, r#"<path("/").<extension"#, json!(Option::<()>::None);
);

#[test]
fn sqpath_children_none() {
    let temp_files = TempFiles::new();
    let query = format!("<path(\"{}\").<children", temp_files.temp_dir());
    test_query_ok(&query, json!([]));
}

#[test]
fn sqpath_children_some() {
    let temp_files = TempFiles::new().file("file1").file("file2");
    let query = format!("<path(\"{}\").<children", temp_files.temp_dir());

    assert_eq!(
        get_query_as_sorted_vec::<String>(&query),
        vec![temp_files[0].to_owned(), temp_files[1].to_owned()],
    );
}

#[test]
fn sqpath_children_hidden() {
    let temp_files = TempFiles::new().file(".file1");
    let query = format!("<path(\"{}\").<children", temp_files.temp_dir());
    assert_eq!(
        get_query_as_sorted_vec::<String>(&query),
        vec![temp_files[0].to_owned()]
    );
}

#[test]
fn sqpath_children_dir() {
    let temp_files = TempFiles::new().dir("dir1");
    let query = format!("<path(\"{}\").<children", temp_files.temp_dir());

    assert_eq!(
        get_query_as_sorted_vec::<String>(&query),
        vec![temp_files[0].to_owned()]
    );
}

#[test]
fn sqpath_children_default_recurse() {
    let temp_files = TempFiles::new().dir("dir1").file("dir1/file1");
    let query = format!("<path(\"{}\").<children", temp_files.temp_dir());

    assert_eq!(
        get_query_as_sorted_vec::<String>(&query),
        vec![temp_files[0].to_owned()]
    );
}

#[test]
fn sqpath_children_recurse_false() {
    let temp_files = TempFiles::new().dir("dir1").file("dir1/file1");
    let query = format!(
        "<path(\"{}\").<children(recurse=false)",
        temp_files.temp_dir()
    );

    assert_eq!(
        get_query_as_sorted_vec::<String>(&query),
        vec![temp_files[0].to_owned()]
    );
}

#[test]
fn sqpath_children_recurse_true() {
    let temp_files = TempFiles::new().dir("dir1").file("dir1/file1");
    let query = format!(
        "<path(\"{}\").<children(recurse=true)",
        temp_files.temp_dir()
    );

    assert_eq!(
        get_query_as_sorted_vec::<String>(&query),
        vec![temp_files[0].to_owned(), temp_files[1].to_owned()],
    );
}

#[test]
fn sqpath_children_default_follow_symlinks() {
    let temp_files = TempFiles::new()
        .dir("dir1")
        .file("dir1/file1")
        .dir("dir2")
        .symlink("dir1", "dir2/symlink1");
    let query = format!("<path(\"{}\").<children(recurse=true)", &temp_files[2]);

    assert_eq!(
        get_query_as_sorted_vec::<String>(&query),
        vec![
            temp_files.abs_path_string("dir2/symlink1"),
            temp_files.abs_path_string("dir2/symlink1/file1"),
        ],
    );
}

#[test]
fn sqpath_children_follow_symlinks_true() {
    let temp_files = TempFiles::new()
        .dir("dir1")
        .file("dir1/file1")
        .dir("dir2")
        .symlink("dir1", "dir2/symlink1");
    let query = format!(
        "<path(\"{}\").<children(recurse=true, follow_symlinks=true)",
        &temp_files[2]
    );

    assert_eq!(
        get_query_as_sorted_vec::<String>(&query),
        vec![
            temp_files.abs_path_string("dir2/symlink1"),
            temp_files.abs_path_string("dir2/symlink1/file1"),
        ],
    );
}

#[test]
fn sqpath_children_follow_symlinks_false() {
    let temp_files = TempFiles::new()
        .dir("dir1")
        .file("dir1/file1")
        .dir("dir2")
        .symlink("dir1", "dir2/symlink1");
    let query = format!(
        "<path(\"{}\").<children(recurse=true, follow_symlinks=false)",
        &temp_files[2]
    );

    assert_eq!(
        get_query_as_sorted_vec::<String>(&query),
        vec![temp_files.abs_path_string("dir2/symlink1")],
    );
}

#[test]
fn sqpath_children_skip_default_permission_denied() {
    let temp_files = TempFiles::new()
        .dir("dir1")
        .file("dir1/file1")
        .chmod("dir1", 0222);
    let query = format!("<path(\"{}\").<children", &temp_files[0]);

    test_query_err(&query, ErrorKind::System);

    temp_files.chmod("dir1", 0777);
}

#[test]
fn sqpath_children_skip_default_permission_true() {
    let temp_files = TempFiles::new()
        .dir("dir1")
        .file("dir1/file1")
        .chmod("dir1", 0222);
    let query = format!(
        "<path(\"{}\").<children(skip_permission_denied=true)",
        &temp_files[0]
    );

    assert_eq!(
        get_query_as_sorted_vec::<String>(&query),
        Vec::<String>::new()
    );

    temp_files.chmod("dir1", 0777);
}

#[test]
fn sqpath_children_skip_default_permission_false() {
    let temp_files = TempFiles::new()
        .dir("dir1")
        .file("dir1/file1")
        .chmod("dir1", 0222);
    let query = format!(
        "<path(\"{}\").<children(skip_permission_denied=false)",
        &temp_files[0]
    );

    test_query_err(&query, ErrorKind::System);

    temp_files.chmod("dir1", 0777);
}

// TODO: tests SqPath::children same_filesystem param?

test_simple_query_ok!(
    sqpath_parts,
    tar, r#"<path("/home/user1/file.tar").<parts"#, json!(["/", "home", "user1", "file.tar"]);
    tar_gz, r#"<path("home/user1/file.tar.gz").<parts"#, json!(["home", "user1", "file.tar.gz"]);
    with_hidden_filename, r#"<path("/home/user1/.file").<parts"#, json!(["/", "home", "user1", ".file"]);
    usr_bin_sq, r#"<path("usr/bin/sq").<parts"#, json!(["usr", "bin", "sq"]);
    with_slash_suffix, r#"<path("/usr/bin/").<parts"#, json!(["/", "usr", "bin"]);
    slash, r#"<path("/").<parts"#, json!(["/"]);
);

test_simple_query_ok!(
    sqpath_absolute_already_absolute,
    tar, r#"<path("/home/user1/file.tar").<absolute"#, json!("/home/user1/file.tar");
    tar_gz, r#"<path("/home/user1/file.tar.gz").<absolute"#, json!("/home/user1/file.tar.gz");
    with_hidden_filename, r#"<path("/home/user1/.file").<absolute"#, json!("/home/user1/.file");
    usr_bin_sq, r#"<path("/usr/bin/sq").<absolute"#, json!("/usr/bin/sq");
    with_slash_suffix, r#"<path("/usr/bin/").<absolute"#, json!("/usr/bin/");
    slash, r#"<path("/").<absolute"#, json!("/");
);

#[rstest]
#[case::normal("home/user1/file.tar")]
#[case::dot_slash_prefix("./home/user1/file.tar.gz")]
#[case::dot_dot_slash_prefix("../home/user1/.file")]
#[case::just_dot(".")]
#[case::just_dot_dot("..")]
fn sqpath_absolute_from_relative(#[case] rel_path: &str) {
    let curr_dir_str = std::env::current_dir()
        .unwrap()
        .to_str()
        .unwrap()
        .to_owned();
    let query = format!("<path(\"{}\").<absolute", rel_path);
    let got = get_query_as::<String>(&query);
    let expected = format!("{}/{}", curr_dir_str, rel_path);
    assert_eq!(got, expected);
}

#[rstest]
#[case::normal("file1")]
#[case::with_dot("./file1")]
#[case::with_dot_dot("dir1/../file1")]
fn sqpath_canonical_already_absolute(#[case] tail: &str) {
    let temp_files = TempFiles::new().dir("dir1").file("file1");
    let query = format!("<path(\"{}/{}\").<canonical", temp_files.temp_dir(), tail);
    let got = get_query_as::<String>(&query);
    let expected = format!("{}/{}", temp_files.temp_dir(), "file1");
    assert_eq!(got, expected);
}

#[test]
fn sqpath_canonical_with_symlink() {
    let temp_files = TempFiles::new()
        .dir("dir1")
        .file("dir1/file1")
        .symlink("dir1", "dir2");

    let query = format!("<path(\"{}/dir2/file1\").<canonical", temp_files.temp_dir());
    let got = get_query_as::<String>(&query);
    let expected = format!("{}/dir1/file1", temp_files.temp_dir());
    assert_eq!(got, expected);
}

test_simple_query_ok!(
    sqpath_is_absolute,
    normal_abs, r#"<path("/home/user1/file.tar").<is_absolute"#, json!(true);
    normal_rel, r#"<path("home/user1/file.tar").<is_absolute"#, json!(false);
    with_dot_abs, r#"<path("/home/./user1/file.tar").<is_absolute"#, json!(true);
    with_dot_rel, r#"<path("home/./user1/file.tar").<is_absolute"#, json!(false);
    with_dot_dot_abs, r#"<path("/home/../user1/file.tar").<is_absolute"#, json!(true);
    with_dot_dot_rel, r#"<path("home/../user1/file.tar").<is_absolute"#, json!(false);
    slash, r#"<path("/").<is_absolute"#, json!(true);
);

#[test]
fn sqpath_file_default_follow_symlinks() {
    let temp_files = TempFiles::new().file("file1").symlink("file1", "file2");

    let query1 = format!("<path(\"{}/file1\").<file.<type", temp_files.temp_dir());
    assert_eq!(get_query_as::<String>(&query1), "file");

    let query2 = format!("<path(\"{}/file2\").<file.<type", temp_files.temp_dir());
    assert_eq!(get_query_as::<String>(&query2), "file");
}

#[test]
fn sqpath_file_follow_symlinks_true() {
    let temp_files = TempFiles::new().file("file1").symlink("file1", "file2");

    let query1 = format!(
        "<path(\"{}/file1\").<file(follow_symlinks=true).<type",
        temp_files.temp_dir()
    );
    assert_eq!(get_query_as::<String>(&query1), "file");

    let query2 = format!(
        "<path(\"{}/file2\").<file(follow_symlinks=true).<type",
        temp_files.temp_dir()
    );
    assert_eq!(get_query_as::<String>(&query2), "file");
}

#[test]
fn sqpath_file_follow_symlinks_false() {
    let temp_files = TempFiles::new().file("file1").symlink("file1", "file2");

    let query1 = format!(
        "<path(\"{}/file1\").<file(follow_symlinks=true).<type",
        temp_files.temp_dir()
    );
    assert_eq!(get_query_as::<String>(&query1), "file");

    let query2 = format!(
        "<path(\"{}/file2\").<file(follow_symlinks=false).<type",
        temp_files.temp_dir()
    );
    assert_eq!(get_query_as::<String>(&query2), "symlink");
}
