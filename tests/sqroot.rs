// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

use std::env::var as env_var;

use serde_json::json;

use integration_test_util::{
    get_query, test_query_err, test_query_ok, test_simple_query_approx_f64_ulp,
    test_simple_query_err, test_simple_query_ok,
};
use sqr::error::ErrorKind;

mod integration_test_util;

test_simple_query_ok!(
    sqroot_bool,
    default_value, "<bool", json!(false);
    bool_false, "<bool(false)", json!(false);
    bool_true, "<bool(true)", json!(true);
);

test_simple_query_ok!(
    sqroot_int,
    default_value, "<int", json!(0);
    one, "<int(1)", json!(1);
    minus_one, "<int(-1)", json!(-1);
    i64_max, "<int(9223372036854775807)", json!(9223372036854775807i64); // i64::MAX
    i64_min, "<int(-9223372036854775808)", json!(-9223372036854775808i64); // i64::MIN
);

test_simple_query_ok!(
    sqroot_ints,
    default_start, "<ints(stop=10, step=2)", json!([0, 2, 4, 6, 8]);
    default_stop, "<ints(start=10, step=2)[:5]", json!([10, 12, 14, 16, 18]);
    default_step, "<ints(start=10, stop=15)[:5]", json!([10, 11, 12, 13, 14]);
    default_all, "<ints[:5]", json!([0, 1, 2, 3, 4]);
);

test_simple_query_approx_f64_ulp!(
    sqroot_duration,
    secs, "<duration(s=10)", 10f64, 4;
    millis, "<duration(ms=10)", 10e-3f64, 4;
    micros, "<duration(us=10)", 10e-6f64, 4;
    nanos, "<duration(ns=10)", 10e-9f64, 4;
    all, "<duration(s=111, ms=222, us=333, ns=444)", 111.222333444f64, 4;
);

test_simple_query_err!(
    sqroot_duration_err,
    neg_secs, "duration(s=-1)", ConvertIntegerArg;
    neg_millis, "duration(ms=-1)", ConvertIntegerArg;
    neg_micros, "duration(us=-1)", ConvertIntegerArg;
    neg_nanos, "duration(ns=-1)", ConvertIntegerArg;
);

test_simple_query_ok!(
    sqroot_data_size,
    default_value, "<data_size", json!(0i64);
    one, "<data_size(1)", json!(1i64);
    i64_max, "<data_size(9223372036854775807)", json!(9_223_372_036_854_775_807i64);
);

test_simple_query_err!(
    sqroot_data_size_err,
    minus_one, "<data_size(-1)", ConvertIntegerArg;
    i64_min, "<data_size(-9223372036854775808)", ConvertIntegerArg;
);

test_simple_query_ok!(
    sqroot_string,
    default_value, "<string", json!("");
    all_unicode_planes,
        "<string(\"\u{B123}\u{1B123}\u{2B123}\u{30123}\u{E0021}\u{F0001}\")",
        json!("\u{B123}\u{1B123}\u{2B123}\u{30123}\u{E0021}\u{F0001}");
    escape_quote, r#"<string("\"")"#, json!("\"");
    escape_backslash, r#"<string("\\")"#, json!("\\");
    escape_forwardslash, r#"<string("\/")"#, json!("/");
    escape_backspace, r#"<string("\b")"#, json!("\u{8}");
    escape_formfeed, r#"<string("\f")"#, json!("\u{C}");
    escape_linefeed, r#"<string("\n")"#, json!("\n");
    escape_carriagereturn, r#"<string("\r")"#, json!("\r");
    escape_tab, r#"<string("\t")"#, json!("\t");
    escape_unicode, r#"<string("\u1234")"#, json!("\u{1234}");
);

test_simple_query_ok!(
    sqroot_float,
    default_value, "<float", json!(0.0f64);
    zero, "<float(0.0)", json!(0.0f64);
    one, "<float(1.0)", json!(1.0f64);
    minus_one, "<float(-1.0)", json!(-1.0f64);
    f64_max, "<float(1.7976931348623157E+308)", json!(1.7976931348623157E+308f64);
    f64_min, "<float(-1.7976931348623157E+308)", json!(-1.7976931348623157E+308f64);
    small_e, "<float(1.0e10)", json!(1.0e10f64);
    big_e, "<float(1.0E10)", json!(1.0E10f64);
    pos_e, "<float(1.0E+10)", json!(1.0E+10f64);
    neg_e, "<float(1.0E-10)", json!(1.0E-10f64);
);

test_simple_query_ok!(
    sqroot_path,
    default_value, "<path", json!(std::env::current_dir().unwrap().to_str().unwrap());
    slash, r#"<path("/")"#, json!("/");
    absolute, r#"<path("/abc/def")"#, json!("/abc/def");
    relative, r#"<path("ghi/jkl")"#, json!("ghi/jkl");
);

#[test]
fn sqroot_user_default() {
    test_query_ok("<user.<username", json!(env_var("USER").unwrap()));
}

#[test]
fn sqroot_user_by_name() {
    let query = format!("<user(username=\"{}\").<username", env_var("USER").unwrap());
    test_query_ok(&query, json!(env_var("USER").unwrap()));
}

#[test]
fn sqroot_user_by_uid() {
    let uid_json = get_query("<user.<uid").unwrap();
    let query = format!("<user(uid={}).<username", uid_json);
    test_query_ok(&query, json!(env_var("USER").unwrap()));
}

test_simple_query_ok!(
    sqroot_user,
    root_by_name, "<user(username=\"root\").<uid", json!(0);
    root_by_uid, "<user(uid=0).<username", json!("root");
);

test_simple_query_err!(
    sqroot_user_err,
    invalid_uid, "user(uid=4294967296)", ConvertIntegerArg; // u32::MAX + 1
);

#[test]
fn sqroot_user_both_uid_and_name_err() {
    let uid_json = get_query("<user.<uid").unwrap();
    let query = format!(
        "user(uid={}, username=\"{}\")",
        uid_json,
        env_var("USER").unwrap()
    );
    test_query_err(&query, ErrorKind::System);
}

#[test]
fn sqroot_group_default() {
    use serde_json::{from_str, Value};
    let group_name_json = get_query("<user.<group.<name").unwrap();
    test_query_ok("<group.<name", from_str::<Value>(&group_name_json).unwrap());
}

#[test]
fn sqroot_group_by_name() {
    use serde_json::{from_str, Value};
    let group_name_json = get_query("<user.<group.<name").unwrap();
    let query = format!("<group(name={}).<name", group_name_json);
    test_query_ok(&query, from_str::<Value>(&group_name_json).unwrap());
}

#[test]
fn sqroot_group_by_gid() {
    use serde_json::{from_str, Value};
    let gid_json = get_query("<user.<group.<gid").unwrap();
    let query = format!("<group(gid={}).<gid", gid_json);
    test_query_ok(&query, from_str::<Value>(&gid_json).unwrap());
}

#[test]
fn sqroot_group_by_both_gid_and_name_err() {
    let gid_json = get_query("<user.<group.<gid").unwrap();
    let name_json = get_query("<user.<group.<name").unwrap();
    let query = format!("<group(gid={}, group_name={})", gid_json, name_json);
    test_query_err(&query, ErrorKind::System);
}

test_simple_query_ok!(
    sqroot_group,
    root_by_name, "<group(group_name=\"root\").<gid", json!(0);
    root_by_gid, "<group(gid=0).<name", json!("root");
);

test_simple_query_err!(
    sqroot_group_err,
    invalid_gid, "group(gid=4294967296)", ConvertIntegerArg; // u32::MAX + 1
);
