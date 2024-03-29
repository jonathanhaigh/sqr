// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

use pretty_assertions::assert_eq;
use serde_json::Value as JsonValue;

use sqr::error::{ErrorKind, Result};
use sqr::parser::Parser;
use sqr::{lexer, results, system};

// Rust doesn't seem to see that this struct is actually used.
#[allow(dead_code)]
pub struct TempFiles {
    temp_dir: tempfile::TempDir,
    paths: Vec<std::path::PathBuf>,
}

impl TempFiles {
    // Rust doesn't seem to see that this function is actually used.
    #[allow(dead_code)]
    pub fn new() -> Self {
        Self {
            temp_dir: tempfile::TempDir::new().unwrap(),
            paths: Vec::new(),
        }
    }

    // Rust doesn't seem to see that this function is actually used.
    #[allow(dead_code)]
    pub fn temp_dir(&self) -> String {
        self.temp_dir
            .path()
            .canonicalize()
            .unwrap()
            .to_str()
            .unwrap()
            .to_owned()
    }

    // Rust doesn't seem to see that this function is actually used.
    #[allow(dead_code)]
    pub fn abs_path(&self, rel_path: &str) -> std::path::PathBuf {
        self.temp_dir.path().join(rel_path)
    }

    // Rust doesn't seem to see that this function is actually used.
    #[allow(dead_code)]
    pub fn abs_path_string(&self, rel_path: &str) -> String {
        self.abs_path(rel_path).to_str().unwrap().to_owned()
    }

    // Rust doesn't seem to see that this function is actually used.
    #[allow(dead_code)]
    pub fn file(mut self, rel_path: &str) -> Self {
        let pathbuf = self.abs_path(rel_path);
        std::fs::write(&pathbuf, "").unwrap();
        self.paths.push(pathbuf);
        self
    }

    // Rust doesn't seem to see that this function is actually used.
    #[allow(dead_code)]
    pub fn dir(mut self, rel_path: &str) -> Self {
        let pathbuf = self.abs_path(rel_path);
        std::fs::create_dir(&pathbuf).unwrap();
        self.paths.push(pathbuf);
        self
    }

    // Rust doesn't seem to see that this function is actually used.
    #[allow(dead_code)]
    pub fn symlink(mut self, rel_target_path: &str, rel_path: &str) -> Self {
        let target_path = self.abs_path(rel_target_path);
        let path = self.abs_path(rel_path);
        std::os::unix::fs::symlink(&target_path, &path).unwrap();
        self.paths.push(path);
        self
    }

    // Rust doesn't seem to see that this function is actually used.
    #[allow(dead_code)]
    pub fn link(mut self, rel_target_path: &str, rel_path: &str) -> Self {
        let target_path = self.abs_path(rel_target_path);
        let path = self.abs_path(rel_path);
        std::fs::hard_link(&target_path, &path).unwrap();
        self.paths.push(path);
        self
    }

    // Rust doesn't seem to see that this function is actually used.
    #[allow(dead_code)]
    pub fn fifo(mut self, rel_path: &str, mode: u32) -> Self {
        let path = self.abs_path(rel_path);
        nix::unistd::mkfifo(&path, nix::sys::stat::Mode::from_bits(mode).unwrap()).unwrap();
        self.paths.push(path);
        self
    }

    // Rust doesn't seem to see that this function is actually used.
    #[allow(dead_code)]
    pub fn unix_socket(mut self, rel_path: &str) -> Self {
        use nix::sys::socket::{bind, socket, AddressFamily, SockFlag, SockType, UnixAddr};
        let path = self.abs_path(rel_path);
        let sock = socket(AddressFamily::Unix, SockType::Raw, SockFlag::empty(), None).unwrap();
        let sock_addr = UnixAddr::new(&path).unwrap();
        bind(sock, &sock_addr).unwrap();
        self.paths.push(path);
        self
    }

    // Rust doesn't seem to see that this function is actually used.
    #[allow(dead_code)]
    pub fn truncate(self, rel_path: &str, len: i64) -> Self {
        let path = self.abs_path(rel_path);
        nix::unistd::truncate(&path, len).unwrap();
        self
    }

    // Rust doesn't seem to see that this function is actually used.
    #[allow(dead_code)]
    pub fn chmod(self, rel_path: &str, mode: u32) -> Self {
        use std::os::unix::fs::PermissionsExt;
        let path = self.abs_path(rel_path);
        let mut perms = std::fs::metadata(&path).unwrap().permissions();
        perms.set_mode(mode);
        std::fs::set_permissions(&path, perms).unwrap();
        self
    }

    // Rust doesn't seem to see that this function is actually used.
    #[allow(dead_code)]
    pub fn utimes(self, rel_path: &str, atime: (i64, i64), mtime: (i64, i64)) -> Self {
        use nix::sys::time::TimeVal;
        let path = self.abs_path(rel_path);
        nix::sys::stat::utimes(
            &path,
            &TimeVal::new(atime.0, atime.1),
            &TimeVal::new(mtime.0, mtime.1),
        )
        .unwrap();
        self
    }
}

impl std::ops::Index<usize> for TempFiles {
    type Output = str;

    fn index(&self, index: usize) -> &Self::Output {
        self.paths[index].to_str().unwrap()
    }
}

pub fn get_query(query: &str) -> Result<String> {
    let tokens = lexer::lex(query)?;

    let mut p = Parser::new(query, &tokens);
    let ast = p.parse()?;

    let buff = Vec::new();
    let mut serializer = serde_json::Serializer::pretty(buff);
    results::generate_results(&ast, &mut serializer, system::root())?;
    Ok(String::from_utf8(serializer.into_inner()).unwrap())
}

// Rust doesn't seem to see that this function is actually used.
#[allow(dead_code)]
pub fn get_query_as<T>(query: &str) -> T
where
    T: for<'a> serde::de::Deserialize<'a>,
{
    serde_json::from_str::<T>(&get_query(query).unwrap()).unwrap()
}

// Rust doesn't seem to see that this function is actually used.
#[allow(dead_code)]
pub fn get_query_as_sorted_vec<T>(query: &str) -> Vec<T>
where
    Vec<T>: for<'a> serde::de::Deserialize<'a>,
    T: Ord,
{
    let mut results = get_query_as::<Vec<T>>(query);
    results.sort();
    results
}

// Rust doesn't seem to see that this function is actually used.
#[allow(dead_code)]
pub fn test_query_ok(query: &str, expected: JsonValue) {
    assert_eq!(
        serde_json::from_str::<JsonValue>(get_query(query).unwrap().as_str()).unwrap(),
        expected,
    );
}

// Rust doesn't seem to see that this macro is actually used.
#[allow(unused_macros)]
macro_rules! test_simple_query_ok {
    ($name:ident, $($case_name:ident, $query:expr, $expected:expr;)*) => {
        // Put the test function in a new module to avoid "use" statements polluting the caller's
        // namespace
        mod $name {
            use rstest::rstest;
            use serde_json::Value as JsonValue;
            use serde_json::json;
            #[rstest]
            $(#[case::$case_name($query, $expected)])*
            fn test_query_ok(#[case] query: &str, #[case] expected: JsonValue) {
                $crate::integration_test_util::test_query_ok(query, expected);
            }
        }
    }
}
#[allow(unused_imports)]
pub(crate) use test_simple_query_ok;

// Rust doesn't seem to see that this macro is actually used.
#[allow(unused_macros)]
macro_rules! test_simple_query_approx_f64_ulp {
    ($name:ident, $($case_name:ident, $query:expr, $expected:expr, $max_ulps:expr;)*) => {
        mod $name {
            use rstest::rstest;
            #[rstest]
            $(#[case::$case_name($query, $expected, $max_ulps)])*
            fn test_query_approx_f64_ulp(#[case] query: &str, #[case] expected: f64, #[case] max_ulps: u32) {
                let result = $crate::integration_test_util::get_query_as::<f64>(query);
                approx::assert_ulps_eq!(result, expected, max_ulps=max_ulps);
            }
        }
    }
}
#[allow(unused_imports)]
pub(crate) use test_simple_query_approx_f64_ulp;

// Rust doesn't seem to see that this macro is actually used.
#[allow(unused_macros)]
macro_rules! test_simple_query_approx_f64_abs_diff {
    ($name:ident, $($case_name:ident, $query:expr, $expected:expr, $epsilon:expr;)*) => {
        mod $name {
            use rstest::rstest;
            #[rstest]
            $(#[case::$case_name($query, $expected, $epsilon)])*
            fn test_query_approx_f64_ulp(#[case] query: &str, #[case] expected: f64, #[case] epsilon: f64) {
                let result = $crate::integration_test_util::get_query_as::<f64>(query);
                approx::assert_abs_diff_eq!(result, expected, epsilon=epsilon);
            }
        }
    }
}
#[allow(unused_imports)]
pub(crate) use test_simple_query_approx_f64_abs_diff;

// Rust doesn't seem to see that this function is actually used.
#[allow(dead_code)]
pub fn test_query_err(query: &str, kind: ErrorKind) {
    assert_eq!(get_query(query).unwrap_err().kind(), kind);
}

// Rust doesn't seem to see that this macro is actually used.
#[allow(unused_macros)]
macro_rules! test_simple_query_err {
    ($name:ident, $($case_name:ident, $query:expr, $expected:ident;)*) => {
        // Put the test function in a new module to avoid "use" statements polluting the caller's
        // namespace
        mod $name {
            use rstest::rstest;
            use sqr::error::ErrorKind;
            #[rstest]
            $(#[case::$case_name($query, ErrorKind::$expected)])*
            fn test_query_err(#[case] query: &str, #[case] expected: ErrorKind) {
                $crate::integration_test_util::test_query_err(query, expected);
            }
        }
    }
}

#[allow(unused_imports)]
pub(crate) use test_simple_query_err;
