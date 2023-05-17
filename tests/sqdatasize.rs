// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

use integration_test_util::{
    test_simple_query_approx_f64_abs_diff, test_simple_query_approx_f64_ulp, test_simple_query_ok,
};

mod integration_test_util;

test_simple_query_ok!(
    sqdatasize_b,
    zero, "<data_size(0).<B", json!(0);
    one, "<data_size(0).<B", json!(0);
    i64_max, "<data_size(9223372036854775807)", json!(9_223_372_036_854_775_807i64);
    // TODO: currently the only way to create an SqDataSize with a size bigger than i64::MAX bytes
    // is to read the size of a file that has such a size. Perhaps we could create a sparse file
    // like that or have another way to create such an SqDataSize.
);

test_simple_query_approx_f64_abs_diff!(
    sqdatasize_zeroes,
    kib, "<data_size(0).<KiB", 0f64, f64::EPSILON;
    mib, "<data_size(0).<MiB", 0f64, f64::EPSILON;
    gib, "<data_size(0).<GiB", 0f64, f64::EPSILON;
    tib, "<data_size(0).<TiB", 0f64, f64::EPSILON;
    pib, "<data_size(0).<PiB", 0f64, f64::EPSILON;
    kb, "<data_size(0).<kB", 0f64, f64::EPSILON;
    mb, "<data_size(0).<MB", 0f64, f64::EPSILON;
    gb, "<data_size(0).<GB", 0f64, f64::EPSILON;
    tb, "<data_size(0).<TB", 0f64, f64::EPSILON;
    pb, "<data_size(0).<PB", 0f64, f64::EPSILON;
);

test_simple_query_approx_f64_ulp!(
    sqdatasize_kib,
    one, "<data_size(1).<KiB", 0.0009765625f64, 4;
    ten, "<data_size(10).<KiB", 0.009765625f64, 4;
    hundred, "<data_size(100).<KiB", 0.09765625f64, 4;
    k, "<data_size(1000).<KiB", 0.9765625f64, 4;
    m, "<data_size(1000000).<KiB", 976.5625f64, 4;
    g, "<data_size(1000000000).<KiB", 976562.5f64, 4;
    t, "<data_size(1000000000000).<KiB", 976562500f64, 4;
    p, "<data_size(1000000000000000).<KiB", 976562500000f64, 4;
    ki, "<data_size(1024).<KiB", 1f64, 4;
    mi, "<data_size(1048576).<KiB", 1024f64, 4;
    gi, "<data_size(1073741824).<KiB", 1048576f64, 4;
    ti, "<data_size(1099511627776).<KiB", 1073741824f64, 4;
    pi, "<data_size(1125899906842624).<KiB", 1099511627776f64, 4;
);

test_simple_query_approx_f64_ulp!(
    sqdatasize_kb,
    one, "<data_size(1).<kB", 1e-3f64, 4;
    ten, "<data_size(10).<kB", 1e-2f64, 4;
    hundred, "<data_size(100).<kB", 1e-1f64, 4;
    k, "<data_size(1000).<kB", 1e0f64, 4;
    m, "<data_size(1000000).<kB", 1e3f64, 4;
    g, "<data_size(1000000000).<kB", 1e6f64, 4;
    t, "<data_size(1000000000000).<kB", 1e9f64, 4;
    p, "<data_size(1000000000000000).<kB", 1e12f64, 4;
    ki, "<data_size(1024).<kB", 1.024f64, 4;
    mi, "<data_size(1048576).<kB", 1048.576f64, 4;
    gi, "<data_size(1073741824).<kB", 1073741.824f64, 4;
    ti, "<data_size(1099511627776).<kB", 1099511627.776f64, 4;
    pi, "<data_size(1125899906842624).<kB", 1125899906842.624f64, 4;
);

test_simple_query_approx_f64_ulp!(
    sqdatasize_mib,
    one, "<data_size(1).<MiB", 9.5367431640625e-7f64, 4;
    ten, "<data_size(10).<MiB", 9.5367431640625e-6f64, 4;
    hundred, "<data_size(100).<MiB", 9.5367431640625e-5f64, 4;
    k, "<data_size(1000).<MiB", 9.5367431640625e-4f64, 4;
    m, "<data_size(1000000).<MiB", 9.5367431640625e-1f64, 4;
    g, "<data_size(1000000000).<MiB", 9.5367431640625e2f64, 4;
    t, "<data_size(1000000000000).<MiB", 9.5367431640625e5f64, 4;
    p, "<data_size(1000000000000000).<MiB", 9.5367431640625e8f64, 4;
    ki, "<data_size(1024).<MiB", 0.0009765625f64, 4;
    mi, "<data_size(1048576).<MiB", 1f64, 4;
    gi, "<data_size(1073741824).<MiB", 1024f64, 4;
    ti, "<data_size(1099511627776).<MiB", 1048576f64, 4;
    pi, "<data_size(1125899906842624).<MiB", 1073741824f64, 4;
);

test_simple_query_approx_f64_ulp!(
    sqdatasize_mb,
    one, "<data_size(1).<MB", 1e-6f64, 4;
    ten, "<data_size(10).<MB", 1e-5f64, 4;
    hundred, "<data_size(100).<MB", 1e-4f64, 4;
    k, "<data_size(1000).<MB", 1e-3f64, 4;
    m, "<data_size(1000000).<MB", 1e0f64, 4;
    g, "<data_size(1000000000).<MB", 1e3f64, 4;
    t, "<data_size(1000000000000).<MB", 1e6f64, 4;
    p, "<data_size(1000000000000000).<MB", 1e9f64, 4;
    ki, "<data_size(1024).<MB", 0.001024f64, 4;
    mi, "<data_size(1048576).<MB", 1.048576f64, 4;
    gi, "<data_size(1073741824).<MB", 1073.741824f64, 4;
    ti, "<data_size(1099511627776).<MB", 1099511.627776f64, 4;
    pi, "<data_size(1125899906842624).<MB", 1125899906.842624f64, 4;
);

test_simple_query_approx_f64_ulp!(
    sqdatasize_gib,
    one, "<data_size(1).<GiB", 9.31322574615478515625e-10f64, 4;
    ten, "<data_size(10).<GiB", 9.31322574615478515625e-9f64, 4;
    hundred, "<data_size(100).<GiB", 9.31322574615478515625e-8f64, 4;
    k, "<data_size(1000).<GiB", 9.31322574615478515625e-7f64, 4;
    m, "<data_size(1000000).<GiB", 9.31322574615478515625e-4f64, 4;
    g, "<data_size(1000000000).<GiB", 9.31322574615478515625e-1f64, 4;
    t, "<data_size(1000000000000).<GiB", 9.31322574615478515625e2f64, 4;
    p, "<data_size(1000000000000000).<GiB", 9.31322574615478515625e5f64, 4;
    ki, "<data_size(1024).<GiB", 9.5367431640625e-7f64, 4;
    mi, "<data_size(1048576).<GiB", 0.0009765625f64, 4;
    gi, "<data_size(1073741824).<GiB", 1f64, 4;
    ti, "<data_size(1099511627776).<GiB", 1024f64, 4;
    pi, "<data_size(1125899906842624).<GiB", 1048576f64, 4;
);

test_simple_query_approx_f64_ulp!(
    sqdatasize_gb,
    one, "<data_size(1).<GB", 1e-9f64, 4;
    ten, "<data_size(10).<GB", 1e-8f64, 4;
    hundred, "<data_size(100).<GB", 1e-7f64, 4;
    k, "<data_size(1000).<GB", 1e-6f64, 4;
    m, "<data_size(1000000).<GB", 1e-3f64, 4;
    g, "<data_size(1000000000).<GB", 1e0f64, 4;
    t, "<data_size(1000000000000).<GB", 1e3f64, 4;
    p, "<data_size(1000000000000000).<GB", 1e6f64, 4;
    ki, "<data_size(1024).<GB", 0.000001024f64, 4;
    mi, "<data_size(1048576).<GB", 0.001048576f64, 4;
    gi, "<data_size(1073741824).<GB", 1.073741824f64, 4;
    ti, "<data_size(1099511627776).<GB", 1099.511627776f64, 4;
    pi, "<data_size(1125899906842624).<GB", 1125899.906842624f64, 4;
);

test_simple_query_approx_f64_ulp!(
    sqdatasize_tib,
    one, "<data_size(1).<TiB", 9.094947017729282379150390625e-13f64, 4;
    ten, "<data_size(10).<TiB", 9.094947017729282379150390625e-12f64, 4;
    hundred, "<data_size(100).<TiB", 9.094947017729282379150390625e-11f64, 4;
    k, "<data_size(1000).<TiB", 9.094947017729282379150390625e-10f64, 4;
    m, "<data_size(1000000).<TiB", 9.094947017729282379150390625e-7f64, 4;
    g, "<data_size(1000000000).<TiB", 9.094947017729282379150390625e-4f64, 4;
    t, "<data_size(1000000000000).<TiB", 9.094947017729282379150390625e-1f64, 4;
    p, "<data_size(1000000000000000).<TiB", 9.094947017729282379150390625e2f64, 4;
    ki, "<data_size(1024).<TiB", 9.31322574615478515625E-10, 4;
    mi, "<data_size(1048576).<TiB", 9.5367431640625e-7f64, 4;
    gi, "<data_size(1073741824).<TiB", 0.0009765625f64, 4;
    ti, "<data_size(1099511627776).<TiB", 1f64, 4;
    pi, "<data_size(1125899906842624).<TiB", 1024f64, 4;
);

test_simple_query_approx_f64_ulp!(
    sqdatasize_tb,
    one, "<data_size(1).<TB", 1e-12f64, 4;
    ten, "<data_size(10).<TB", 1e-11f64, 4;
    hundred, "<data_size(100).<TB", 1e-10f64, 4;
    k, "<data_size(1000).<TB", 1e-9f64, 4;
    m, "<data_size(1000000).<TB", 1e-6f64, 4;
    g, "<data_size(1000000000).<TB", 1e-3f64, 4;
    t, "<data_size(1000000000000).<TB", 1e0f64, 4;
    p, "<data_size(1000000000000000).<TB", 1e3f64, 4;
    ki, "<data_size(1024).<TB", 0.000000001024f64, 4;
    mi, "<data_size(1048576).<TB", 0.000001048576f64, 4;
    gi, "<data_size(1073741824).<TB", 0.001073741824f64, 4;
    ti, "<data_size(1099511627776).<TB", 1.099511627776f64, 4;
    pi, "<data_size(1125899906842624).<TB", 1125.899906842624f64, 4;
);

test_simple_query_approx_f64_ulp!(
    sqdatasize_pib,
    one, "<data_size(1).<PiB", 8.8817841970012523233890533447265625e-16f64, 4;
    ten, "<data_size(10).<PiB", 8.8817841970012523233890533447265625e-15f64, 4;
    hundred, "<data_size(100).<PiB", 8.8817841970012523233890533447265625e-14f64, 4;
    k, "<data_size(1000).<PiB", 8.8817841970012523233890533447265625e-13f64, 4;
    m, "<data_size(1000000).<PiB", 8.8817841970012523233890533447265625e-10f64, 4;
    g, "<data_size(1000000000).<PiB", 8.8817841970012523233890533447265625e-7f64, 4;
    t, "<data_size(1000000000000).<PiB", 8.8817841970012523233890533447265625e-4f64, 4;
    p, "<data_size(1000000000000000).<PiB", 8.8817841970012523233890533447265625e-1f64, 4;
    ki, "<data_size(1024).<PiB", 9.094947017729282379150390625e-13f64, 4;
    mi, "<data_size(1048576).<PiB", 9.31322574615478515625E-10, 4;
    gi, "<data_size(1073741824).<PiB", 9.5367431640625e-7f64, 4;
    ti, "<data_size(1099511627776).<PiB", 0.0009765625f64, 4;
    pi, "<data_size(1125899906842624).<PiB", 1f64, 4;
);

test_simple_query_approx_f64_ulp!(
    sqdatasize_pb,
    one, "<data_size(1).<PB", 1e-15f64, 4;
    ten, "<data_size(10).<PB", 1e-14f64, 4;
    hundred, "<data_size(100).<PB", 1e-13f64, 4;
    k, "<data_size(1000).<PB", 1e-12f64, 4;
    m, "<data_size(1000000).<PB", 1e-9f64, 4;
    g, "<data_size(1000000000).<PB", 1e-6f64, 4;
    t, "<data_size(1000000000000).<PB", 1e-3f64, 4;
    p, "<data_size(1000000000000000).<PB", 1e0f64, 4;
    ki, "<data_size(1024).<PB", 0.000000000001024f64, 4;
    mi, "<data_size(1048576).<PB", 0.000000001048576f64, 4;
    gi, "<data_size(1073741824).<PB", 0.000001073741824f64, 4;
    ti, "<data_size(1099511627776).<PB", 0.001099511627776f64, 4;
    pi, "<data_size(1125899906842624).<PB", 1.125899906842624f64, 4;
);
