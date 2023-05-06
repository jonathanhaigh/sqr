// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

//! Utilities for tests.

pub fn none_on_32_bit_arch<T>(v: T) -> Option<T> {
    if cfg!(target_pointer_width = "32") {
        None
    } else {
        Some(v)
    }
}

pub fn none_on_64_bit_arch<T>(v: T) -> Option<T> {
    if cfg!(target_pointer_width = "64") {
        None
    } else {
        Some(v)
    }
}
