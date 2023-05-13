// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

use std::ffi::OsString;

use anyhow::anyhow;

use crate::primitive::Primitive;
use crate::system::sqstring::SqString;
use crate::system::SqOsStringTrait;

pub struct SqOsString {
    value: OsString,
}

impl SqOsString {
    pub fn new(value: OsString) -> Self {
        Self { value }
    }

    pub fn to_str(&self) -> anyhow::Result<&str> {
        self.value
            .to_str()
            .ok_or(anyhow!("OS string is not valid UTF-8"))
    }
}

impl SqOsStringTrait for SqOsString {
    fn to_primitive(&self) -> anyhow::Result<Primitive> {
        Ok(Primitive::Str(self.to_str()?.to_owned()))
    }

    fn string(&self, replace_invalid: Option<bool>) -> anyhow::Result<SqString> {
        if replace_invalid.unwrap_or(false) {
            Ok(SqString::new(self.value.to_string_lossy().into_owned()))
        } else {
            Ok(SqString::new(self.to_str()?.to_owned()))
        }
    }
}
