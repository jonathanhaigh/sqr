// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::primitive::Primitive;
use crate::system::SqIntTrait;

pub struct SqInt {
    value: i64,
}

impl SqInt {
    pub fn new(value: i64) -> Self {
        Self { value }
    }
}

impl SqIntTrait for SqInt {
    fn to_primitive(&self) -> anyhow::Result<Primitive> {
        Ok(Primitive::Int(self.value))
    }
}
