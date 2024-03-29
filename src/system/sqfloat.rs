// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::primitive::Primitive;
use crate::system::SqFloatTrait;

pub struct SqFloat {
    value: f64,
}

impl SqFloat {
    pub fn new(value: f64) -> Self {
        Self { value }
    }
}

impl SqFloatTrait for SqFloat {
    fn to_primitive(&self) -> anyhow::Result<Primitive> {
        Ok(Primitive::F64(self.value))
    }
}
