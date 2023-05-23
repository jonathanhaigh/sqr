// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::primitive::Primitive;
use crate::system::SqIntTrait;

pub struct SqInt {
    value: i128,
}

impl SqInt {
    pub fn new(value: i128) -> Self {
        Self { value }
    }
}

impl SqIntTrait for SqInt {
    fn to_primitive(&self) -> anyhow::Result<Primitive> {
        Ok(Primitive::I128(self.value))
    }
}

impl<T> From<T> for SqInt
where
    i128: From<T>,
{
    fn from(v: T) -> Self {
        Self::new(i128::from(v))
    }
}
