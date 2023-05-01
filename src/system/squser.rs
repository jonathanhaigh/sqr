// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::primitive::Primitive;
use crate::system::SqUserTrait;

pub struct SqUser {
    uid: u32,
}

impl SqUser {
    pub fn new(uid: u32) -> Self {
        Self { uid }
    }
}

impl SqUserTrait for SqUser {
    fn to_primitive(&self) -> anyhow::Result<Primitive> {
        Ok(Primitive::Int(i64::from(self.uid)))
    }
}
