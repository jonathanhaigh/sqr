// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::primitive::Primitive;
use crate::system::SqGroupTrait;

pub struct SqGroup {
    gid: u32,
}

impl SqGroup {
    pub fn new(gid: u32) -> Self {
        Self { gid }
    }
}

impl SqGroupTrait for SqGroup {
    fn to_primitive(&self) -> anyhow::Result<Primitive> {
        Ok(Primitive::Int(i64::from(self.gid)))
    }
}
