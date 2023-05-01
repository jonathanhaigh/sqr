// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

use nix::sys::stat::Mode;

use crate::primitive::Primitive;
use crate::system::{sqbool::SqBool, sqint::SqInt, SqFileModeTrait};

pub struct SqFileMode {
    mode: Mode,
}

impl SqFileMode {
    pub fn new(mode: Mode) -> Self {
        Self { mode }
    }
}

impl SqFileModeTrait for SqFileMode {
    fn to_primitive(&self) -> anyhow::Result<Primitive> {
        Ok(Primitive::Int(i64::from(self.mode.bits())))
    }

    fn permissions(&self) -> anyhow::Result<SqInt> {
        let num = self
            .mode
            .intersection(Mode::S_IRWXU | Mode::S_IRWXG | Mode::S_IRWXO)
            .bits();
        Ok(SqInt::new(i64::from(num)))
    }

    fn suid(&self) -> anyhow::Result<SqBool> {
        Ok(SqBool::new(self.mode.contains(Mode::S_ISUID)))
    }

    fn sgid(&self) -> anyhow::Result<SqBool> {
        Ok(SqBool::new(self.mode.contains(Mode::S_ISGID)))
    }

    fn sticky(&self) -> anyhow::Result<SqBool> {
        Ok(SqBool::new(self.mode.contains(Mode::S_ISVTX)))
    }
}
