// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

use anyhow::anyhow;
use nix::unistd;
use nix::unistd::{Gid, Group};

use crate::primitive::Primitive;
use crate::sqvalue::SqValueSequence;
use crate::system::{sqint::SqInt, sqstring::SqString, squser::SqUser, SqGroupTrait};

pub struct SqGroup {
    group: Group,
}

impl SqGroup {
    pub fn from_gid(gid: u32) -> anyhow::Result<Self> {
        match Group::from_gid(Gid::from_raw(gid)) {
            Ok(Some(group)) => Ok(Self { group }),
            Ok(None) => Err(anyhow!("Group with GID {} not found", gid)),
            Err(e) => Err(anyhow!("Error getting group from GID {}: {}", gid, e)),
        }
    }

    pub fn from_name(name: &str) -> anyhow::Result<Self> {
        match Group::from_name(name) {
            Ok(Some(group)) => Ok(Self { group }),
            Ok(None) => Err(anyhow!("Group with name {} not found", name)),
            Err(e) => Err(anyhow!("Error getting group from name {}: {}", name, e)),
        }
    }

    pub fn real() -> anyhow::Result<Self> {
        Self::from_gid(unistd::getgid().as_raw())
    }
}

impl SqGroupTrait for SqGroup {
    fn to_primitive(&self) -> anyhow::Result<Primitive> {
        Ok(Primitive::from(self.group.gid.as_raw()))
    }

    fn gid(&self) -> anyhow::Result<SqInt> {
        Ok(SqInt::from(self.group.gid.as_raw()))
    }

    fn name(&self) -> anyhow::Result<SqString> {
        Ok(SqString::new(self.group.name.clone()))
    }

    fn members(&self) -> anyhow::Result<SqValueSequence<SqUser>> {
        Ok(SqValueSequence::DoubleEndedIterator(Box::new(
            self.group
                .mem
                .iter()
                .map(|username| SqUser::from_name(username.as_str())),
        )))
    }
}
