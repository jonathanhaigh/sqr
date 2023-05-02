// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

use anyhow::anyhow;
use nix::unistd::{Gid, Group};

use crate::primitive::Primitive;
use crate::sqvalue::SqValueSequence;
use crate::system::{sqint::SqInt, sqstring::SqString, squser::SqUser, SqGroupTrait};

pub struct SqGroup {
    group: Group,
}

impl SqGroup {
    pub fn from_gid(gid: u32) -> anyhow::Result<Option<Self>> {
        match Group::from_gid(Gid::from_raw(gid)) {
            Ok(Some(group)) => Ok(Some(Self { group })),
            Ok(None) => Ok(None),
            Err(e) => Err(anyhow!("Error getting group from GID {}: {}", gid, e)),
        }
    }
}

impl SqGroupTrait for SqGroup {
    fn to_primitive(&self) -> anyhow::Result<Primitive> {
        Ok(Primitive::Int(i64::from(self.group.gid.as_raw())))
    }

    fn gid(&self) -> anyhow::Result<SqInt> {
        Ok(SqInt::new(i64::from(self.group.gid.as_raw())))
    }

    fn name(&self) -> anyhow::Result<SqString> {
        Ok(SqString::new(self.group.name.clone()))
    }

    fn members(&self) -> anyhow::Result<SqValueSequence<SqUser>> {
        Ok(SqValueSequence::DoubleEndedIterator(Box::new(
            self.group
                .mem
                .iter()
                .filter_map(|username| SqUser::from_name(username.as_str()).transpose()),
        )))
    }
}
