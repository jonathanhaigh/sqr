// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

use anyhow::anyhow;
use nix::unistd;
use nix::unistd::{Uid, User};

use crate::primitive::Primitive;
use crate::system::{
    sqgroup::SqGroup, sqint::SqInt, sqpath::SqPath, sqstring::SqString, SqUserTrait,
};

pub struct SqUser {
    user: User,
}

impl SqUser {
    pub fn from_uid(uid: u32) -> anyhow::Result<Self> {
        match User::from_uid(Uid::from_raw(uid)) {
            Ok(Some(user)) => Ok(Self { user }),
            Ok(None) => Err(anyhow!("User with UID {} not found", uid)),
            Err(e) => Err(anyhow!("Error getting user from UID {}: {}", uid, e)),
        }
    }

    pub fn from_name(name: &str) -> anyhow::Result<Self> {
        match User::from_name(name) {
            Ok(Some(user)) => Ok(Self { user }),
            Ok(None) => Err(anyhow!("User wit username {} not found", name)),
            Err(e) => Err(anyhow!("Error getting user from username {}: {}", name, e)),
        }
    }

    pub fn real() -> anyhow::Result<Self> {
        Self::from_uid(unistd::getuid().as_raw())
    }

    fn gecos_as_string(&self) -> anyhow::Result<String> {
        match self.user.gecos.to_str() {
            Ok(s) => Ok(s.to_owned()),
            Err(e) => Err(anyhow!(
                "GECOS for user {} ({}) contains invalid unicode: {}",
                self.user.name,
                self.user.gid,
                e
            )),
        }
    }
}

impl SqUserTrait for SqUser {
    fn to_primitive(&self) -> anyhow::Result<Primitive> {
        Ok(Primitive::from(self.user.uid.as_raw()))
    }

    fn uid(&self) -> anyhow::Result<SqInt> {
        Ok(SqInt::from(self.user.uid.as_raw()))
    }

    fn username(&self) -> anyhow::Result<SqString> {
        Ok(SqString::new(self.user.name.clone()))
    }

    fn group(&self) -> anyhow::Result<SqGroup> {
        SqGroup::from_gid(self.user.gid.as_raw())
    }

    fn gecos(&self) -> anyhow::Result<SqString> {
        Ok(SqString::new(self.gecos_as_string()?))
    }

    fn name(&self) -> anyhow::Result<Option<SqString>> {
        let mut gecos = self.gecos_as_string()?;
        let name_end = gecos.find(',').unwrap_or(usize::MAX);
        if name_end == 0 {
            Ok(None)
        } else {
            gecos.truncate(name_end);
            Ok(Some(SqString::new(gecos)))
        }
    }

    fn home(&self) -> anyhow::Result<Option<SqPath>> {
        if self.user.dir.as_os_str().is_empty() {
            Ok(None)
        } else {
            Ok(Some(SqPath::new(self.user.dir.clone())))
        }
    }

    fn shell(&self) -> anyhow::Result<Option<SqPath>> {
        if self.user.shell.as_os_str().is_empty() {
            Ok(None)
        } else {
            Ok(Some(SqPath::new(self.user.shell.clone())))
        }
    }
}
