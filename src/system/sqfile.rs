// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

use std::path::PathBuf;

use anyhow::anyhow;
use nix::sys::stat;

use crate::primitive::Primitive;
use crate::system::{
    sqdatasize::SqDataSize, sqfilemode::SqFileMode, sqgroup::SqGroup, sqint::SqInt,
    sqstring::SqString, sqsystemtime::SqSystemTime, squser::SqUser, SqFileTrait,
};

pub struct SqFile {
    stat: stat::FileStat,
    path: PathBuf,
}

impl SqFile {
    pub fn new(stat: stat::FileStat, path: PathBuf) -> Self {
        Self { stat, path }
    }

    pub fn to_str(&self) -> anyhow::Result<&str> {
        self.path.to_str().ok_or(anyhow!("path is not valid UTF-8"))
    }

    pub fn stat_time_to_sq_system_time(
        &self,
        which_time: &'static str,
        secs: i64,
        nanos: i64,
    ) -> anyhow::Result<SqSystemTime> {
        let Ok(usecs) = u64::try_from(secs) else {
            return Err(anyhow!(
                "Failed to convert {} ({} seconds) of {} to unsigned 64-bit integer",
                which_time,
                secs,
                self.path.to_string_lossy()
            ));
        };
        let Ok(unanos) = u64::try_from(nanos) else {
            return Err(anyhow!(
                "Failed to convert fractional part of {} ({} nanoseconds) of {} to unsigned 64-bit integer",
                which_time,
                nanos,
                self.path.to_string_lossy()
            ));
        };
        SqSystemTime::try_from_secs_and_nanos(usecs, unanos)
    }
}

impl SqFileTrait for SqFile {
    fn to_primitive(&self) -> anyhow::Result<Primitive> {
        Ok(Primitive::Str(self.to_str()?.to_owned()))
    }

    fn inode(&self) -> anyhow::Result<SqInt> {
        let inode = self.stat.st_ino;
        match i64::try_from(inode) {
            Ok(inode_i64) => Ok(SqInt::new(inode_i64)),
            Err(_) => Err(anyhow!(
                "Failed to convert inode number {} for {} to 64-bit signed integer",
                inode,
                self.path.to_string_lossy()
            )),
        }
    }

    fn size(&self) -> anyhow::Result<SqDataSize> {
        let isize = self.stat.st_size;
        match u64::try_from(isize) {
            Ok(usize) => Ok(SqDataSize::new(usize)),
            Err(_) => Err(anyhow!(
                "Failed to convert file size {} for {} to 64-bit unsigned integer",
                isize,
                self.path.to_string_lossy()
            )),
        }
    }

    // The field name is "type", but that's a Rust keyword so it's transformed to "type0".
    fn type0(&self) -> anyhow::Result<SqString> {
        use stat::SFlag;
        let file_type = SFlag::from_bits_truncate(self.stat.st_mode);
        let type_str = if file_type == SFlag::S_IFREG {
            "file"
        } else if file_type == SFlag::S_IFDIR {
            "dir"
        } else if file_type == SFlag::S_IFLNK {
            "symlink"
        } else if file_type == SFlag::S_IFBLK {
            "block_device"
        } else if file_type == SFlag::S_IFCHR {
            "char_device"
        } else if file_type == SFlag::S_IFIFO {
            "fifo"
        } else if file_type == SFlag::S_IFSOCK {
            "socket"
        } else {
            return Err(anyhow!(
                "Unrecognized file type for {}",
                self.path.to_string_lossy()
            ));
        };
        Ok(SqString::new(type_str.to_owned()))
    }

    fn hard_link_count(&self) -> anyhow::Result<SqInt> {
        let count = self.stat.st_nlink;
        match i64::try_from(count) {
            Ok(count_i64) => Ok(SqInt::new(count_i64)),
            Err(_) => Err(anyhow!(
                "Failed to convert hard link count {} for {} to 64-bit signed integer",
                count,
                self.path.to_string_lossy()
            )),
        }
    }

    fn mode(&self) -> anyhow::Result<SqFileMode> {
        Ok(SqFileMode::new(stat::Mode::from_bits_truncate(
            self.stat.st_mode,
        )))
    }

    fn atime(&self) -> anyhow::Result<SqSystemTime> {
        self.stat_time_to_sq_system_time("atime", self.stat.st_atime, self.stat.st_atime_nsec)
    }

    fn mtime(&self) -> anyhow::Result<SqSystemTime> {
        self.stat_time_to_sq_system_time("mtime", self.stat.st_mtime, self.stat.st_mtime_nsec)
    }

    fn ctime(&self) -> anyhow::Result<SqSystemTime> {
        self.stat_time_to_sq_system_time("ctime", self.stat.st_ctime, self.stat.st_ctime_nsec)
    }

    fn block_count(&self) -> anyhow::Result<SqInt> {
        Ok(SqInt::new(self.stat.st_blocks))
    }

    fn user(&self) -> anyhow::Result<SqUser> {
        SqUser::from_uid(self.stat.st_uid)
    }

    fn group(&self) -> anyhow::Result<SqGroup> {
        SqGroup::from_gid(self.stat.st_gid)
    }
}
