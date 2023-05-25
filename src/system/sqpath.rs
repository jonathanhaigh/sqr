// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

use std::env;
use std::path::PathBuf;

use anyhow::anyhow;
use nix::errno;
use nix::sys::stat;
use walkdir::WalkDir;

use crate::primitive::Primitive;
use crate::sqvalue::SqValueSequence;
use crate::system::{
    sqbool::SqBool, sqfile::SqFile, sqosstring::SqOsString, sqstring::SqString, SqPathTrait,
};

pub struct SqPath {
    path: PathBuf,
}

impl SqPath {
    pub fn new(path: PathBuf) -> Self {
        Self { path }
    }

    pub fn to_str(&self) -> anyhow::Result<&str> {
        self.path.to_str().ok_or(anyhow!("path is not valid UTF-8"))
    }
}

impl SqPathTrait for SqPath {
    fn to_primitive(&self) -> anyhow::Result<Primitive> {
        Ok(Primitive::Str(self.to_str()?.to_owned()))
    }

    fn string(&self, replace_invalid: bool) -> anyhow::Result<SqString> {
        if replace_invalid {
            Ok(SqString::new(self.path.to_string_lossy().into_owned()))
        } else {
            Ok(SqString::new(self.to_str()?.to_owned()))
        }
    }

    fn os_string(&self) -> anyhow::Result<SqOsString> {
        Ok(SqOsString::new(self.path.as_os_str().to_owned()))
    }

    fn parent(&self) -> anyhow::Result<Option<SqPath>> {
        Ok(self.path.parent().map(|p| SqPath::new(p.to_path_buf())))
    }

    fn filename(&self) -> anyhow::Result<Option<SqOsString>> {
        Ok(self.path.file_name().map(|s| SqOsString::new(s.to_owned())))
    }

    fn stem(&self) -> anyhow::Result<Option<SqOsString>> {
        Ok(self.path.file_stem().map(|s| SqOsString::new(s.to_owned())))
    }

    fn exists(&self) -> anyhow::Result<SqBool> {
        Ok(SqBool::new(self.path.try_exists()?))
    }

    fn extension(&self) -> anyhow::Result<Option<SqOsString>> {
        Ok(self.path.extension().map(|s| SqOsString::new(s.to_owned())))
    }

    fn children(
        &self,
        recurse: bool,
        follow_symlinks: bool,
        skip_permission_denied: bool,
        same_filesystem: bool,
    ) -> anyhow::Result<SqValueSequence<SqPath>> {
        let max_depth = if recurse { usize::MAX } else { 1 };

        Ok(SqValueSequence::Iterator(Box::new(
            WalkDir::new(&self.path)
                .min_depth(1)
                .max_depth(max_depth)
                .follow_links(follow_symlinks)
                .same_file_system(same_filesystem)
                .into_iter()
                .filter_map(move |result| match result {
                    Ok(dirent) => Some(Ok(SqPath::new(dirent.path().to_owned()))),
                    Err(e) => match (e.io_error(), skip_permission_denied) {
                        (Some(_inner_io_error), true) => None,
                        _ => Some(Err(anyhow::Error::from(e))),
                    },
                }),
        )))
    }

    fn parts(&self) -> anyhow::Result<SqValueSequence<SqOsString>> {
        Ok(SqValueSequence::DoubleEndedIterator(Box::new(
            self.path
                .iter()
                .map(|part| Ok(SqOsString::new(part.to_owned()))),
        )))
    }

    fn absolute(&self) -> anyhow::Result<SqPath> {
        if self.path.is_absolute() {
            Ok(SqPath::new(self.path.clone()))
        } else {
            match env::current_dir() {
                Ok(mut pb) => {
                    pb.push(&self.path);
                    Ok(SqPath::new(pb))
                }
                Err(e) => Err(anyhow!(
                    "Failed to determine current working directory: {}",
                    e
                )),
            }
        }
    }

    fn canonical(&self) -> anyhow::Result<SqPath> {
        Ok(SqPath::new(self.path.canonicalize()?))
    }

    fn is_absolute(&self) -> anyhow::Result<SqBool> {
        Ok(SqBool::new(self.path.is_absolute()))
    }

    fn file(&self, follow_symlinks: bool) -> anyhow::Result<Option<SqFile>> {
        let stat_result = if follow_symlinks {
            stat::stat(&self.path)
        } else {
            stat::lstat(&self.path)
        };

        match stat_result {
            Ok(stat) => Ok(Some(SqFile::new(stat, self.path.clone()))),
            Err(errno::Errno::ENOENT) => Ok(None),
            Err(e) => Err(anyhow!(
                "Failed to get file metadata for path {}: {}",
                self.path.to_string_lossy(),
                e
            )),
        }
    }
}
