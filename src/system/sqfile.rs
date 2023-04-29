// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

use std::fs::Metadata;
use std::path::PathBuf;

use anyhow::anyhow;

use crate::primitive::Primitive;
use crate::system::{sqdatasize::SqDataSize, SqFileTrait};

pub struct SqFile {
    metadata: Metadata,
    path: PathBuf,
}

impl SqFile {
    pub fn new(metadata: Metadata, path: PathBuf) -> Self {
        Self { metadata, path }
    }

    pub fn to_str(&self) -> anyhow::Result<&str> {
        self.path.to_str().ok_or(anyhow!("path is not valid UTF-8"))
    }
}

impl SqFileTrait for SqFile {
    fn to_primitive(&self) -> anyhow::Result<Primitive> {
        Ok(Primitive::Str(self.to_str()?.to_owned()))
    }

    fn size(&self) -> anyhow::Result<SqDataSize> {
        Ok(SqDataSize::new(self.metadata.len()))
    }
}
