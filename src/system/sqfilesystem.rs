// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

use std::path::Path;

use anyhow::anyhow;
use nix::sys::statvfs::{statvfs, Statvfs};

use crate::primitive::Primitive;
use crate::system::{sqdatasize::SqDataSize, sqfloat::SqFloat, sqint::SqInt, SqFilesystemTrait};

pub struct SqFilesystem {
    statvfs: Statvfs,
}

impl SqFilesystem {
    pub fn from_path(path: &Path) -> anyhow::Result<Self> {
        Ok(Self {
            statvfs: statvfs(path).map_err(|e| {
                anyhow!(
                    "Failed to get filesystem information for path {}: statvfs(3) failed: {}",
                    path.to_string_lossy().into_owned(),
                    e
                )
            })?,
        })
    }

    fn block_count_to_size(&self, block_count: u64) -> anyhow::Result<SqDataSize> {
        let block_size = self.statvfs.fragment_size();
        match block_count.checked_mul(block_size) {
            Some(size) => Ok(SqDataSize::from(size)),
            None => Err(anyhow!(
                "Cannot fit size ({} block size * {} blocks) into unsigned 64-bit integer",
                block_size,
                block_count
            )),
        }
    }

    fn block_count_to_percent(&self, block_count: u64) -> anyhow::Result<SqFloat> {
        Ok(SqFloat::new(
            100f64 * (block_count as f64) / (self.statvfs.blocks() as f64),
        ))
    }

    fn blocks_used_u64(&self) -> u64 {
        let blocks = self.statvfs.blocks();
        let available = self.statvfs.blocks_available();
        assert!(blocks > available);
        blocks - available
    }
}

impl SqFilesystemTrait for SqFilesystem {
    fn to_primitive(&self) -> anyhow::Result<Primitive> {
        Ok(Primitive::from(self.statvfs.filesystem_id()))
    }

    fn id(&self) -> anyhow::Result<SqInt> {
        Ok(SqInt::from(self.statvfs.filesystem_id()))
    }

    fn block_size(&self) -> anyhow::Result<SqDataSize> {
        Ok(SqDataSize::from(self.statvfs.fragment_size()))
    }

    fn blocks(&self) -> anyhow::Result<SqInt> {
        Ok(SqInt::from(self.statvfs.blocks()))
    }

    fn blocks_available(&self) -> anyhow::Result<SqInt> {
        Ok(SqInt::from(self.statvfs.blocks_available()))
    }

    fn blocks_used(&self) -> anyhow::Result<SqInt> {
        Ok(SqInt::from(self.blocks_used_u64()))
    }

    fn size(&self) -> anyhow::Result<SqDataSize> {
        self.block_count_to_size(self.statvfs.blocks())
    }

    fn space_available(&self) -> anyhow::Result<SqDataSize> {
        self.block_count_to_size(self.statvfs.blocks_available())
    }

    fn space_used(&self) -> anyhow::Result<SqDataSize> {
        self.block_count_to_size(self.blocks_used_u64())
    }

    fn percent_available(&self) -> anyhow::Result<SqFloat> {
        self.block_count_to_percent(self.statvfs.blocks_available())
    }

    fn percent_used(&self) -> anyhow::Result<SqFloat> {
        self.block_count_to_percent(self.blocks_used_u64())
    }
}
