// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

use anyhow::anyhow;

use crate::primitive::Primitive;
use crate::system::{sqfloat::SqFloat, sqint::SqInt, SqDataSizeTrait};

const KIB: f64 = 1024f64;
const KB: f64 = 1000f64;
const MIB: f64 = 1024f64 * KIB;
const MB: f64 = 1000f64 * KIB;
const GIB: f64 = 1024f64 * MIB;
const GB: f64 = 1000f64 * MIB;
const TIB: f64 = 1024f64 * GIB;
const TB: f64 = 1000f64 * GIB;
const PIB: f64 = 1024f64 * TIB;
const PB: f64 = 1000f64 * TIB;

pub struct SqDataSize {
    value: u64,
}

impl SqDataSize {
    pub fn new(value: u64) -> Self {
        Self { value }
    }
}

impl SqDataSizeTrait for SqDataSize {
    fn to_primitive(&self) -> anyhow::Result<Primitive> {
        match i64::try_from(self.value) {
            Err(_) => Err(anyhow!(
                "Failed to convert SqDataSize {}B to 64-bit signed integer",
                self.value
            )),
            Ok(value_i64) => Ok(Primitive::Int(value_i64)),
        }
    }

    fn B(&self) -> anyhow::Result<SqInt> {
        match i64::try_from(self.value) {
            Ok(value_i64) => Ok(SqInt::new(value_i64)),
            Err(_) => Err(anyhow!(
                "Failed to convert data size {}B to 64-bit signed integer",
                self.value
            )),
        }
    }

    fn KiB(&self) -> anyhow::Result<SqFloat> {
        Ok(SqFloat::new(self.value as f64 / KIB))
    }

    fn kB(&self) -> anyhow::Result<SqFloat> {
        Ok(SqFloat::new(self.value as f64 / KB))
    }

    fn MiB(&self) -> anyhow::Result<SqFloat> {
        Ok(SqFloat::new(self.value as f64 / MIB))
    }

    fn MB(&self) -> anyhow::Result<SqFloat> {
        Ok(SqFloat::new(self.value as f64 / MB))
    }

    fn GiB(&self) -> anyhow::Result<SqFloat> {
        Ok(SqFloat::new(self.value as f64 / GIB))
    }

    fn GB(&self) -> anyhow::Result<SqFloat> {
        Ok(SqFloat::new(self.value as f64 / GB))
    }

    fn TiB(&self) -> anyhow::Result<SqFloat> {
        Ok(SqFloat::new(self.value as f64 / TIB))
    }

    fn TB(&self) -> anyhow::Result<SqFloat> {
        Ok(SqFloat::new(self.value as f64 / TB))
    }

    fn PiB(&self) -> anyhow::Result<SqFloat> {
        Ok(SqFloat::new(self.value as f64 / PIB))
    }

    fn PB(&self) -> anyhow::Result<SqFloat> {
        Ok(SqFloat::new(self.value as f64 / PB))
    }
}
