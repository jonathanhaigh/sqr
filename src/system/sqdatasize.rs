// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::primitive::Primitive;
use crate::system::{sqfloat::SqFloat, sqint::SqInt, SqDataSizeTrait};

const KIB: f64 = 1024f64;
const KB: f64 = 1000f64;
const MIB: f64 = 1024f64 * KIB;
const MB: f64 = 1000f64 * KB;
const GIB: f64 = 1024f64 * MIB;
const GB: f64 = 1000f64 * MB;
const TIB: f64 = 1024f64 * GIB;
const TB: f64 = 1000f64 * GB;
const PIB: f64 = 1024f64 * TIB;
const PB: f64 = 1000f64 * TB;

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
        Ok(Primitive::I128(i128::from(self.value)))
    }

    fn b(&self) -> anyhow::Result<SqInt> {
        Ok(SqInt::from(self.value))
    }

    fn kib(&self) -> anyhow::Result<SqFloat> {
        Ok(SqFloat::new(self.value as f64 / KIB))
    }

    fn kb(&self) -> anyhow::Result<SqFloat> {
        Ok(SqFloat::new(self.value as f64 / KB))
    }

    fn mib(&self) -> anyhow::Result<SqFloat> {
        Ok(SqFloat::new(self.value as f64 / MIB))
    }

    fn mb(&self) -> anyhow::Result<SqFloat> {
        Ok(SqFloat::new(self.value as f64 / MB))
    }

    fn gib(&self) -> anyhow::Result<SqFloat> {
        Ok(SqFloat::new(self.value as f64 / GIB))
    }

    fn gb(&self) -> anyhow::Result<SqFloat> {
        Ok(SqFloat::new(self.value as f64 / GB))
    }

    fn tib(&self) -> anyhow::Result<SqFloat> {
        Ok(SqFloat::new(self.value as f64 / TIB))
    }

    fn tb(&self) -> anyhow::Result<SqFloat> {
        Ok(SqFloat::new(self.value as f64 / TB))
    }

    fn pib(&self) -> anyhow::Result<SqFloat> {
        Ok(SqFloat::new(self.value as f64 / PIB))
    }

    fn pb(&self) -> anyhow::Result<SqFloat> {
        Ok(SqFloat::new(self.value as f64 / PB))
    }
}

impl<T> From<T> for SqDataSize
where
    u64: From<T>,
{
    fn from(value: T) -> Self {
        Self::new(u64::from(value))
    }
}
