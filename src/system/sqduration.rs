// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

use std::time::Duration;

use anyhow::anyhow;

use crate::primitive::Primitive;
use crate::system::{sqint::SqInt, SqDurationTrait};

pub struct SqDuration {
    value: Duration,
}

impl SqDuration {
    pub fn new(value: Duration) -> Self {
        Self { value }
    }
}

impl SqDurationTrait for SqDuration {
    fn to_primitive(&self) -> anyhow::Result<Primitive> {
        Ok(Primitive::Float(self.value.as_secs_f64()))
    }

    fn s(&self) -> anyhow::Result<SqInt> {
        let secs_u64 = self.value.as_secs();
        let Ok(secs_i64) = i64::try_from(secs_u64) else {
            return Err(anyhow!(
                "Failed to convert duration in seconds ({}s) to a 64-bit signed integer",
                secs_u64
            ));
        };

        Ok(SqInt::new(secs_i64))
    }

    fn subsec_ns(&self) -> anyhow::Result<SqInt> {
        Ok(SqInt::new(i64::from(self.value.subsec_nanos())))
    }

    fn subsec_us(&self) -> anyhow::Result<SqInt> {
        Ok(SqInt::new(i64::from(self.value.subsec_micros())))
    }

    fn subsec_ms(&self) -> anyhow::Result<SqInt> {
        Ok(SqInt::new(i64::from(self.value.subsec_millis())))
    }
}
