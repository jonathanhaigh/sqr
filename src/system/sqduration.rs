// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

use std::time::Duration;

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
        Ok(Primitive::F64(self.value.as_secs_f64()))
    }

    fn s(&self) -> anyhow::Result<SqInt> {
        Ok(SqInt::from(self.value.as_secs()))
    }

    fn subsec_ns(&self) -> anyhow::Result<SqInt> {
        Ok(SqInt::from(self.value.subsec_nanos()))
    }

    fn subsec_us(&self) -> anyhow::Result<SqInt> {
        Ok(SqInt::from(self.value.subsec_micros()))
    }

    fn subsec_ms(&self) -> anyhow::Result<SqInt> {
        Ok(SqInt::from(self.value.subsec_millis()))
    }
}
