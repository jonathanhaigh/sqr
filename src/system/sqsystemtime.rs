// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

use std::time::{Duration, SystemTime};

use anyhow::anyhow;

use crate::primitive::Primitive;
use crate::system::SqSystemTimeTrait;

pub struct SqSystemTime {
    value: SystemTime,
}

impl SqSystemTime {
    pub fn new(value: SystemTime) -> Self {
        Self { value }
    }

    pub fn try_from_secs_and_nanos(secs: u64, nanos: u64) -> anyhow::Result<Self> {
        let value = SystemTime::UNIX_EPOCH
            .checked_add(Duration::from_secs(secs))
            .map(|st| st + Duration::from_nanos(nanos))
            .ok_or_else(|| {
                anyhow!(
                    "Failed to convert seconds ({}) and nanoseconds ({}) to system time",
                    secs,
                    nanos
                )
            })?;

        Ok(Self { value })
    }
}

impl SqSystemTimeTrait for SqSystemTime {
    fn to_primitive(&self) -> anyhow::Result<Primitive> {
        let usecs = match self.value.duration_since(SystemTime::UNIX_EPOCH) {
            Ok(duration) => duration.as_secs(),
            Err(_) => return Err(anyhow!("Failed to convert system time to UNIX timestamp")),
        };
        let Ok(isecs) = i64::try_from(usecs) else {
            return Err(anyhow!("Failed to convert UNIX timestamp {} to a 64-bit signed integer", usecs));
        };

        Ok(Primitive::Int(isecs))
    }
}
