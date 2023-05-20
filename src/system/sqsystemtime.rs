// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

use std::time::{Duration, SystemTime};

use anyhow::anyhow;

use crate::primitive::Primitive;
use crate::system::{sqduration::SqDuration, SqSystemTimeTrait};

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

    pub fn since_epoch(&self) -> anyhow::Result<Duration> {
        self.value
            .duration_since(SystemTime::UNIX_EPOCH)
            .map_err(|e| {
                anyhow!(
                    "System time is {}s before UNIX epoch",
                    e.duration().as_secs_f64()
                )
            })
    }
}

impl SqSystemTimeTrait for SqSystemTime {
    fn to_primitive(&self) -> anyhow::Result<Primitive> {
        Ok(Primitive::from(self.since_epoch()?.as_secs()))
    }

    fn duration_since_epoch(&self) -> anyhow::Result<SqDuration> {
        Ok(SqDuration::new(self.since_epoch()?))
    }
}
