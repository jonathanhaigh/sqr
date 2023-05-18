// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

use std::env;
use std::iter::Iterator;
use std::path::PathBuf;
use std::time::Duration;

use anyhow::{anyhow, ensure};

use crate::primitive::Primitive;
use crate::sqvalue::SqValueSequence;
use crate::system::{
    sqbool::SqBool, sqdatasize::SqDataSize, sqduration::SqDuration, sqfloat::SqFloat,
    sqgroup::SqGroup, sqint::SqInt, sqpath::SqPath, sqstring::SqString, squser::SqUser,
    SqRootTrait,
};

pub struct SqRoot {}

impl SqRoot {
    pub fn new() -> Self {
        Self {}
    }
}

impl Default for SqRoot {
    fn default() -> Self {
        Self::new()
    }
}

impl SqRootTrait for SqRoot {
    fn to_primitive(&self) -> anyhow::Result<Primitive> {
        Ok(Primitive::Str(String::from("root")))
    }

    fn bool(&self, p_value: Option<bool>) -> anyhow::Result<SqBool> {
        Ok(SqBool::new(p_value.unwrap_or(false)))
    }

    fn path(&self, p_value: Option<&str>) -> anyhow::Result<SqPath> {
        match p_value {
            Some(s) => Ok(SqPath::new(PathBuf::from(s))),
            None => match env::current_dir() {
                Ok(pb) => Ok(SqPath::new(pb)),
                Err(e) => Err(anyhow!(
                    "Failed to determine current working directory: {}",
                    e
                )),
            },
        }
    }

    fn int(&self, p_value: Option<i64>) -> anyhow::Result<SqInt> {
        Ok(SqInt::new(p_value.unwrap_or(0)))
    }

    fn ints(
        &self,
        start: Option<i64>,
        stop: Option<i64>,
        step: Option<i64>,
    ) -> anyhow::Result<SqValueSequence<SqInt>> {
        let start = start.unwrap_or(0);
        let step_i64 = step.unwrap_or(1);
        let step: usize = step_i64
            .try_into()
            .map_err(|_| anyhow!("Invalid step option{}: must be > 0", step_i64))?;
        ensure!(step > 0, "Invalid step option {}: must be > 0", step);
        // Only return an Iterator because:
        // * Range<i64> isn't an ExactSizeIterator because on 32-bit platforms the size hint (a
        //   usize) might not be big enough to hold the size of the range.
        // * Range<i64> isn't a DoubleEndedIterator because the range could be infinite.
        //
        // Perhaps we could add special cases for when the size fits into a usize. This field
        // probably isn't particularly useful for real-world applications though so it's probably
        // not worth the effort and complexity.
        Ok(SqValueSequence::Iterator(match stop {
            Some(s) => Box::new((start..s).step_by(step).map(|i| Ok(SqInt::new(i)))),
            None => Box::new((start..).step_by(step).map(|i| Ok(SqInt::new(i)))),
        }))
    }

    fn string(&self, value: Option<&str>) -> anyhow::Result<SqString> {
        Ok(SqString::new(value.unwrap_or("").to_owned()))
    }

    fn float(&self, value: Option<f64>) -> anyhow::Result<SqFloat> {
        Ok(SqFloat::new(value.unwrap_or(0f64)))
    }

    fn duration(
        &self,
        s: Option<i64>,
        ms: Option<i64>,
        us: Option<i64>,
        ns: Option<i64>,
    ) -> anyhow::Result<SqDuration> {
        let s_duration = Duration::from_secs(
            u64::try_from(s.unwrap_or(0)).map_err(|_| anyhow!("s argument must be nonnegative"))?,
        );
        let ms_duration = Duration::from_millis(
            u64::try_from(ms.unwrap_or(0))
                .map_err(|_| anyhow!("ms argument must be nonnegative"))?,
        );
        let us_duration = Duration::from_micros(
            u64::try_from(us.unwrap_or(0))
                .map_err(|_| anyhow!("us argument must be nonnegative"))?,
        );
        let ns_duration = Duration::from_nanos(
            u64::try_from(ns.unwrap_or(0))
                .map_err(|_| anyhow!("ns argument must be nonnegative"))?,
        );

        let opt_duration = s_duration
            .checked_add(ms_duration)
            .and_then(|d| d.checked_add(us_duration))
            .and_then(|d| d.checked_add(ns_duration));

        match opt_duration {
            Some(d) => Ok(SqDuration::new(d)),
            None => Err(anyhow!("Duration does not fit into SqDuration type.")),
        }
    }

    fn data_size(&self, value: Option<i64>) -> anyhow::Result<SqDataSize> {
        let value = value.unwrap_or(0);
        match u64::try_from(value) {
            Ok(v) => Ok(SqDataSize::new(v)),
            Err(_) => Err(anyhow!("Data size value must be non-negative")),
        }
    }

    fn user(&self, opt_username: Option<&str>, opt_uid: Option<i64>) -> anyhow::Result<SqUser> {
        if let Some(username) = opt_username {
            if opt_uid.is_some() {
                return Err(anyhow!(
                    "At most one of the `username` and `uid` parameters can be specified"
                ));
            }
            return SqUser::from_name(username);
        }
        if let Some(uid) = opt_uid {
            return match u32::try_from(uid) {
                Ok(u32uid) => SqUser::from_uid(u32uid),
                Err(_) => Err(anyhow!(
                    "uid argument {} must be in the range [{},{})",
                    uid,
                    0,
                    u32::MAX
                )),
            };
        }
        SqUser::real()
    }

    fn group(&self, opt_group_name: Option<&str>, opt_gid: Option<i64>) -> anyhow::Result<SqGroup> {
        if let Some(group_name) = opt_group_name {
            if opt_gid.is_some() {
                return Err(anyhow!(
                    "At most one of the `group_name` and `gid` parameters can be specified"
                ));
            }
            return SqGroup::from_name(group_name);
        }
        if let Some(gid) = opt_gid {
            return match u32::try_from(gid) {
                Ok(u32gid) => SqGroup::from_gid(u32gid),
                Err(_) => Err(anyhow!(
                    "gid argument {} must be in the range [{},{})",
                    gid,
                    0,
                    u32::MAX
                )),
            };
        }
        SqGroup::real()
    }
}
