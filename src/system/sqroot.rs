use std::env;
use std::iter::Iterator;
use std::path::PathBuf;

use anyhow::{anyhow, ensure};

use crate::primitive::Primitive;
use crate::sqvalue::SqValueSequence;
use crate::system::{sqbool::SqBool, sqint::SqInt, sqpath::SqPath, SqRootTrait};

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
}
