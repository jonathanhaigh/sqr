use anyhow::anyhow;

use crate::primitive::Primitive;
use crate::system::SqDataSizeTrait;

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
}
