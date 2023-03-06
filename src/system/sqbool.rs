use crate::primitive::Primitive;
use crate::system::SqBoolTrait;

pub struct SqBool {
    value: bool,
}

impl SqBool {
    pub fn new(value: bool) -> Self {
        Self { value }
    }
}

impl SqBoolTrait for SqBool {
    fn to_primitive(&self) -> anyhow::Result<Primitive> {
        Ok(Primitive::Bool(self.value))
    }

    fn not(&self) -> anyhow::Result<SqBool> {
        Ok(Self::new(!self.value))
    }
}
