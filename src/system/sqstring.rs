use crate::primitive::Primitive;
use crate::system::SqStringTrait;

pub struct SqString {
    value: String,
}

impl SqString {
    pub fn new(value: String) -> Self {
        Self { value }
    }
}

impl SqStringTrait for SqString {
    fn to_primitive(&self) -> anyhow::Result<Primitive> {
        Ok(Primitive::Str(self.value.clone()))
    }
}
