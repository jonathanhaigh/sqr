use std::path::PathBuf;

use anyhow::anyhow;

use crate::primitive::Primitive;
use crate::system::SqFileTrait;

pub struct SqFile {
    path: PathBuf,
}

impl SqFile {
    pub fn new(path: PathBuf) -> Self {
        Self { path }
    }

    pub fn to_str(&self) -> anyhow::Result<&str> {
        self.path.to_str().ok_or(anyhow!("path is not valid UTF-8"))
    }
}

impl SqFileTrait for SqFile {
    fn to_primitive(&self) -> anyhow::Result<Primitive> {
        Ok(Primitive::Str(self.to_str()?.to_owned()))
    }
}
