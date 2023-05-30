// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

use anyhow::anyhow;

use crate::primitive::Primitive;
use crate::system::{sqstring::SqString, SqIntTrait};
use crate::util::InfallibleAbs;

fn format_int_helper<T>(value: T, radix: u8, prefix: &str, upper_case: bool) -> String
where
    radix_fmt::Radix<T>: std::fmt::Display,
{
    match upper_case {
        false => format!("{}{}", prefix, radix_fmt::radix(value, radix)),
        true => format!("{}{:#}", prefix, radix_fmt::radix(value, radix)),
    }
}

fn format_int(
    value: i128,
    radix: u8,
    prefix: &str,
    twos_complement: bool,
    upper_case: bool,
) -> String {
    if !twos_complement && value < 0i128 {
        return format_int_helper(
            value.infallible_abs(),
            radix,
            &format!("{}-", prefix),
            upper_case,
        );
    }
    format_int_helper(value, radix, prefix, upper_case)
}

pub struct SqInt {
    value: i128,
}

impl SqInt {
    pub fn new(value: i128) -> Self {
        Self { value }
    }
}

impl SqIntTrait for SqInt {
    fn to_primitive(&self) -> anyhow::Result<Primitive> {
        Ok(Primitive::I128(self.value))
    }

    fn string(
        &self,
        radix: u32,
        twos_complement: bool,
        upper_case: bool,
    ) -> anyhow::Result<SqString> {
        if radix > 36 {
            return Err(anyhow!("Radix argument must be in the range [0, 36]"));
        }
        let radix = u8::try_from(radix).unwrap();

        Ok(SqString::new(format_int(
            self.value,
            radix,
            "",
            twos_complement,
            upper_case,
        )))
    }

    fn binary(&self, prefix_radix: bool, twos_complement: bool) -> anyhow::Result<SqString> {
        let prefix = if prefix_radix { "0b" } else { "" };
        Ok(SqString::new(format_int(
            self.value,
            2,
            prefix,
            twos_complement,
            false,
        )))
    }

    fn octal(&self, prefix_radix: bool, twos_complement: bool) -> anyhow::Result<SqString> {
        let prefix = if prefix_radix { "0o" } else { "" };
        Ok(SqString::new(format_int(
            self.value,
            8,
            prefix,
            twos_complement,
            false,
        )))
    }

    fn hexadecimal(
        &self,
        prefix_radix: bool,
        twos_complement: bool,
        upper_case: bool,
    ) -> anyhow::Result<SqString> {
        let prefix = if prefix_radix { "0x" } else { "" };
        Ok(SqString::new(format_int(
            self.value,
            16,
            prefix,
            twos_complement,
            upper_case,
        )))
    }
}

impl<T> From<T> for SqInt
where
    i128: From<T>,
{
    fn from(v: T) -> Self {
        Self::new(i128::from(v))
    }
}
