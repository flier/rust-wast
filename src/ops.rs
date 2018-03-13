use std::{i32, u32};

use parity_wasm::elements::Opcode;

use parse::{const_type, int, nat, Parse, Value, float32, float64};

impl Parse for Opcode {
    named!(
        parse<Opcode>,
        alt_complete!(
            tag!("unreachable") => { |_| Opcode::Unreachable } |
            tag!("nop") => { |_| Opcode::Nop } |
            br
        )
    );
}

named!(br<Opcode>, map!(nat, |idx| Opcode::Br(idx as u32)));

named!(
    const_value<Opcode>,
    ws!(do_parse!(
        ty: const_type
            >> val:
                switch!(value!(ty),
            b"i32.const" => map!(verify!(int, |n| i32::MIN as isize <= n && n <= i32::MAX as isize), Value::Int) |
            b"i64.const" => map!(int, Value::Int) |
            b"f32.const" => map!(float32, |v| Value::Float(v as f64)) |
            b"f64.const" => map!(float64, Value::Float)
        ) >> (match ty {
            b"i32.const" => Opcode::I32Const(val.as_int() as i32),
            b"i64.const" => Opcode::I64Const(val.as_int() as i64),
            b"f32.const" => Opcode::F32Const(val.as_int() as u32),
            b"f64.const" => Opcode::F64Const(val.as_int() as u64),
            _ => unreachable!(),
        })
    ))
);

#[cfg(test)]
mod tests {
    use std::str;

    use nom::{ErrorKind, IResult};

    use super::*;

    lazy_static! {
        static ref CONST_TEST_CASES: Vec<(&'static [u8], IResult<&'static [u8], Opcode>)> = vec![
            (&b"i32.const 0xffffffff"[..],  IResult::Error(ErrorKind::Switch)),
            (&b"i32.const -0x80000000"[..], IResult::Done(&[][..], Opcode::I32Const(-0x80000000))),
            (&b"i64.const 18446744073709551615"[..], IResult::Done(&[][..], Opcode::I64Const(-1))),
            (&b"i64.const -9223372036854775808"[..], IResult::Error(ErrorKind::Switch)),
            (&b"f32.const 0x1p127"[..], IResult::Done(&[][..], Opcode::F32Const(0))),
        ];
    }

    #[test]
    fn parse_const() {
        for &(code, ref result) in CONST_TEST_CASES.iter() {
            assert_eq!(const_value(code), *result, "parse {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }
}
