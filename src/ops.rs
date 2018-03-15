use std::str::{self, FromStr};
use std::usize;

use parity_wasm::elements::NameMap;

use failure::Error;
use parity_wasm::elements::{FunctionType, Opcode, Type, ValueType};

use parse::{const_type, value_type, value_type_list, var, Var, float32, float64, int32, int64,
            nat32};

named_args!(
    opcode<'a>(labels: &'a NameMap, types: &'a NameMap, funcs: &'a mut Vec<Type>)<Opcode>,
    alt!(
        tag!("unreachable") => { |_| Opcode::Unreachable } |
        tag!("nop") => { |_| Opcode::Nop } |
        tag!("return") => { |_| Opcode::Return } |
        tag!("drop") => { |_| Opcode::Drop } |
        tag!("select") => { |_| Opcode::Select } |
        apply!(br, labels) |
        apply!(br_if, labels) |
        apply!(br_table, labels) |
        apply!(call, types) |
        apply!(call_indirect, types, funcs) |
        apply!(get_local, types) |
        apply!(set_local, types) |
        apply!(tee_local, types) |
        apply!(get_global, types) |
        apply!(set_global, types) |
        load |
        store |
        const_value
    )
);

named_args!(
    br<'a>(labels: &'a NameMap)<Opcode>,
    map_res!(
        ws!(preceded!(tag!("br"), var)),
        |var: Var| var.resolve_ref(labels).map(Opcode::Br)
    )
);

named_args!(
    br_if<'a>(labels: &'a NameMap)<Opcode>,
    map_res!(
        ws!(preceded!(tag!("br_if"), var)),
        |var: Var| var.resolve_ref(labels).map(Opcode::BrIf)
    )
);

named_args!(
    br_table<'a>(labels: &'a NameMap)<Opcode>,
    map_res!(ws!(preceded!(tag!("br_table"), many1!(var))),
        |vars: Vec<Var>| -> Result<Opcode, Error> {
            let (idx, offsets) = vars.split_last().unwrap();

            idx.resolve_ref(labels)
                .and_then(|idx|
                    offsets
                        .iter()
                        .map(|var| var.resolve_ref(labels)).collect::<Result<Vec<u32>, _>>()
                        .map(|offsets| Opcode::BrTable(offsets.into(), idx))
                )
        }
    )
);

named_args!(
    call<'a>(types: &'a NameMap)<Opcode>,
    map_res!(
        ws!(preceded!(tag!("call"), var)),
        |func: Var| func.resolve_ref(types).map(Opcode::Call)
    )
);

named_args!(
    type_use<'a>(types: &'a NameMap)<u32>,
    ws!(delimited!(
        tag!("("),
        preceded!(
            tag!("type"),
            map_res!(var, |var: Var| var.resolve_ref(types))
        ),
        tag!(")")
    ))
);

named_args!(
    call_indirect<'a>(types: &'a NameMap, funcs: &'a mut Vec<Type>)<Opcode>,
    ws!(preceded!(tag!("call_indirect"), map!(apply!(func_type, types),
        |(type_use, func_type): (Option<u32>, Option<FunctionType>)|
            if let Some(idx) = type_use {
                Opcode::CallIndirect(idx, 0)
            } else if let Some(func_type) = func_type {
                if let Some(idx) = funcs.iter().position(|ty| { Type::Function(func_type.clone()) == *ty }) {
                    Opcode::CallIndirect(idx as u32, 0)
                } else {
                    let idx = funcs.len();

                    funcs.push(Type::Function(func_type));

                    Opcode::CallIndirect(idx as u32, 0)
                }
            } else {
                Opcode::CallIndirect(0, 0)
            }
        )
    ))
);

named_args!(
    func_type<'a>(types: &'a NameMap)<(Option<u32>, Option<FunctionType>)>,
    alt!(
        map_res!(var, |var: Var| var.resolve_ref(types)) => { |idx| (Some(idx), None) } |
        ws!(pair!(opt!(apply!(type_use, types)), opt!(complete!(func_sig))))
    )
);

named!(
    func_sig<FunctionType>,
    map!(
        ws!(pair!(many0!(func_param), opt!(complete!(func_result)))),
        |(params, return_type)| FunctionType::new(
            params.into_iter().flat_map(|types| types).collect(),
            return_type.unwrap_or_default()
        )
    )
);

named!(
    func_param<Vec<ValueType>>,
    ws!(delimited!(
        tag!("("),
        preceded!(tag!("param"), value_type_list),
        tag!(")")
    ))
);

named!(
    func_result<Option<ValueType>>,
    ws!(delimited!(
        tag!("("),
        preceded!(tag!("result"), opt!(value_type)),
        tag!(")")
    ))
);

named!(
    const_value<Opcode>,
    ws!(switch!(const_type,
            b"i32.const" => map!(int32, |n| Opcode::I32Const(n)) |
            b"i64.const" => map!(int64, |n| Opcode::I64Const(n)) |
            b"f32.const" => map!(float32, |v| Opcode::F32Const(v as u32)) |
            b"f64.const" => map!(float64, |v| Opcode::F64Const(v as u64))
        ))
);

named_args!(
    get_local<'a>(types: &'a NameMap)<Opcode>,
    map!(
        map_res!(
            ws!(preceded!(tag!("get_local"), var)),
            |var: Var| var.resolve_ref(types)
        ),
    Opcode::GetLocal)
);

named_args!(
    set_local<'a>(types: &'a NameMap)<Opcode>,
    map!(
        map_res!(
            ws!(preceded!(tag!("set_local"), var)),
            |var: Var| var.resolve_ref(types)
        ),
    Opcode::SetLocal)
);

named_args!(
    tee_local<'a>(types: &'a NameMap)<Opcode>,
    map!(
        map_res!(
            ws!(preceded!(tag!("tee_local"), var)),
            |var: Var| var.resolve_ref(types)
        ),
    Opcode::TeeLocal)
);

named_args!(
    get_global<'a>(types: &'a NameMap)<Opcode>,
    map!(
        map_res!(
            ws!(preceded!(tag!("get_global"), var)),
            |var: Var| var.resolve_ref(types)
        ),
    Opcode::GetGlobal)
);

named_args!(
    set_global<'a>(types: &'a NameMap)<Opcode>,
    map!(
        map_res!(
            ws!(preceded!(tag!("set_global"), var)),
            |var: Var| var.resolve_ref(types)
        ),
    Opcode::SetGlobal)
);

/// <val_type>.load((8|16|32)_<sign>)? <offset>? <align>?
#[cfg_attr(rustfmt, rustfmt_skip)]
named!(
    load<Opcode>,
    ws!(do_parse!(
        op: recognize!(tuple!(value_type, tag!(".load"), opt!(complete!(tuple!(mem_size, tag!("_"), sign))))) >>
        offset: map!(opt!(complete!(offset)), |n| n.unwrap_or_default()) >>
        align: verify!(
            map!(opt!(complete!(align)), |n| n.unwrap_or_default()),
            |n: u32| n == 0 || n.is_power_of_two()
        ) >>
        (match op {
            b"i32.load" => Opcode::I32Load(align, offset),
            b"i64.load" => Opcode::I64Load(align, offset),
            b"f32.load" => Opcode::F32Load(align, offset),
            b"f64.load" => Opcode::F64Load(align, offset),
            b"i32.load8_s" => Opcode::I32Load8S(align, offset),
            b"i32.load8_u" => Opcode::I32Load8U(align, offset),
            b"i32.load16_s" => Opcode::I32Load16S(align, offset),
            b"i32.load16_u" => Opcode::I32Load16U(align, offset),
            b"i64.load8_s" => Opcode::I64Load8S(align, offset),
            b"i64.load8_u" => Opcode::I64Load8U(align, offset),
            b"i64.load16_s" => Opcode::I64Load16S(align, offset),
            b"i64.load16_u" => Opcode::I64Load16U(align, offset),
            b"i64.load32_s" => Opcode::I64Load32S(align, offset),
            b"i64.load32_u" => Opcode::I64Load32U(align, offset),
            _ => unreachable!(),
        })
    ))
);

/// <val_type>.store(8|16|32)? <offset>? <align>?
#[cfg_attr(rustfmt, rustfmt_skip)]
named!(
    store<Opcode>,
    ws!(do_parse!(
        op: recognize!(tuple!(
                value_type,
                tag!(".store"),
                opt!(complete!(mem_size))
            )) >>
        offset: map!(opt!(complete!(offset)), |n| n.unwrap_or_default()) >>
        align: verify!(
            map!(opt!(complete!(align)), |n| n.unwrap_or_default()),
            |n: u32| n == 0 || n.is_power_of_two()
        ) >>
        (
            match op {
                b"i32.store" => Opcode::I32Store(align, offset),
                b"i64.store" => Opcode::I64Store(align, offset),
                b"f32.store" => Opcode::F32Store(align, offset),
                b"f64.store" => Opcode::F64Store(align, offset),
                b"i32.store8" => Opcode::I32Store8(align, offset),
                b"i32.store16" => Opcode::I32Store16(align, offset),
                b"i64.store8" => Opcode::I64Store8(align, offset),
                b"i64.store16" => Opcode::I64Store16(align, offset),
                b"i64.store32" => Opcode::I64Store32(align, offset),
                _ => unreachable!(),
            }
        )
    ))
);

named!(
    mem_size<usize>,
    map_res!(
        map_res!(alt!(tag!("8") | tag!("16") | tag!("32")), str::from_utf8),
        usize::from_str
    )
);

/// sign:  s|u
named!(sign, alt!(tag!("s") | tag!("u")));

/// offset: offset=<nat>
named!(
    offset<u32>,
    map!(preceded!(tag!("offset="), nat32), |n| n as u32)
);

/// align: align=(1|2|4|8|...)
named!(
    align<u32>,
    map!(preceded!(tag!("align="), nat32), |n| n as u32)
);

#[cfg(test)]
mod tests {
    use std::str;

    use pretty_env_logger;

    use nom::{ErrorKind, IResult};

    use super::*;

    #[test]
    fn parse_simple_opcode() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"unreachable", IResult::Done(&[][..], Opcode::Unreachable)),
            (b"nop", IResult::Done(&[][..], Opcode::Nop)),
            (b"return", IResult::Done(&[][..], Opcode::Return)),
            (b"drop", IResult::Done(&[][..], Opcode::Drop)),
            (b"select", IResult::Done(&[][..], Opcode::Select)),
        ];
        let labels = NameMap::default();
        let types = NameMap::default();
        let mut funcs = vec![];

        for &(code, ref result) in tests.iter() {
            assert_eq!(
                opcode(code, &labels, &types, &mut funcs),
                *result,
                "parse opcode: {}",
                unsafe { str::from_utf8_unchecked(code) }
            );
        }
    }

    #[test]
    fn parse_br() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"br 0", IResult::Done(&[][..], Opcode::Br(0))),
            (b"br $done", IResult::Done(&[][..], Opcode::Br(123))),
        ];
        let mut labels = NameMap::default();

        labels.insert(123, "done".to_owned());

        for &(code, ref result) in tests.iter() {
            assert_eq!(br(code, &labels), *result, "parse opcode: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_br_if() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"br_if 0", IResult::Done(&[][..], Opcode::BrIf(0))),
            (b"br_if $done", IResult::Done(&[][..], Opcode::BrIf(123))),
        ];
        let mut labels = NameMap::default();

        labels.insert(123, "done".to_owned());

        for &(code, ref result) in tests.iter() {
            assert_eq!(br_if(code, &labels), *result, "parse opcode: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_br_table() {
        let tests: Vec<(&[u8], _)> = vec![
            (
                b"br_table 0",
                IResult::Done(&[][..], Opcode::BrTable(Box::new([]), 0)),
            ),
            (
                b"br_table 0 0",
                IResult::Done(&[][..], Opcode::BrTable(Box::new([0]), 0)),
            ),
            (
                b"br_table 3 2 1 0 4",
                IResult::Done(&[][..], Opcode::BrTable(Box::new([3, 2, 1, 0]), 4)),
            ),
        ];
        let labels = NameMap::default();

        for &(code, ref result) in tests.iter() {
            assert_eq!(
                br_table(code, &labels),
                *result,
                "parse opcode: {}",
                unsafe { str::from_utf8_unchecked(code) }
            );
        }
    }

    #[test]
    fn parse_call() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"call 0", IResult::Done(&[][..], Opcode::Call(0))),
            (b"call $hello", IResult::Done(&[][..], Opcode::Call(123))),
        ];
        let mut labels = NameMap::default();

        labels.insert(123, "hello".to_owned());

        for &(code, ref result) in tests.iter() {
            assert_eq!(call(code, &labels), *result, "parse opcode: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_type_use() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"(type 123)", IResult::Done(&[][..], 123)),
            (b"(type $hello)", IResult::Done(&[][..], 123)),
        ];
        let mut labels = NameMap::default();

        labels.insert(123, "hello".to_owned());

        for &(code, ref result) in tests.iter() {
            assert_eq!(
                type_use(code, &labels),
                *result,
                "parse opcode: {}",
                unsafe { str::from_utf8_unchecked(code) }
            );
        }
    }

    #[test]
    fn parse_func_param() {
        let func_param_tests: Vec<(&[u8], _)> = vec![
            (b"(param)", IResult::Done(&[][..], vec![])),
            (b"(param i32)", IResult::Done(&[][..], vec![ValueType::I32])),
            (
                b"(param f64 i32 i64)",
                IResult::Done(
                    &[][..],
                    vec![ValueType::F64, ValueType::I32, ValueType::I64],
                ),
            ),
        ];

        for &(code, ref result) in func_param_tests.iter() {
            assert_eq!(func_param(code), *result, "parse func_param: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_func_result() {
        let func_result_tests: Vec<(&[u8], _)> = vec![
            (b"(result)", IResult::Done(&[][..], None)),
            (
                b"(result i32)",
                IResult::Done(&[][..], Some(ValueType::I32)),
            ),
            (b"(result i32 i64)", IResult::Error(ErrorKind::Tag)),
        ];

        for &(code, ref result) in func_result_tests.iter() {
            assert_eq!(
                func_result(code),
                *result,
                "parse func_result: {}",
                unsafe { str::from_utf8_unchecked(code) }
            );
        }
    }

    #[test]
    fn parse_call_indirect() {
        let call_indirect_tests: Vec<(&[u8], _)> = vec![
            (
                b"call_indirect 123",
                IResult::Done(&[][..], Opcode::CallIndirect(123, 0)),
            ),
            (
                b"call_indirect $hello",
                IResult::Done(&[][..], Opcode::CallIndirect(123, 0)),
            ),
            (
                b"call_indirect (type $hello)",
                IResult::Done(&[][..], Opcode::CallIndirect(123, 0)),
            ),
            (
                b"call_indirect (param i32)",
                IResult::Done(&[][..], Opcode::CallIndirect(0, 0)),
            ),
            (
                b"call_indirect (param i64)",
                IResult::Done(&[][..], Opcode::CallIndirect(1, 0)),
            ),
        ];
        let mut labels = NameMap::default();
        let mut funcs = vec![
            Type::Function(FunctionType::new(vec![ValueType::I32], None)),
        ];

        labels.insert(123, "hello".to_owned());

        for &(code, ref result) in call_indirect_tests.iter() {
            assert_eq!(
                call_indirect(code, &labels, &mut funcs),
                *result,
                "parse opcode: {}",
                unsafe { str::from_utf8_unchecked(code) }
            );
        }

        assert_eq!(
            &funcs,
            &[
                Type::Function(FunctionType::new(vec![ValueType::I32], None)),
                Type::Function(FunctionType::new(vec![ValueType::I64], None)),
            ]
        );
    }

    #[test]
    fn parse_const() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"i32.const 0xffffffff", IResult::Error(ErrorKind::Switch)),
            (
                b"i32.const -0x80000000",
                IResult::Done(&[][..], Opcode::I32Const(-0x80000000)),
            ),
            (
                b"i64.const 18446744073709551615",
                IResult::Error(ErrorKind::Switch),
            ),
            (
                b"i64.const -9223372036854775808",
                IResult::Error(ErrorKind::Switch),
            ),
            (
                b"f32.const 0x1p127",
                IResult::Done(&[][..], Opcode::F32Const(0)),
            ),
        ];

        for (code, ref result) in tests {
            assert_eq!(const_value(code), *result, "parse const: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_get_local() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"get_local 0", IResult::Done(&[][..], Opcode::GetLocal(0))),
            (
                b"get_local $x",
                IResult::Done(&[][..], Opcode::GetLocal(123)),
            ),
            (b"set_local 0", IResult::Done(&[][..], Opcode::SetLocal(0))),
            (
                b"set_local $x",
                IResult::Done(&[][..], Opcode::SetLocal(123)),
            ),
            (b"tee_local 0", IResult::Done(&[][..], Opcode::TeeLocal(0))),
            (
                b"tee_local $x",
                IResult::Done(&[][..], Opcode::TeeLocal(123)),
            ),
            (
                b"get_global 0",
                IResult::Done(&[][..], Opcode::GetGlobal(0)),
            ),
            (
                b"get_global $x",
                IResult::Done(&[][..], Opcode::GetGlobal(123)),
            ),
            (
                b"set_global 0",
                IResult::Done(&[][..], Opcode::SetGlobal(0)),
            ),
            (
                b"set_global $x",
                IResult::Done(&[][..], Opcode::SetGlobal(123)),
            ),
        ];
        let labels = NameMap::default();
        let mut types = NameMap::default();
        let mut funcs = vec![];

        types.insert(123, "x".to_owned());

        for &(code, ref result) in tests.iter() {
            assert_eq!(
                opcode(code, &labels, &types, &mut funcs),
                *result,
                "parse opcode: {}",
                unsafe { str::from_utf8_unchecked(code) }
            );
        }
    }

    #[test]
    fn parse_offset() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"offset=1234", IResult::Done(&b""[..], 1234)),
            (b"offset=0xABCD", IResult::Done(&b""[..], 0xABCD)),
        ];

        for (code, ref result) in tests {
            assert_eq!(offset(code), *result, "parse offset: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_align() {
        let tests: Vec<(&[u8], _)> = vec![(b"align=8", IResult::Done(&b""[..], 8))];

        for (code, ref result) in tests {
            assert_eq!(align(code), *result, "parse align: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_load() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"i32.load", IResult::Done(&[][..], Opcode::I32Load(0, 0))),
            (b"i64.load", IResult::Done(&[][..], Opcode::I64Load(0, 0))),
            (b"f32.load", IResult::Done(&[][..], Opcode::F32Load(0, 0))),
            (b"f64.load", IResult::Done(&[][..], Opcode::F64Load(0, 0))),
            (
                b"i32.load8_s",
                IResult::Done(&[][..], Opcode::I32Load8S(0, 0)),
            ),
            (
                b"i32.load8_u",
                IResult::Done(&[][..], Opcode::I32Load8U(0, 0)),
            ),
            (
                b"i32.load16_s",
                IResult::Done(&[][..], Opcode::I32Load16S(0, 0)),
            ),
            (
                b"i32.load16_u",
                IResult::Done(&[][..], Opcode::I32Load16U(0, 0)),
            ),
            (
                b"i64.load8_s",
                IResult::Done(&[][..], Opcode::I64Load8S(0, 0)),
            ),
            (
                b"i64.load8_u",
                IResult::Done(&[][..], Opcode::I64Load8U(0, 0)),
            ),
            (
                b"i64.load16_s",
                IResult::Done(&[][..], Opcode::I64Load16S(0, 0)),
            ),
            (
                b"i64.load16_u",
                IResult::Done(&[][..], Opcode::I64Load16U(0, 0)),
            ),
            (
                b"i64.load32_s",
                IResult::Done(&[][..], Opcode::I64Load32S(0, 0)),
            ),
            (
                b"i64.load32_u",
                IResult::Done(&[][..], Opcode::I64Load32U(0, 0)),
            ),
            (
                b"i64.load offset=4",
                IResult::Done(&[][..], Opcode::I64Load(0, 4)),
            ),
            (
                b"i32.load8_s align=8",
                IResult::Done(&[][..], Opcode::I32Load8S(8, 0)),
            ),
            (
                b"i32.load8_u offset=4 align=8",
                IResult::Done(&[][..], Opcode::I32Load8U(8, 4)),
            ),
            (b"i32.load8_s align=3", IResult::Error(ErrorKind::Verify)),
        ];

        for &(code, ref result) in tests.iter() {
            assert_eq!(load(code), *result, "parse opcode: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_store() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"i32.store", IResult::Done(&[][..], Opcode::I32Store(0, 0))),
            (b"i64.store", IResult::Done(&[][..], Opcode::I64Store(0, 0))),
            (b"f32.store", IResult::Done(&[][..], Opcode::F32Store(0, 0))),
            (b"f64.store", IResult::Done(&[][..], Opcode::F64Store(0, 0))),
            (
                b"i32.store8",
                IResult::Done(&[][..], Opcode::I32Store8(0, 0)),
            ),
            (
                b"i32.store16",
                IResult::Done(&[][..], Opcode::I32Store16(0, 0)),
            ),
            (
                b"i64.store8 offset=4",
                IResult::Done(&[][..], Opcode::I64Store8(0, 4)),
            ),
            (
                b"i64.store16 align=8",
                IResult::Done(&[][..], Opcode::I64Store16(8, 0)),
            ),
            (
                b"i64.store32 offset=4 align=8",
                IResult::Done(&[][..], Opcode::I64Store32(8, 4)),
            ),
            (b"i32.store32 align=3", IResult::Error(ErrorKind::Verify)),
        ];

        for &(code, ref result) in tests.iter() {
            assert_eq!(store(code), *result, "parse opcode: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }
}
