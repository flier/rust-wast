use parity_wasm::elements::NameMap;

use failure::Error;
use parity_wasm::elements::{FunctionType, Opcode, Type, ValueType};

use parse::{const_type, value_type, value_type_list, var, Var, float32, float64, int32, int64};

named_args!(
    opcode<'a>(labels: &'a NameMap, funcs: &'a NameMap)<Opcode>,
    alt_complete!(
            tag!("unreachable") => { |_| Opcode::Unreachable } |
            tag!("nop") => { |_| Opcode::Nop } |
            tag!("return") => { |_| Opcode::Return } |
            tag!("drop") => { |_| Opcode::Drop } |
            tag!("select") => { |_| Opcode::Select } |
            apply!(br, labels) |
            apply!(br_if, labels) |
            apply!(br_table, labels) |
            apply!(call, funcs) |
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

#[cfg(test)]
mod tests {
    use std::str;

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
        let funcs = NameMap::default();

        for &(code, ref result) in tests.iter() {
            assert_eq!(
                opcode(code, &labels, &funcs),
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
}
