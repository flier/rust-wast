use std::str::{self, FromStr};
use std::usize;

use parity_wasm::elements::NameMap;

use failure::Error;
use parity_wasm::elements::{BlockType, FunctionNameSection, FunctionType, Opcode, Type, TypeSection};

use ast::Var;
use func::{param, result};
use parse::{int_type, name, value_type, var, float32, float64, int32, int64, nat32};

named_args!(
    opcode<'a>(labels: &'a NameMap,
               globals: &'a NameMap,
               locals: &'a NameMap,
               funcs: &'a FunctionNameSection,
               signatures: &'a mut TypeSection)<Opcode>,
    alt!(
        tag!("unreachable") => { |_| Opcode::Unreachable } |
        tag!("nop") => { |_| Opcode::Nop } |

        block |
        loop_ |
        if_ |
        else_ |
        end |
        apply!(br, labels) |
        apply!(br_if, labels) |
        apply!(br_table, labels) |
        tag!("return") => { |_| Opcode::Return } |

        apply!(call, funcs) |
        apply!(call_indirect, funcs, signatures) |

        tag!("drop") => { |_| Opcode::Drop } |
        tag!("select") => { |_| Opcode::Select } |

        apply!(get_local, locals) |
        apply!(set_local, locals) |
        apply!(tee_local, locals) |
        apply!(get_global, globals) |
        apply!(set_global, globals) |

        load |
        store |

        tag!("current_memory") => { |_| Opcode::CurrentMemory(0) } |
        tag!("grow_memory") => { |_| Opcode::GrowMemory(0) } |

        constant |
        test |
        unary |
        binary |
        compare |
        convert
    )
);

named!(
    block<Opcode>,
    map!(
        ws!(tuple!(
            tag!("block"),
            opt!(complete!(name)),
            opt!(complete!(block_type))
        )),
        |(_, name, block_type)| Opcode::Block(block_type.unwrap_or_else(|| BlockType::NoResult))
    )
);

named!(
    loop_<Opcode>,
    map!(
        ws!(tuple!(tag!("loop"), opt!(complete!(name)), opt!(complete!(block_type)))),
        |(_, name, block_type)| Opcode::Loop(block_type.unwrap_or_else(|| BlockType::NoResult))
    )
);

named!(
    if_<Opcode>,
    map!(
        ws!(tuple!(tag!("if"), opt!(complete!(name)), opt!(complete!(block_type)))),
        |(_, name, block_type)| Opcode::If(block_type.unwrap_or_else(|| BlockType::NoResult))
    )
);

named!(
    else_<Opcode>,
    map!(ws!(preceded!(tag!("else"), opt!(complete!(name)))), |name| Opcode::Else)
);

named!(
    end<Opcode>,
    map!(ws!(preceded!(tag!("end"), opt!(complete!(name)))), |name| Opcode::End)
);

named!(
    block_type<BlockType>,
    map!(alt!(result | opt!(value_type)), |block_type| {
        if let Some(value_type) = block_type {
            BlockType::Value(value_type)
        } else {
            BlockType::NoResult
        }
    })
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
    call<'a>(funcs: &'a FunctionNameSection)<Opcode>,
    map_res!(
        ws!(preceded!(tag!("call"), var)),
        |func: Var| func.resolve_ref(funcs.names()).map(Opcode::Call)
    )
);

named_args!(
    call_instr_type<'a>(funcs: &'a FunctionNameSection)<(Option<u32>, Option<FunctionType>)>,
    alt!(
        map_res!(var, |var: Var| var.resolve_ref(funcs.names())) => { |idx| (Some(idx), None) } |
        ws!(pair!(opt!(apply!(type_use, funcs)), opt!(complete!(call_instr_params))))
    )
);

named!(
    call_instr_params<FunctionType>,
    map!(
        ws!(pair!(many0!(param), opt!(complete!(result)))),
        |(params, return_type)| FunctionType::new(
            params.into_iter().flat_map(|types| types).collect(),
            return_type.unwrap_or_default()
        )
    )
);

named_args!(
    call_indirect<'a>(funcs: &'a FunctionNameSection, signatures: &'a mut TypeSection)<Opcode>,
    ws!(preceded!(tag!("call_indirect"), map!(apply!(call_instr_type, funcs),
        |(type_use, func_type): (Option<u32>, Option<FunctionType>)|
            if let Some(idx) = type_use {
                Opcode::CallIndirect(idx, 0)
            } else if let Some(func_type) = func_type {
                let idx = signatures.types().iter().position(|ty| {
                    Type::Function(func_type.clone()) == *ty
                });

                if let Some(idx) = idx {
                    Opcode::CallIndirect(idx as u32, 0)
                } else {
                    let idx = signatures.types().len();

                    signatures.types_mut().push(Type::Function(func_type));

                    Opcode::CallIndirect(idx as u32, 0)
                }
            } else {
                Opcode::CallIndirect(0, 0)
            }
        )
    ))
);

named_args!(
    get_local<'a>(locals: &'a NameMap)<Opcode>,
    map!(
        map_res!(
            ws!(preceded!(tag!("get_local"), var)),
            |var: Var| var.resolve_ref(locals)
        ),
    Opcode::GetLocal)
);

named_args!(
    set_local<'a>(locals: &'a NameMap)<Opcode>,
    map!(
        map_res!(
            ws!(preceded!(tag!("set_local"), var)),
            |var: Var| var.resolve_ref(locals)
        ),
    Opcode::SetLocal)
);

named_args!(
    tee_local<'a>(locals: &'a NameMap)<Opcode>,
    map!(
        map_res!(
            ws!(preceded!(tag!("tee_local"), var)),
            |var: Var| var.resolve_ref(locals)
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
    pub load<Opcode>,
    ws!(do_parse!(
        op: recognize!(tuple!(value_type, tag!(".load"), opt!(complete!(tuple!(mem_size, tag!("_"), sign))))) >>
        offset: map!(opt!(complete!(offset)), |n| n.unwrap_or_default()) >>
        align: verify!(
            map!(opt!(complete!(align)), |n| n.unwrap_or_default()),
            |n: u32| n == 0 || n.is_power_of_two()
        ) >>
        opcode: switch!(value!(op),
            b"i32.load" => value!(Opcode::I32Load(align, offset)) |
            b"i64.load" => value!(Opcode::I64Load(align, offset)) |
            b"f32.load" => value!(Opcode::F32Load(align, offset)) |
            b"f64.load" => value!(Opcode::F64Load(align, offset)) |
            b"i32.load8_s" => value!(Opcode::I32Load8S(align, offset)) |
            b"i32.load8_u" => value!(Opcode::I32Load8U(align, offset)) |
            b"i32.load16_s" => value!(Opcode::I32Load16S(align, offset)) |
            b"i32.load16_u" => value!(Opcode::I32Load16U(align, offset)) |
            b"i64.load8_s" => value!(Opcode::I64Load8S(align, offset)) |
            b"i64.load8_u" => value!(Opcode::I64Load8U(align, offset)) |
            b"i64.load16_s" => value!(Opcode::I64Load16S(align, offset)) |
            b"i64.load16_u" => value!(Opcode::I64Load16U(align, offset)) |
            b"i64.load32_s" => value!(Opcode::I64Load32S(align, offset)) |
            b"i64.load32_u" => value!(Opcode::I64Load32U(align, offset))
        ) >>
        ( opcode )
    ))
);

/// <val_type>.store(8|16|32)? <offset>? <align>?
#[cfg_attr(rustfmt, rustfmt_skip)]
named!(
    pub store<Opcode>,
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
        opcode: switch!(value!(op),
            b"i32.store" => value!(Opcode::I32Store(align, offset)) |
            b"i64.store" => value!(Opcode::I64Store(align, offset)) |
            b"f32.store" => value!(Opcode::F32Store(align, offset)) |
            b"f64.store" => value!(Opcode::F64Store(align, offset)) |
            b"i32.store8" => value!(Opcode::I32Store8(align, offset)) |
            b"i32.store16" => value!(Opcode::I32Store16(align, offset)) |
            b"i64.store8" => value!(Opcode::I64Store8(align, offset)) |
            b"i64.store16" => value!(Opcode::I64Store16(align, offset)) |
            b"i64.store32" => value!(Opcode::I64Store32(align, offset))
        ) >>
        ( opcode )
    ))
);

/// <val_type>.const <value>
named!(
    pub constant<Opcode>,
    ws!(switch!(recognize!(pair!(value_type, tag!(".const"))),
            b"i32.const" => map!(int32, |n| Opcode::I32Const(n)) |
            b"i64.const" => map!(int64, |n| Opcode::I64Const(n)) |
            b"f32.const" => map!(float32, |v| Opcode::F32Const(v as u32)) |
            b"f64.const" => map!(float64, |v| Opcode::F64Const(v as u64))
        ))
);

#[cfg(test)]
mod tests {
    use std::str;

    use nom::Err::*;
    use nom::ErrorKind::*;
    use nom::IResult::*;
    use parity_wasm::elements::ValueType;
    use pretty_env_logger;

    use super::*;
    use errors::Parsing::*;

    #[test]
    fn parse_simple_opcode() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"unreachable", Done(&[][..], Opcode::Unreachable)),
            (b"nop", Done(&[][..], Opcode::Nop)),
            (b"return", Done(&[][..], Opcode::Return)),
            (b"drop", Done(&[][..], Opcode::Drop)),
            (b"select", Done(&[][..], Opcode::Select)),
            (b"current_memory", Done(&[][..], Opcode::CurrentMemory(0))),
            (b"grow_memory", Done(&[][..], Opcode::GrowMemory(0))),
        ];
        let labels = NameMap::default();
        let globals = NameMap::default();
        let locals = NameMap::default();
        let funcs = FunctionNameSection::default();
        let mut signatures = TypeSection::with_types(vec![]);

        for &(code, ref result) in tests.iter() {
            assert_eq!(
                opcode(code, &labels, &globals, &locals, &funcs, &mut signatures),
                *result,
                "parse opcode: {}",
                unsafe { str::from_utf8_unchecked(code) }
            );
        }
    }

    #[test]
    fn parse_block() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"block", Done(&[][..], Opcode::Block(BlockType::NoResult))),
            (b"block $exit", Done(&[][..], Opcode::Block(BlockType::NoResult))),
            (
                b"block (result i32)",
                Done(&[][..], Opcode::Block(BlockType::Value(ValueType::I32))),
            ),
            (
                b"block $done (result i32)",
                Done(&[][..], Opcode::Block(BlockType::Value(ValueType::I32))),
            ),
            (
                b"block i32",
                Done(&[][..], Opcode::Block(BlockType::Value(ValueType::I32))),
            ),
        ];

        for &(code, ref result) in tests.iter() {
            assert_eq!(block(code), *result, "parse opcode: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_loop() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"loop", Done(&[][..], Opcode::Loop(BlockType::NoResult))),
            (b"loop $exit", Done(&[][..], Opcode::Loop(BlockType::NoResult))),
            (
                b"loop (result i32)",
                Done(&[][..], Opcode::Loop(BlockType::Value(ValueType::I32))),
            ),
            (
                b"loop $done (result i32)",
                Done(&[][..], Opcode::Loop(BlockType::Value(ValueType::I32))),
            ),
            (
                b"loop i32",
                Done(&[][..], Opcode::Loop(BlockType::Value(ValueType::I32))),
            ),
        ];

        for &(code, ref result) in tests.iter() {
            assert_eq!(loop_(code), *result, "parse opcode: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_if() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"if", Done(&[][..], Opcode::If(BlockType::NoResult))),
            (b"if $exit", Done(&[][..], Opcode::If(BlockType::NoResult))),
            (
                b"if (result i32)",
                Done(&[][..], Opcode::If(BlockType::Value(ValueType::I32))),
            ),
            (
                b"if $done (result i32)",
                Done(&[][..], Opcode::If(BlockType::Value(ValueType::I32))),
            ),
            (b"if i32", Done(&[][..], Opcode::If(BlockType::Value(ValueType::I32)))),
        ];

        for &(code, ref result) in tests.iter() {
            assert_eq!(if_(code), *result, "parse opcode: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_else() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"else", Done(&[][..], Opcode::Else)),
            (b"else $exit", Done(&[][..], Opcode::Else)),
        ];

        for &(code, ref result) in tests.iter() {
            assert_eq!(else_(code), *result, "parse opcode: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_end() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"end", Done(&[][..], Opcode::End)),
            (b"end $exit", Done(&[][..], Opcode::End)),
        ];

        for &(code, ref result) in tests.iter() {
            assert_eq!(end(code), *result, "parse opcode: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_br() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"br 0", Done(&[][..], Opcode::Br(0))),
            (b"br $done", Done(&[][..], Opcode::Br(123))),
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
            (b"br_if 0", Done(&[][..], Opcode::BrIf(0))),
            (b"br_if $done", Done(&[][..], Opcode::BrIf(123))),
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
            (b"br_table 0", Done(&[][..], Opcode::BrTable(Box::new([]), 0))),
            (b"br_table 0 0", Done(&[][..], Opcode::BrTable(Box::new([0]), 0))),
            (
                b"br_table 3 2 1 0 4",
                Done(&[][..], Opcode::BrTable(Box::new([3, 2, 1, 0]), 4)),
            ),
        ];
        let labels = NameMap::default();

        for &(code, ref result) in tests.iter() {
            assert_eq!(br_table(code, &labels), *result, "parse opcode: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_call() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"call 0", Done(&[][..], Opcode::Call(0))),
            (b"call $hello", Done(&[][..], Opcode::Call(123))),
        ];
        let mut funcs = FunctionNameSection::default();

        funcs.names_mut().insert(123, "hello".to_owned());

        for &(code, ref result) in tests.iter() {
            assert_eq!(call(code, &funcs), *result, "parse opcode: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_type_use() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"(type 123)", Done(&[][..], 123)),
            (b"(type $hello)", Done(&[][..], 123)),
        ];
        let mut funcs = FunctionNameSection::default();

        funcs.names_mut().insert(123, "hello".to_owned());

        for &(code, ref result) in tests.iter() {
            assert_eq!(type_use(code, &funcs), *result, "parse opcode: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_call_indirect() {
        let call_indirect_tests: Vec<(&[u8], _)> = vec![
            (b"call_indirect 123", Done(&[][..], Opcode::CallIndirect(123, 0))),
            (b"call_indirect $hello", Done(&[][..], Opcode::CallIndirect(123, 0))),
            (
                b"call_indirect (type $hello)",
                Done(&[][..], Opcode::CallIndirect(123, 0)),
            ),
            (b"call_indirect (param i32)", Done(&[][..], Opcode::CallIndirect(0, 0))),
            (b"call_indirect (param i64)", Done(&[][..], Opcode::CallIndirect(1, 0))),
        ];
        let mut funcs = FunctionNameSection::default();
        let mut signatures =
            TypeSection::with_types(vec![Type::Function(FunctionType::new(vec![ValueType::I32], None))]);

        funcs.names_mut().insert(123, "hello".to_owned());

        for &(code, ref result) in call_indirect_tests.iter() {
            assert_eq!(
                call_indirect(code, &funcs, &mut signatures),
                *result,
                "parse opcode: {}",
                unsafe { str::from_utf8_unchecked(code) }
            );
        }

        assert_eq!(
            signatures.types(),
            &[
                Type::Function(FunctionType::new(vec![ValueType::I32], None)),
                Type::Function(FunctionType::new(vec![ValueType::I64], None)),
            ]
        );
    }

    #[test]
    fn parse_get_local() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"get_local 0", Done(&[][..], Opcode::GetLocal(0))),
            (b"get_local $l", Done(&[][..], Opcode::GetLocal(123))),
            (b"set_local 0", Done(&[][..], Opcode::SetLocal(0))),
            (b"set_local $l", Done(&[][..], Opcode::SetLocal(123))),
            (b"tee_local 0", Done(&[][..], Opcode::TeeLocal(0))),
            (b"tee_local $l", Done(&[][..], Opcode::TeeLocal(123))),
            (b"get_global 0", Done(&[][..], Opcode::GetGlobal(0))),
            (b"get_global $g", Done(&[][..], Opcode::GetGlobal(123))),
            (b"set_global 0", Done(&[][..], Opcode::SetGlobal(0))),
            (b"set_global $g", Done(&[][..], Opcode::SetGlobal(123))),
        ];
        let labels = NameMap::default();
        let mut globals = NameMap::default();
        let mut locals = NameMap::default();
        let funcs = FunctionNameSection::default();
        let mut signatures = TypeSection::with_types(vec![]);

        globals.insert(123, "g".to_owned());
        locals.insert(123, "l".to_owned());

        for &(code, ref result) in tests.iter() {
            assert_eq!(
                opcode(code, &labels, &globals, &locals, &funcs, &mut signatures),
                *result,
                "parse opcode: {}",
                unsafe { str::from_utf8_unchecked(code) }
            );
        }
    }

    #[test]
    fn parse_offset() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"offset=1234", Done(&b""[..], 1234)),
            (b"offset=0xABCD", Done(&b""[..], 0xABCD)),
        ];

        for (code, ref result) in tests {
            assert_eq!(offset(code), *result, "parse offset: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_align() {
        let tests: Vec<(&[u8], _)> = vec![(b"align=8", Done(&b""[..], 8))];

        for (code, ref result) in tests {
            assert_eq!(align(code), *result, "parse align: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_load() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"i32.load", Done(&[][..], Opcode::I32Load(0, 0))),
            (b"i64.load", Done(&[][..], Opcode::I64Load(0, 0))),
            (b"f32.load", Done(&[][..], Opcode::F32Load(0, 0))),
            (b"f64.load", Done(&[][..], Opcode::F64Load(0, 0))),
            (b"i32.load8_s", Done(&[][..], Opcode::I32Load8S(0, 0))),
            (b"i32.load8_u", Done(&[][..], Opcode::I32Load8U(0, 0))),
            (b"i32.load16_s", Done(&[][..], Opcode::I32Load16S(0, 0))),
            (b"i32.load16_u", Done(&[][..], Opcode::I32Load16U(0, 0))),
            (b"i64.load8_s", Done(&[][..], Opcode::I64Load8S(0, 0))),
            (b"i64.load8_u", Done(&[][..], Opcode::I64Load8U(0, 0))),
            (b"i64.load16_s", Done(&[][..], Opcode::I64Load16S(0, 0))),
            (b"i64.load16_u", Done(&[][..], Opcode::I64Load16U(0, 0))),
            (b"i64.load32_s", Done(&[][..], Opcode::I64Load32S(0, 0))),
            (b"i64.load32_u", Done(&[][..], Opcode::I64Load32U(0, 0))),
            (b"i64.load offset=4", Done(&[][..], Opcode::I64Load(0, 4))),
            (b"i32.load8_s align=8", Done(&[][..], Opcode::I32Load8S(8, 0))),
            (b"i32.load8_u offset=4 align=8", Done(&[][..], Opcode::I32Load8U(8, 4))),
            (b"i32.load8_s align=3", Error(Position(Verify, &b"align=3"[..]))),
            (b"f32.load8_s", Error(Position(Switch, &b""[..]))),
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
            (b"i32.store", Done(&[][..], Opcode::I32Store(0, 0))),
            (b"i64.store", Done(&[][..], Opcode::I64Store(0, 0))),
            (b"f32.store", Done(&[][..], Opcode::F32Store(0, 0))),
            (b"f64.store", Done(&[][..], Opcode::F64Store(0, 0))),
            (b"i32.store8", Done(&[][..], Opcode::I32Store8(0, 0))),
            (b"i32.store16", Done(&[][..], Opcode::I32Store16(0, 0))),
            (b"i64.store8 offset=4", Done(&[][..], Opcode::I64Store8(0, 4))),
            (b"i64.store16 align=8", Done(&[][..], Opcode::I64Store16(8, 0))),
            (b"i64.store32 offset=4 align=8", Done(&[][..], Opcode::I64Store32(8, 4))),
            (b"i32.store32 align=3", Error(Position(Verify, &b"align=3"[..]))),
            (b"f32.store8_s", Error(Position(Switch, &b"_s"[..]))),
        ];

        for &(code, ref result) in tests.iter() {
            assert_eq!(store(code), *result, "parse opcode: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_constant() {
        let tests: Vec<(&[u8], _)> = vec![
            (
                b"i32.const 0xffffffff",
                Error(NodePosition(
                    Switch,
                    &b"i32.const 0xffffffff"[..],
                    vec![
                        Position(Verify, &b"0xffffffff"[..]),
                        Position(Custom(Int32 as u32), &b"0xffffffff"[..]),
                    ],
                )),
            ),
            (b"i32.const -0x80000000", Done(&[][..], Opcode::I32Const(-0x80000000))),
            (
                b"i64.const 18446744073709551615",
                Error(NodePosition(
                    Switch,
                    &b"i64.const 18446744073709551615"[..],
                    vec![
                        Position(Alt, &b"18446744073709551615"[..]),
                        Position(Custom(Integer as u32), &b"18446744073709551615"[..]),
                        Position(Custom(Int64 as u32), &b"18446744073709551615"[..]),
                    ],
                )),
            ),
            (
                b"i64.const -9223372036854775808",
                Error(NodePosition(
                    Switch,
                    &b"i64.const -9223372036854775808"[..],
                    vec![
                        Position(Alt, &b"-9223372036854775808"[..]),
                        Position(Custom(Integer as u32), &b"-9223372036854775808"[..]),
                        Position(Custom(Int64 as u32), &b"-9223372036854775808"[..]),
                    ],
                )),
            ),
            (b"f32.const 0x1p127", Done(&[][..], Opcode::F32Const(0))),
        ];

        for (code, ref result) in tests {
            assert_eq!(constant(code), *result, "parse const: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }
}
