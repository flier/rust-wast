use std::str::{self, FromStr};
use std::usize;

use parity_wasm::elements::NameMap;

use failure::Error;
use parity_wasm::elements::{BlockType, FunctionNameSection, FunctionType, Opcode, Type,
                            TypeSection};

use parse::{int_type, name, value_type, var, Var, float32, float64, int32, int64, nat32};
use func::{param, result};

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
        ws!(tuple!(
            tag!("loop"),
            opt!(complete!(name)),
            opt!(complete!(block_type))
        )),
        |(_, name, block_type)| Opcode::Loop(block_type.unwrap_or_else(|| BlockType::NoResult))
    )
);

named!(
    if_<Opcode>,
    map!(
        ws!(tuple!(
            tag!("if"),
            opt!(complete!(name)),
            opt!(complete!(block_type))
        )),
        |(_, name, block_type)| Opcode::If(block_type.unwrap_or_else(|| BlockType::NoResult))
    )
);

named!(
    else_<Opcode>,
    map!(
        ws!(preceded!(tag!("else"), opt!(complete!(name)))),
        |name| Opcode::Else
    )
);

named!(
    end<Opcode>,
    map!(ws!(preceded!(tag!("end"), opt!(complete!(name)))), |name| {
        Opcode::End
    })
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
    pub type_use<'a>(funcs: &'a FunctionNameSection)<u32>,
    ws!(delimited!(
        tag!("("),
        preceded!(
            tag!("type"),
            map_res!(var, |var: Var| var.resolve_ref(funcs.names()))
        ),
        tag!(")")
    ))
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
                if let Some(idx) = signatures.types().iter().position(|ty| { Type::Function(func_type.clone()) == *ty }) {
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
    load<Opcode>,
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

/// <val_type>.const <value>
named!(
    constant<Opcode>,
    ws!(switch!(recognize!(pair!(value_type, tag!(".const"))),
            b"i32.const" => map!(int32, |n| Opcode::I32Const(n)) |
            b"i64.const" => map!(int64, |n| Opcode::I64Const(n)) |
            b"f32.const" => map!(float32, |v| Opcode::F32Const(v as u32)) |
            b"f64.const" => map!(float64, |v| Opcode::F64Const(v as u64))
        ))
);

// <val_type>.<testop>
named!(
    test<Opcode>,
    switch!(recognize!(tuple!(int_type, tag!("."), testop)),
        b"i32.eqz" => value!(Opcode::I32Eqz) |
        b"i64.eqz" => value!(Opcode::I64Eqz)
    )
);

named!(testop, tag!("eqz"));

/// <val_type>.<unop>
named!(
    unary<Opcode>,
    switch!(recognize!(tuple!(int_type, tag!("."), unop)),
        b"i32.clz" => value!(Opcode::I32Clz) |
        b"i32.ctz" => value!(Opcode::I32Ctz) |
        b"i32.popcnt" => value!(Opcode::I32Popcnt) |

        b"i64.clz" => value!(Opcode::I64Clz) |
        b"i64.ctz" => value!(Opcode::I64Ctz) |
        b"i64.popcnt" => value!(Opcode::I64Popcnt) |

        b"f32.abs" => value!(Opcode::F32Abs) |
		b"f32.neg" => value!(Opcode::F32Neg) |
        b"f32.sqrt" => value!(Opcode::F32Sqrt ) |
		b"f32.ceil" => value!(Opcode::F32Ceil ) |
		b"f32.floor" => value!(Opcode::F32Floor ) |
		b"f32.trunc" => value!(Opcode::F32Trunc ) |
		b"f32.nearest" => value!(Opcode::F32Nearest ) |

        b"f64.abs" => value!(Opcode::F64Abs) |
		b"f64.neg" => value!(Opcode::F64Neg) |
		b"f64.sqrt" => value!(Opcode::F64Sqrt ) |
		b"f64.ceil" => value!(Opcode::F64Ceil ) |
		b"f64.floor" => value!(Opcode::F64Floor ) |
		b"f64.trunc" => value!(Opcode::F64Trunc ) |
		b"f64.nearest" => value!(Opcode::F64Nearest )
    )
);

named!(
    unop,
    alt!(
        tag!("clz") | tag!("ctz") | tag!("popcnt") | tag!("abs") | tag!("neg") | tag!("sqrt")
            | tag!("ceil") | tag!("floor") | tag!("trunc") | tag!("nearest")
    )
);

/// <val_type>.<binop>
named!(
    binary<Opcode>,
    alt!(
        tag!("i32.add") => { |_| Opcode::I32Add } |
		tag!("i32.sub") => { |_| Opcode::I32Sub } |
		tag!("i32.mul") => { |_| Opcode::I32Mul } |
		tag!("i32.div_s") => { |_| Opcode::I32DivS } |
		tag!("i32.div_u") => { |_| Opcode::I32DivU } |
		tag!("i32.rem_s") => { |_| Opcode::I32RemS } |
		tag!("i32.rem_u") => { |_| Opcode::I32RemU } |
		tag!("i32.and") => { |_| Opcode::I32And } |
		tag!("i32.or") => { |_| Opcode::I32Or } |
		tag!("i32.xor") => { |_| Opcode::I32Xor } |
		tag!("i32.shl") => { |_| Opcode::I32Shl } |
		tag!("i32.shr_s") => { |_| Opcode::I32ShrS } |
		tag!("i32.shr_u") => { |_| Opcode::I32ShrU } |
		tag!("i32.rotl") => { |_| Opcode::I32Rotl } |
		tag!("i32.rotr") => { |_| Opcode::I32Rotr } |

        tag!("i64.add") => { |_| Opcode::I64Add } |
		tag!("i64.sub") => { |_| Opcode::I64Sub } |
		tag!("i64.mul") => { |_| Opcode::I64Mul } |
		tag!("i64.div_s") => { |_| Opcode::I64DivS } |
		tag!("i64.div_u") => { |_| Opcode::I64DivU } |
		tag!("i64.rem_s") => { |_| Opcode::I64RemS } |
		tag!("i64.rem_u") => { |_| Opcode::I64RemU } |
		tag!("i64.and") => { |_| Opcode::I64And } |
		tag!("i64.or") => { |_| Opcode::I64Or } |
		tag!("i64.xor") => { |_| Opcode::I64Xor } |
		tag!("i64.shl") => { |_| Opcode::I64Shl } |
		tag!("i64.shr_s") => { |_| Opcode::I64ShrS } |
		tag!("i64.shr_u") => { |_| Opcode::I64ShrU } |
		tag!("i64.rotl") => { |_| Opcode::I64Rotl } |
		tag!("i64.rotr") => { |_| Opcode::I64Rotr } |

		tag!("f32.add") => { |_| Opcode::F32Add } |
		tag!("f32.sub") => { |_| Opcode::F32Sub } |
		tag!("f32.mul") => { |_| Opcode::F32Mul } |
		tag!("f32.div") => { |_| Opcode::F32Div } |
		tag!("f32.min") => { |_| Opcode::F32Min } |
		tag!("f32.max") => { |_| Opcode::F32Max } |
		tag!("f32.copysign") => { |_| Opcode::F32Copysign } |

		tag!("f64.add") => { |_| Opcode::F64Add } |
		tag!("f64.sub") => { |_| Opcode::F64Sub } |
		tag!("f64.mul") => { |_| Opcode::F64Mul } |
		tag!("f64.div") => { |_| Opcode::F64Div } |
		tag!("f64.min") => { |_| Opcode::F64Min } |
		tag!("f64.max") => { |_| Opcode::F64Max } |
		tag!("f64.copysign") => { |_| Opcode::F64Copysign }
    )
);

///<val_type>.<relop>
named!(
    compare<Opcode>,
    alt!(
        tag!("i32.eq") => { |_| Opcode::I32Eq } |
		tag!("i32.ne") => { |_| Opcode::I32Ne } |
		tag!("i32.lt_s") => { |_| Opcode::I32LtS } |
		tag!("i32.lt_u") => { |_| Opcode::I32LtU } |
		tag!("i32.gt_s") => { |_| Opcode::I32GtS } |
		tag!("i32.gt_u") => { |_| Opcode::I32GtU } |
		tag!("i32.le_s") => { |_| Opcode::I32LeS } |
		tag!("i32.le_u") => { |_| Opcode::I32LeU } |
		tag!("i32.ge_s") => { |_| Opcode::I32GeS } |
		tag!("i32.ge_u") => { |_| Opcode::I32GeU } |

        tag!("i64.eq") => { |_| Opcode::I64Eq } |
		tag!("i64.ne") => { |_| Opcode::I64Ne } |
		tag!("i64.lt_s") => { |_| Opcode::I64LtS } |
		tag!("i64.lt_u") => { |_| Opcode::I64LtU } |
		tag!("i64.gt_s") => { |_| Opcode::I64GtS } |
		tag!("i64.gt_u") => { |_| Opcode::I64GtU } |
		tag!("i64.le_s") => { |_| Opcode::I64LeS } |
		tag!("i64.le_u") => { |_| Opcode::I64LeU } |
		tag!("i64.ge_s") => { |_| Opcode::I64GeS } |
		tag!("i64.ge_u") => { |_| Opcode::I64GeU } |

        tag!("f32.eq") => { |_| Opcode::F32Eq } |
		tag!("f32.ne") => { |_| Opcode::F32Ne } |
		tag!("f32.lt") => { |_| Opcode::F32Lt } |
        tag!("f32.gt") => { |_| Opcode::F32Gt } |
        tag!("f32.le") => { |_| Opcode::F32Le } |
        tag!("f32.ge") => { |_| Opcode::F32Ge } |

        tag!("f64.eq") => { |_| Opcode::F64Eq } |
		tag!("f64.ne") => { |_| Opcode::F64Ne } |
		tag!("f64.lt") => { |_| Opcode::F64Lt } |
        tag!("f64.gt") => { |_| Opcode::F64Gt } |
        tag!("f64.le") => { |_| Opcode::F64Le } |
        tag!("f64.ge") => { |_| Opcode::F64Ge }
    )
);

/// <val_type>.<cvtop>/<val_type>
named!(
    convert<Opcode>,
    alt!(
		tag!("i32.wrap/i64") => { |_| Opcode::I32WrapI64 } |
		tag!("i32.trunc_s/f32") => { |_| Opcode::I32TruncSF32 } |
		tag!("i32.trunc_u/f32") => { |_| Opcode::I32TruncUF32 } |
		tag!("i32.trunc_s/f64") => { |_| Opcode::I32TruncSF64 } |
		tag!("i32.trunc_u/f64") => { |_| Opcode::I32TruncUF64 } |
		tag!("i64.extend_s/i32") => { |_| Opcode::I64ExtendSI32 } |
		tag!("i64.extend_u/i32") => { |_| Opcode::I64ExtendUI32 } |
		tag!("i64.trunc_s/f32") => { |_| Opcode::I64TruncSF32 } |
		tag!("i64.trunc_u/f32") => { |_| Opcode::I64TruncSF64 } |
		tag!("i64.trunc_s/f64") => { |_| Opcode::I64TruncSF64 } |
		tag!("i64.trunc_u/f64") => { |_| Opcode::I64TruncUF64 } |
		tag!("f32.convert_s/i32") => { |_| Opcode::F32ConvertSI32 } |
		tag!("f32.convert_u/i32") => { |_| Opcode::F32ConvertUI32 } |
		tag!("f32.convert_s/i64") => { |_| Opcode::F32ConvertSI64 } |
		tag!("f32.convert_u/i64") => { |_| Opcode::F32ConvertUI64 } |
		tag!("f32.demote/f64") => { |_| Opcode::F32DemoteF64 } |
		tag!("f64.convert_s/i32") => { |_| Opcode::F64ConvertSI32 } |
		tag!("f64.convert_u/i32") => { |_| Opcode::F64ConvertUI32 } |
		tag!("f64.convert_s/i64") => { |_| Opcode::F64ConvertSI64 } |
		tag!("f64.convert_u/i64") => { |_| Opcode::F64ConvertUI64 } |
		tag!("f64.promote/f32") => { |_| Opcode::F64PromoteF32 } |
		tag!("i32.reinterpret/f32") => { |_| Opcode::I32ReinterpretF32 } |
		tag!("i64.reinterpret/f64") => { |_| Opcode::I64ReinterpretF64 } |
		tag!("f32.reinterpret/i32") => { |_| Opcode::F32ReinterpretI32 } |
		tag!("f64.reinterpret/i64") => { |_| Opcode::F64ReinterpretI64 }
    )
);

#[cfg(test)]
mod tests {
    use std::str;

    use pretty_env_logger;
    use parity_wasm::elements::ValueType;
    use nom::{self, ErrorKind, IResult};

    use super::*;

    #[test]
    fn parse_simple_opcode() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"unreachable", IResult::Done(&[][..], Opcode::Unreachable)),
            (b"nop", IResult::Done(&[][..], Opcode::Nop)),
            (b"return", IResult::Done(&[][..], Opcode::Return)),
            (b"drop", IResult::Done(&[][..], Opcode::Drop)),
            (b"select", IResult::Done(&[][..], Opcode::Select)),
            (
                b"current_memory",
                IResult::Done(&[][..], Opcode::CurrentMemory(0)),
            ),
            (
                b"grow_memory",
                IResult::Done(&[][..], Opcode::GrowMemory(0)),
            ),
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
            (
                b"block",
                IResult::Done(&[][..], Opcode::Block(BlockType::NoResult)),
            ),
            (
                b"block $exit",
                IResult::Done(&[][..], Opcode::Block(BlockType::NoResult)),
            ),
            (
                b"block (result i32)",
                IResult::Done(&[][..], Opcode::Block(BlockType::Value(ValueType::I32))),
            ),
            (
                b"block $done (result i32)",
                IResult::Done(&[][..], Opcode::Block(BlockType::Value(ValueType::I32))),
            ),
            (
                b"block i32",
                IResult::Done(&[][..], Opcode::Block(BlockType::Value(ValueType::I32))),
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
            (
                b"loop",
                IResult::Done(&[][..], Opcode::Loop(BlockType::NoResult)),
            ),
            (
                b"loop $exit",
                IResult::Done(&[][..], Opcode::Loop(BlockType::NoResult)),
            ),
            (
                b"loop (result i32)",
                IResult::Done(&[][..], Opcode::Loop(BlockType::Value(ValueType::I32))),
            ),
            (
                b"loop $done (result i32)",
                IResult::Done(&[][..], Opcode::Loop(BlockType::Value(ValueType::I32))),
            ),
            (
                b"loop i32",
                IResult::Done(&[][..], Opcode::Loop(BlockType::Value(ValueType::I32))),
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
            (
                b"if",
                IResult::Done(&[][..], Opcode::If(BlockType::NoResult)),
            ),
            (
                b"if $exit",
                IResult::Done(&[][..], Opcode::If(BlockType::NoResult)),
            ),
            (
                b"if (result i32)",
                IResult::Done(&[][..], Opcode::If(BlockType::Value(ValueType::I32))),
            ),
            (
                b"if $done (result i32)",
                IResult::Done(&[][..], Opcode::If(BlockType::Value(ValueType::I32))),
            ),
            (
                b"if i32",
                IResult::Done(&[][..], Opcode::If(BlockType::Value(ValueType::I32))),
            ),
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
            (b"else", IResult::Done(&[][..], Opcode::Else)),
            (b"else $exit", IResult::Done(&[][..], Opcode::Else)),
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
            (b"end", IResult::Done(&[][..], Opcode::End)),
            (b"end $exit", IResult::Done(&[][..], Opcode::End)),
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
            (b"(type 123)", IResult::Done(&[][..], 123)),
            (b"(type $hello)", IResult::Done(&[][..], 123)),
        ];
        let mut funcs = FunctionNameSection::default();

        funcs.names_mut().insert(123, "hello".to_owned());

        for &(code, ref result) in tests.iter() {
            assert_eq!(
                type_use(code, &funcs),
                *result,
                "parse opcode: {}",
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
        let mut funcs = FunctionNameSection::default();
        let mut signatures = TypeSection::with_types(vec![
            Type::Function(FunctionType::new(vec![ValueType::I32], None)),
        ]);

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
            (b"get_local 0", IResult::Done(&[][..], Opcode::GetLocal(0))),
            (
                b"get_local $l",
                IResult::Done(&[][..], Opcode::GetLocal(123)),
            ),
            (b"set_local 0", IResult::Done(&[][..], Opcode::SetLocal(0))),
            (
                b"set_local $l",
                IResult::Done(&[][..], Opcode::SetLocal(123)),
            ),
            (b"tee_local 0", IResult::Done(&[][..], Opcode::TeeLocal(0))),
            (
                b"tee_local $l",
                IResult::Done(&[][..], Opcode::TeeLocal(123)),
            ),
            (
                b"get_global 0",
                IResult::Done(&[][..], Opcode::GetGlobal(0)),
            ),
            (
                b"get_global $g",
                IResult::Done(&[][..], Opcode::GetGlobal(123)),
            ),
            (
                b"set_global 0",
                IResult::Done(&[][..], Opcode::SetGlobal(0)),
            ),
            (
                b"set_global $g",
                IResult::Done(&[][..], Opcode::SetGlobal(123)),
            ),
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
            (
                b"i32.load8_s align=3",
                IResult::Error(nom::Err::Position(ErrorKind::Verify, &b"align=3"[..])),
            ),
            (
                b"f32.load8_s",
                IResult::Error(nom::Err::Position(ErrorKind::Switch, &b""[..])),
            ),
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
            (
                b"i32.store32 align=3",
                IResult::Error(nom::Err::Position(ErrorKind::Verify, &b"align=3"[..])),
            ),
            (
                b"f32.store8_s",
                IResult::Error(nom::Err::Position(ErrorKind::Switch, &b"_s"[..])),
            ),
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
                IResult::Error(nom::Err::NodePosition(
                    ErrorKind::Switch,
                    &b"i32.const 0xffffffff"[..],
                    vec![nom::Err::Position(ErrorKind::Verify, &b"0xffffffff"[..])],
                )),
            ),
            (
                b"i32.const -0x80000000",
                IResult::Done(&[][..], Opcode::I32Const(-0x80000000)),
            ),
            (
                b"i64.const 18446744073709551615",
                IResult::Error(nom::Err::NodePosition(
                    ErrorKind::Switch,
                    &b"i64.const 18446744073709551615"[..],
                    vec![
                        nom::Err::Position(ErrorKind::Alt, &b"18446744073709551615"[..]),
                    ],
                )),
            ),
            (
                b"i64.const -9223372036854775808",
                IResult::Error(nom::Err::NodePosition(
                    ErrorKind::Switch,
                    &b"i64.const -9223372036854775808"[..],
                    vec![
                        nom::Err::Position(ErrorKind::Alt, &b"-9223372036854775808"[..]),
                    ],
                )),
            ),
            (
                b"f32.const 0x1p127",
                IResult::Done(&[][..], Opcode::F32Const(0)),
            ),
        ];

        for (code, ref result) in tests {
            assert_eq!(constant(code), *result, "parse const: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }
}
