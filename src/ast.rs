use itertools;
use parity_wasm::elements::{BlockType, FunctionNameSection, FunctionType, NameMap, Opcode, Type,
                            TypeSection, ValueType};

use parse::{value_type, var, FunctionTypeExt, TypeSectionExt, Value, Var};
use func::func_type;

#[derive(Clone, Debug, PartialEq)]
pub enum Instr {
    /// trap unconditionally
    Unreachable,
    /// do nothing
    Nop,
    /// execute in sequence
    Block(BlockType, Vec<Instr>),
    /// loop header
    Loop(BlockType, Vec<Instr>),
    /// conditional
    If(BlockType, Vec<Instr>, Vec<Instr>),
    /// break to n-th surrounding label
    Br(Var),
    /// conditional break
    BrIf(Var),
    /// indexed break
    BrTable(Vec<Var>, Var),
    /// break from function body
    Return,
    /// call function
    Call(Var),
    /// call function through table
    CallIndirect(Var),
    /// forget a value
    Drop,
    /// branchless conditional
    Select,
    /// read local variable
    GetLocal(Var),
    /// write local variable
    SetLocal(Var),
    /// write local variable and keep value
    TeeLocal(Var),
    /// read global variable
    GetGlobal(Var),
    /// write global variable
    SetGlobal(Var),
    /// read memory at address
    Load(Opcode),
    /// write memory at address
    Store(Opcode),
    /// size of linear memory
    CurrentMemory,
    /// grow linear memory
    GrowMemory,
    /// constant
    Const(Value),
    /// numeric test
    Test(Opcode),
    /// numeric comparison
    Compare(Opcode),
    /// unary numeric operator
    Unary(Opcode),
    /// binary numeric operator
    Binary(Opcode),
    /// conversion
    Convert(Opcode),
}

#[derive(Clone, Debug, Default)]
pub struct Context {
    pub types: TypeSection,
    pub tables: NameMap,
    pub memories: NameMap,
    pub funcs: FunctionNameSection,
    pub locals: NameMap,
    pub globals: NameMap,
    pub labels: NameMap,
}

named_args!(
    instr_list<'a>(ctxt: &'a mut Context)<Vec<Instr>>,
    map!(ws!(many0!(apply!(instr, ctxt))), |instrs| itertools::flatten(instrs).collect())
);

named_args!(
    instr<'a>(ctxt: &'a mut Context)<Vec<Instr>>,
    parsing!(Instr,
        ws!(alt!(
            plain_instr => { |instr| vec![instr] } |
            apply!(call_instr, ctxt) => { |instr| vec![instr] } |
            apply!(block_instr, ctxt) => { |instr| vec![instr] } |
            apply!(expr, ctxt)
        ))
    )
);

named!(
    plain_instr<Instr>,
    parsing!(
        PlainInstr,
        alt_complete!(
            tag!("unreachable") => { |_| Instr::Unreachable } |
            tag!("nop") => { |_| Instr::Nop } |
            ws!(preceded!(tag!("br"), var)) => { |var| Instr::Br(var) } |
            ws!(preceded!(tag!("br_if"), var)) => { |var| Instr::BrIf(var) } |
            ws!(preceded!(tag!("br_table"), pair!(var, many0!(var)))) => {
                |(default, targets)| Instr::BrTable(targets, default)
            } |
            tag!("return") => { |_| Instr::Return } |
            ws!(preceded!(tag!("call"), var)) => { |var| Instr::Call(var) } |
            tag!("drop") => { |_| Instr::Drop } |
            tag!("select") => { |_| Instr::Select } |
            ws!(preceded!(tag!("get_local"), var)) => { |var| Instr::GetLocal(var) } |
            ws!(preceded!(tag!("set_local"), var)) => { |var| Instr::SetLocal(var) } |
            ws!(preceded!(tag!("tee_local"), var)) => { |var| Instr::TeeLocal(var) } |
            ws!(preceded!(tag!("get_global"), var)) => { |var| Instr::GetGlobal(var) } |
            ws!(preceded!(tag!("set_global"), var)) => { |var| Instr::SetGlobal(var) } |
            tag!("current_memory") => { |_| Instr::CurrentMemory } |
            tag!("grow_memory") => { |_| Instr::GrowMemory }
        )
    )
);

named_args!(
    call_instr<'a>(ctxt: &'a mut Context)<Instr>,
    ws!(preceded!(tag!("call_indirect"), apply!(call_instr_type, ctxt)))
);

named_args!(
    call_instr_type<'a>(ctxt: &'a mut Context)<Instr>,
    alt!(
        map!(var, |var| Instr::CallIndirect(var)) |
        map_res!(
            ws!(pair!(opt!(type_use), func_type)),
            |(type_use, func_type): (Option<Var>, FunctionType)| {
                trace!("call_indirect (type {:?}) {:?}", type_use, func_type);

                match type_use {
                    Some(var) => {
                        if !func_type.is_empty() &&
                           ctxt.types.types()
                                .get(var.resolve_ref(ctxt.funcs.names())? as usize)
                                .map_or(true, |ty| *ty == Type::Function(func_type))
                        {
                            bail!("inline function type does not match explicit type")
                        }

                        Ok(Instr::CallIndirect(var))
                    },
                    None => {
                        Ok(Instr::CallIndirect(Var::Index(ctxt.types.get_or_insert(func_type) as u32)))
                    }
                }
            }
        )
    )
);

named!(
    type_use<Var>,
    ws!(delimited!(
        tag!("("),
        preceded!(tag!("type"), var),
        tag!(")")
    ))
);

// named!(call_instr_instr<Instr>, tag!(""));

named_args!(
    block_instr<'a>(ctxt: &'a mut Context)<Instr>,
    parsing!(BlockInstr,
        ws!(alt_complete!(
            tuple!(tag!("block"), opt!(var), apply!(block, ctxt), tag!("end"), opt!(var)) => {
                |(_, _, (block_type, instrs), _, _)| Instr::Block(block_type, instrs)
            } |
            tuple!(tag!("loop"), opt!(var), apply!(block, ctxt), tag!("end"), opt!(var)) => {
                |(_, _, (block_type, instrs), _, _)| Instr::Loop(block_type, instrs)
            } |
            tuple!(tag!("if"), opt!(var), apply!(block, ctxt), tag!("end"), opt!(var)) => {
                |(_, _, (block_type, then), _, _)| Instr::If(block_type, then, vec![])
            } |
            tuple!(tag!("if"), opt!(var), apply!(block, ctxt), tag!("else"), opt!(var), apply!(instr_list, ctxt), tag!("end"), opt!(var)) => {
                |(_, _, (block_type, then), _, _, else_, _, _)| Instr::If(block_type, then, else_)
            }
        ))
    )
);

named_args!(
    block<'a>(ctxt: &'a mut Context)<(BlockType, Vec<Instr>)>,
    parsing!(Block,
        map!(
            pair!(opt!(block_type), apply!(instr_list, ctxt)),
            |(block_type, instrs)| (
                block_type.map_or(BlockType::NoResult, BlockType::Value),
                instrs
            )
        )
    )
);

named!(
    block_type<ValueType>,
    ws!(delimited!(
        tag!("("),
        preceded!(tag!("result"), value_type),
        tag!(")")
    ))
);

named_args!(expr_list<'a>(ctxt: &'a mut Context)<Vec<Instr>>,
    map!(ws!(many0!(apply!(expr, ctxt))), |instrs| itertools::flatten(instrs).collect())
);

named_args!(expr<'a>(ctxt: &'a mut Context)<Vec<Instr>>,
    parsing!(Expr,
        ws!(delimited!(
            tag!("("),
            alt!(
                plain_instr => {
                    |instr| vec![instr]
                } |
                apply!(call_instr, ctxt) => {
                    |instr| vec![instr]
                } |
                tuple!(tag!("block"), opt!(var), apply!(block, ctxt)) => {
                    |(_, label, (block_type, instrs))| vec![Instr::Block(block_type, instrs)]
                } |
                tuple!(tag!("loop"), opt!(var), apply!(block, ctxt)) => {
                    |(_, label, (block_type, instrs))| vec![Instr::Loop(block_type, instrs)]
                } |
                tuple!(tag!("if"), opt!(var), apply!(if_block, ctxt)) => {
                    |(_, label, (mut cond, block_type, then, else_))| {
                        let mut instrs = vec![];

                        instrs.append(&mut cond);
                        instrs.push(Instr::If(block_type, then, else_));

                        instrs
                    }
                }
            ),
            tag!(")")
        ))
    )
);

named_args!(
    if_block<'a>(ctxt: &'a mut Context)<(Vec<Instr>, BlockType, Vec<Instr>, Vec<Instr>)>,
    parsing!(IfBlock,
        map!(
            pair!(opt!(block_type), apply!(if_, ctxt)),
            |(block_type, (cond, then, else_))| (
                cond,
                block_type.map_or(BlockType::NoResult, BlockType::Value),
                then,
                else_.unwrap_or_default()
            )
        )
    )
);

named_args!(
    if_<'a>(ctxt: &'a mut Context)<(Vec<Instr>, Vec<Instr>, Option<Vec<Instr>>)>,
    ws!(tuple!(
        apply!(expr, ctxt),
        delimited!(
            tag!("("),
                preceded!(tag!("then"), apply!(instr_list, ctxt)),
            tag!(")")
        ),
        opt!(delimited!(
            tag!("("),
                preceded!(tag!("else"), apply!(instr_list, ctxt)),
            tag!(")")
        ))
    ))
);

#[cfg(test)]
mod tests {
    use std::str;

    use pretty_env_logger;
    use nom::IResult::*;

    use super::*;

    #[test]
    fn parse_plain_instr() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"unreachable", Done(&[][..], Instr::Unreachable)),
            (b"nop", Done(&[][..], Instr::Nop)),
            (b"br 123", Done(&[][..], Instr::Br(Var::Index(123)))),
            (
                b"br $end",
                Done(&[][..], Instr::Br(Var::Name("end".to_owned()))),
            ),
            (b"br_if 123", Done(&[][..], Instr::BrIf(Var::Index(123)))),
            (
                b"br_if $end",
                Done(&[][..], Instr::BrIf(Var::Name("end".to_owned()))),
            ),
            (
                b"br_table 0",
                Done(&[][..], Instr::BrTable(vec![], Var::Index(0))),
            ),
            (
                b"br_table 0 1 2 3",
                Done(
                    &[][..],
                    Instr::BrTable(
                        vec![Var::Index(1), Var::Index(2), Var::Index(3)],
                        Var::Index(0),
                    ),
                ),
            ),
            (b"return", Done(&[][..], Instr::Return)),
            (b"call 123", Done(&[][..], Instr::Call(Var::Index(123)))),
            (
                b"call $name",
                Done(&[][..], Instr::Call(Var::Name("name".to_owned()))),
            ),
            (
                b"get_local 123",
                Done(&[][..], Instr::GetLocal(Var::Index(123))),
            ),
            (
                b"get_local $name",
                Done(&[][..], Instr::GetLocal(Var::Name("name".to_owned()))),
            ),
            (
                b"set_local 123",
                Done(&[][..], Instr::SetLocal(Var::Index(123))),
            ),
            (
                b"set_local $name",
                Done(&[][..], Instr::SetLocal(Var::Name("name".to_owned()))),
            ),
            (
                b"tee_local 123",
                Done(&[][..], Instr::TeeLocal(Var::Index(123))),
            ),
            (
                b"tee_local $name",
                Done(&[][..], Instr::TeeLocal(Var::Name("name".to_owned()))),
            ),
            (
                b"get_global 123",
                Done(&[][..], Instr::GetGlobal(Var::Index(123))),
            ),
            (
                b"get_global $name",
                Done(&[][..], Instr::GetGlobal(Var::Name("name".to_owned()))),
            ),
            (
                b"set_global 123",
                Done(&[][..], Instr::SetGlobal(Var::Index(123))),
            ),
            (
                b"set_global $name",
                Done(&[][..], Instr::SetGlobal(Var::Name("name".to_owned()))),
            ),
            (b"current_memory", Done(&[][..], Instr::CurrentMemory)),
            (b"grow_memory", Done(&[][..], Instr::GrowMemory)),
        ];

        for &(code, ref result) in tests.iter() {
            assert_eq!(plain_instr(code), *result, "parse instr: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_call_instr() {
        let tests: Vec<(&[u8], _)> = vec![
            (
                b"call_indirect 123",
                Done(&[][..], Instr::CallIndirect(Var::Index(123))),
            ),
            (
                b"call_indirect $hello",
                Done(&[][..], Instr::CallIndirect(Var::Name("hello".to_owned()))),
            ),
            (
                b"call_indirect (type $hello)",
                Done(&[][..], Instr::CallIndirect(Var::Name("hello".to_owned()))),
            ),
            (
                b"call_indirect (param i32)",
                Done(&[][..], Instr::CallIndirect(Var::Index(0))),
            ),
            (
                b"call_indirect (param i64)",
                Done(&[][..], Instr::CallIndirect(Var::Index(1))),
            ),
            (
                b"call_indirect (param i32) (param i64) (result f32)",
                Done(&[][..], Instr::CallIndirect(Var::Index(2))),
            ),
        ];

        let mut ctxt = Context::default();

        ctxt.types
            .types_mut()
            .push(Type::Function(FunctionType::new(
                vec![ValueType::I32],
                None,
            )));

        for &(code, ref result) in tests.iter() {
            assert_eq!(
                call_instr(code, &mut ctxt),
                *result,
                "parse instr: {}",
                unsafe { str::from_utf8_unchecked(code) }
            );
        }

        assert_eq!(
            ctxt.types.types(),
            &[
                Type::Function(FunctionType::new(vec![ValueType::I32], None)),
                Type::Function(FunctionType::new(vec![ValueType::I64], None)),
                Type::Function(FunctionType::new(
                    vec![ValueType::I32, ValueType::I64],
                    Some(ValueType::F32)
                )),
            ]
        );
    }

    #[test]
    fn parse_block_instr() {
        let tests: Vec<(&[u8], _)> = vec![
            (
                b"block\nend",
                Done(&[][..], Instr::Block(BlockType::NoResult, vec![])),
            ),
            (
                b"block (result i32)\nend",
                Done(
                    &[][..],
                    Instr::Block(BlockType::Value(ValueType::I32), vec![]),
                ),
            ),
            (
                b"block $done\nend $done",
                Done(&[][..], Instr::Block(BlockType::NoResult, vec![])),
            ),
            (
                b"block $done (result i32)\nend $done",
                Done(
                    &[][..],
                    Instr::Block(BlockType::Value(ValueType::I32), vec![]),
                ),
            ),
            (
                b"block $done (result i32) nop\nend $done",
                Done(
                    &[][..],
                    Instr::Block(BlockType::Value(ValueType::I32), vec![Instr::Nop]),
                ),
            ),
            (
                b"loop $done (result i32) unreachable\nend $done",
                Done(
                    &[][..],
                    Instr::Loop(BlockType::Value(ValueType::I32), vec![Instr::Unreachable]),
                ),
            ),
            (
                b"if $done (result i32) nop unreachable\nend $done",
                Done(
                    &[][..],
                    Instr::If(
                        BlockType::Value(ValueType::I32),
                        vec![Instr::Nop, Instr::Unreachable],
                        vec![],
                    ),
                ),
            ),
            (
                b"if $done (result i32) nop\nelse unreachable\nend $done",
                Done(
                    &[][..],
                    Instr::If(
                        BlockType::Value(ValueType::I32),
                        vec![Instr::Nop],
                        vec![Instr::Unreachable],
                    ),
                ),
            ),
        ];

        let mut ctxt = Context::default();

        for &(code, ref result) in tests.iter() {
            assert_eq!(
                block_instr(code, &mut ctxt),
                *result,
                "parse instr: {}",
                unsafe { str::from_utf8_unchecked(code) }
            );
        }
    }

    #[test]
    fn parse_expr() {
        let tests: Vec<(&[u8], _)> = vec![
            (
                b"(if (get_local 0) (then))",
                Done(
                    &[][..],
                    vec![
                        Instr::GetLocal(Var::Index(0)),
                        Instr::If(BlockType::NoResult, vec![], vec![]),
                    ],
                ),
            ),
            (
                b"(if (get_local 0) (then) (else))",
                Done(
                    &[][..],
                    vec![
                        Instr::GetLocal(Var::Index(0)),
                        Instr::If(BlockType::NoResult, vec![], vec![]),
                    ],
                ),
            ),
            (
                b"(if $l (get_local 0) (then))",
                Done(
                    &[][..],
                    vec![
                        Instr::GetLocal(Var::Index(0)),
                        Instr::If(BlockType::NoResult, vec![], vec![]),
                    ],
                ),
            ),
            (
                b"(if $l (get_local 0) (then) (else))",
                Done(
                    &[][..],
                    vec![
                        Instr::GetLocal(Var::Index(0)),
                        Instr::If(BlockType::NoResult, vec![], vec![]),
                    ],
                ),
            ),
            (
                b"(if (get_local 0) (then (nop)))",
                Done(
                    &[][..],
                    vec![
                        Instr::GetLocal(Var::Index(0)),
                        Instr::If(BlockType::NoResult, vec![Instr::Nop], vec![]),
                    ],
                ),
            ),
            (
                b"(if (get_local 0) (then (nop)) (else (nop)))",
                Done(
                    &[][..],
                    vec![
                        Instr::GetLocal(Var::Index(0)),
                        Instr::If(BlockType::NoResult, vec![Instr::Nop], vec![Instr::Nop]),
                    ],
                ),
            ),
            (
                b"(if (result i32) (get_local 0) (then (nop)) (else (nop)))",
                Done(
                    &[][..],
                    vec![
                        Instr::GetLocal(Var::Index(0)),
                        Instr::If(
                            BlockType::Value(ValueType::I32),
                            vec![Instr::Nop],
                            vec![Instr::Nop],
                        ),
                    ],
                ),
            ),
            (
                b"(if (get_local 0) (then (call $dummy) (call $dummy) (call $dummy)))",
                Done(
                    &[][..],
                    vec![
                        Instr::GetLocal(Var::Index(0)),
                        Instr::If(
                            BlockType::NoResult,
                            vec![
                                Instr::Call(Var::Name("dummy".to_owned())),
                                Instr::Call(Var::Name("dummy".to_owned())),
                                Instr::Call(Var::Name("dummy".to_owned())),
                            ],
                            vec![],
                        ),
                    ],
                ),
            ),
            (
                b"(if (get_local 0) (then) (else (call $dummy) (call $dummy) (call $dummy)))",
                Done(
                    &[][..],
                    vec![
                        Instr::GetLocal(Var::Index(0)),
                        Instr::If(
                            BlockType::NoResult,
                            vec![],
                            vec![
                                Instr::Call(Var::Name("dummy".to_owned())),
                                Instr::Call(Var::Name("dummy".to_owned())),
                                Instr::Call(Var::Name("dummy".to_owned())),
                            ],
                        ),
                    ],
                ),
            ),
            (
                b"(if (result i32) (get_local 0)
                    (then (call $dummy) (call $dummy) (call $dummy))
                    (else (call $dummy) (call $dummy) (call $dummy))
                )",
                Done(
                    &[][..],
                    vec![
                        Instr::GetLocal(Var::Index(0)),
                        Instr::If(
                            BlockType::Value(ValueType::I32),
                            vec![
                                Instr::Call(Var::Name("dummy".to_owned())),
                                Instr::Call(Var::Name("dummy".to_owned())),
                                Instr::Call(Var::Name("dummy".to_owned())),
                            ],
                            vec![
                                Instr::Call(Var::Name("dummy".to_owned())),
                                Instr::Call(Var::Name("dummy".to_owned())),
                                Instr::Call(Var::Name("dummy".to_owned())),
                            ],
                        ),
                    ],
                ),
            ),
        ];

        let mut ctxt = Context::default();

        for &(code, ref result) in tests.iter() {
            assert_eq!(expr(code, &mut ctxt), *result, "parse expr: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }
}
