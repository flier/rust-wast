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
    ws!(alt!(
        plain_instr => { |instr| vec![instr] } |
        apply!(call_instr, ctxt) => { |instr| vec![instr] } |
        apply!(block_instr, ctxt) => { |instr| vec![instr] } |
        apply!(expr, ctxt)
    ))
);

named!(
    plain_instr<Instr>,
    alt_complete!(
        tag!("unreachable") => { |_| Instr::Unreachable } |
        tag!("nop") => { |_| Instr::Nop }
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
);

named_args!(
    if_block<'a>(ctxt: &'a mut Context)<(Vec<Instr>, BlockType, Vec<Instr>, Vec<Instr>)>,
    map!(
        pair!(opt!(block_type), apply!(if_, ctxt)),
        |(block_type, (cond, then, else_))| (
            cond,
            block_type.map_or(BlockType::NoResult, BlockType::Value),
            then,
            else_.unwrap_or_default()
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
}
