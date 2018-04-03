use failure::Error;
use itertools;
use parity_wasm::elements::{BlockType, InitExpr, Opcode};

use super::{block, block_type, call_instr, instr_list, plain_instr, var};
use ast::Instr;

named!(
    pub expr_list<Vec<Instr>>,
    map!(ws!(many0!(first!(expr))), |instrs| itertools::flatten(instrs).collect())
);

named!(
    pub expr<Vec<Instr>>,
    parsing!(
        Expr,
        ws!(delimited!(
            tag!("("),
            alt!(
                plain_instr => {
                    |instr| vec![instr]
                } |
                call_instr => {
                    |instr| vec![instr]
                } |
                tuple!(tag!("block"), opt!(first!(var)), first!(block)) => {
                    |(_, label, (block_type, instrs))| vec![Instr::Block(block_type, instrs)]
                } |
                tuple!(tag!("loop"), opt!(first!(var)), first!(block)) => {
                    |(_, label, (block_type, instrs))| vec![Instr::Loop(block_type, instrs)]
                } |
                tuple!(tag!("if"), opt!(first!(var)), first!(if_block)) => {
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

named!(
    if_block<(Vec<Instr>, BlockType, Vec<Instr>, Vec<Instr>)>,
    parsing!(
        IfBlock,
        map!(
            pair!(opt!(block_type), first!(if_)),
            |(block_type, (cond, then, else_))| (
                cond,
                block_type.map_or(BlockType::NoResult, BlockType::Value),
                then,
                else_.unwrap_or_default()
            )
        )
    )
);

named!(
    if_<(Vec<Instr>, Vec<Instr>, Option<Vec<Instr>>)>,
    ws!(tuple!(
        first!(expr),
        delimited!(tag!("("), preceded!(tag!("then"), first!(instr_list)), tag!(")")),
        opt!(delimited!(
            tag!("("),
            preceded!(tag!("else"), first!(instr_list)),
            tag!(")")
        ))
    ))
);

named!(
    pub init_expr<InitExpr>,
    map_res!(instr_list, |instrs: Vec<Instr>| -> Result<InitExpr, Error> {
        let opcodes = instrs
            .into_iter()
            .map(|instr| match instr {
                Instr::Const(constant) => Ok(constant.into()),
                _ => bail!("constant expression required"),
            })
            .collect::<Result<Vec<Opcode>, Error>>()?;

        Ok(InitExpr::new(opcodes))
    })
);

named!(
    pub offset<InitExpr>,
    delimited!(
        tag!("("),
        preceded!(first!(tag!("offset")), first!(init_expr)),
        tag!(")")
    )
);

#[cfg(test)]
mod tests {
    use std::str;

    use nom::IResult::*;
    use parity_wasm::elements::ValueType::*;

    use super::*;
    use ast::{Constant, Var};
    use ast::Instr::*;

    #[test]
    fn parse_expr() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"(block)", Done(&[][..], vec![Block(BlockType::NoResult, vec![])])),
            (b"(block $l)", Done(&[][..], vec![Block(BlockType::NoResult, vec![])])),
            (
                b"(block (nop))",
                Done(&[][..], vec![Block(BlockType::NoResult, vec![Nop])]),
            ),
            (
                b"(block (result i32) (i32.const 7))",
                Done(&[][..], vec![Block(BlockType::Value(I32), vec![Instr::i32(7)])]),
            ),
            (
                b"(block (call $dummy) (call $dummy) (call $dummy) (call $dummy))",
                Done(
                    &[][..],
                    vec![
                        Block(
                            BlockType::NoResult,
                            vec![
                                Call(Var::Id("dummy".to_owned())),
                                Call(Var::Id("dummy".to_owned())),
                                Call(Var::Id("dummy".to_owned())),
                                Call(Var::Id("dummy".to_owned())),
                            ],
                        ),
                    ],
                ),
            ),
            (
                b"(block (result i32)
                    (block (call $dummy) (block) (nop))
                    (block (result i32) (call $dummy) (i32.const 9))
                )",
                Done(
                    &[][..],
                    vec![
                        Block(
                            BlockType::Value(I32),
                            vec![
                                Block(
                                    BlockType::NoResult,
                                    vec![
                                        Call(Var::Id("dummy".to_owned())),
                                        Block(BlockType::NoResult, vec![]),
                                        Nop,
                                    ],
                                ),
                                Block(
                                    BlockType::Value(I32),
                                    vec![Call(Var::Id("dummy".to_owned())), Const(Constant::I32(9))],
                                ),
                            ],
                        ),
                    ],
                ),
            ),
            (
                b"(if (get_local 0) (then))",
                Done(
                    &[][..],
                    vec![GetLocal(Var::Index(0)), If(BlockType::NoResult, vec![], vec![])],
                ),
            ),
            (
                b"(if (get_local 0) (then) (else))",
                Done(
                    &[][..],
                    vec![GetLocal(Var::Index(0)), If(BlockType::NoResult, vec![], vec![])],
                ),
            ),
            (
                b"(if $l (get_local 0) (then))",
                Done(
                    &[][..],
                    vec![GetLocal(Var::Index(0)), If(BlockType::NoResult, vec![], vec![])],
                ),
            ),
            (
                b"(if $l (get_local 0) (then) (else))",
                Done(
                    &[][..],
                    vec![GetLocal(Var::Index(0)), If(BlockType::NoResult, vec![], vec![])],
                ),
            ),
            (
                b"(if (get_local 0) (then (nop)))",
                Done(
                    &[][..],
                    vec![GetLocal(Var::Index(0)), If(BlockType::NoResult, vec![Nop], vec![])],
                ),
            ),
            (
                b"(if (get_local 0) (then (nop)) (else (nop)))",
                Done(
                    &[][..],
                    vec![GetLocal(Var::Index(0)), If(BlockType::NoResult, vec![Nop], vec![Nop])],
                ),
            ),
            (
                b"(if (result i32) (get_local 0) (then (nop)) (else (nop)))",
                Done(
                    &[][..],
                    vec![GetLocal(Var::Index(0)), If(BlockType::Value(I32), vec![Nop], vec![Nop])],
                ),
            ),
            (
                b"(if (get_local 0) (then (call $dummy) (call $dummy) (call $dummy)))",
                Done(
                    &[][..],
                    vec![
                        GetLocal(Var::Index(0)),
                        If(
                            BlockType::NoResult,
                            vec![
                                Call(Var::Id("dummy".to_owned())),
                                Call(Var::Id("dummy".to_owned())),
                                Call(Var::Id("dummy".to_owned())),
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
                        GetLocal(Var::Index(0)),
                        If(
                            BlockType::NoResult,
                            vec![],
                            vec![
                                Call(Var::Id("dummy".to_owned())),
                                Call(Var::Id("dummy".to_owned())),
                                Call(Var::Id("dummy".to_owned())),
                            ],
                        ),
                    ],
                ),
            ),
            (
                b"(if (result i32) (get_local 0)
                    (then (call $dummy) (call $dummy) (i32.const 8))
                    (else (call $dummy) (call $dummy) (i32.const 9))
                )",
                Done(
                    &[][..],
                    vec![
                        GetLocal(Var::Index(0)),
                        If(
                            BlockType::Value(I32),
                            vec![
                                Call(Var::Id("dummy".to_owned())),
                                Call(Var::Id("dummy".to_owned())),
                                Instr::i32(8),
                            ],
                            vec![
                                Call(Var::Id("dummy".to_owned())),
                                Call(Var::Id("dummy".to_owned())),
                                Instr::i32(9),
                            ],
                        ),
                    ],
                ),
            ),
            (
                b"(if (result i32) (get_local 0)
                    (then
                        (if (get_local 1) (then (call $dummy) (block) (nop)))
                        (if (get_local 1) (then) (else (call $dummy) (block) (nop)))
                        (if (result i32) (get_local 1)
                            (then (call $dummy) (i32.const 9))
                            (else (call $dummy) (i32.const 10))
                        )
                    )
                    (else
                        (if (get_local 1) (then (call $dummy) (block) (nop)))
                        (if (get_local 1) (then) (else (call $dummy) (block) (nop)))
                        (if (result i32) (get_local 1)
                            (then (call $dummy) (i32.const 10))
                            (else (call $dummy) (i32.const 11))
                        )
                    )
                )",
                Done(
                    &[][..],
                    vec![
                        GetLocal(Var::Index(0)),
                        If(
                            BlockType::Value(I32),
                            vec![
                                GetLocal(Var::Index(1)),
                                If(
                                    BlockType::NoResult,
                                    vec![
                                        Call(Var::Id("dummy".to_owned())),
                                        Block(BlockType::NoResult, vec![]),
                                        Nop,
                                    ],
                                    vec![],
                                ),
                                GetLocal(Var::Index(1)),
                                If(
                                    BlockType::NoResult,
                                    vec![],
                                    vec![
                                        Call(Var::Id("dummy".to_owned())),
                                        Block(BlockType::NoResult, vec![]),
                                        Nop,
                                    ],
                                ),
                                GetLocal(Var::Index(1)),
                                If(
                                    BlockType::Value(I32),
                                    vec![Call(Var::Id("dummy".to_owned())), Instr::i32(9)],
                                    vec![Call(Var::Id("dummy".to_owned())), Instr::i32(10)],
                                ),
                            ],
                            vec![
                                GetLocal(Var::Index(1)),
                                If(
                                    BlockType::NoResult,
                                    vec![
                                        Call(Var::Id("dummy".to_owned())),
                                        Block(BlockType::NoResult, vec![]),
                                        Nop,
                                    ],
                                    vec![],
                                ),
                                GetLocal(Var::Index(1)),
                                If(
                                    BlockType::NoResult,
                                    vec![],
                                    vec![
                                        Call(Var::Id("dummy".to_owned())),
                                        Block(BlockType::NoResult, vec![]),
                                        Nop,
                                    ],
                                ),
                                GetLocal(Var::Index(1)),
                                If(
                                    BlockType::Value(I32),
                                    vec![Call(Var::Id("dummy".to_owned())), Instr::i32(10)],
                                    vec![Call(Var::Id("dummy".to_owned())), Instr::i32(11)],
                                ),
                            ],
                        ),
                    ],
                ),
            ),
        ];

        for &(code, ref result) in tests.iter() {
            assert_eq!(expr(code), *result, "parse expr: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }
}
