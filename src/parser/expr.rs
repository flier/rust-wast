use failure::Error;
use itertools;
use parity_wasm::elements::{BlockType, InitExpr, Opcode};

use super::{block, block_type, call_instr, instr_list, label, plain_instr, BLOCK, ELSE, IF, LOOP, LPAR, OFFSET, RPAR,
            THEN};
use ast::Instr;

named!(
    pub expr_list<Vec<Instr>>,
    map!(many0!(expr), |instrs| itertools::flatten(instrs).collect())
);

named!(
    pub expr<Vec<Instr>>,
    parsing!(
        Expr,
        delimited!(
            LPAR,
            alt!(
                plain_instr => {
                    |instr| vec![instr]
                } |
                call_instr => {
                    |instr| vec![instr]
                } |
                preceded!(BLOCK, tuple!(label, first!(block))) => { |(label, (result_type, instrs))|
                    vec![Instr::Block(label, result_type, instrs)]
                } |
                preceded!(LOOP, tuple!(label, first!(block))) => { |(label, (result_type, instrs))|
                    vec![Instr::Loop(label, result_type, instrs)]
                } |
                preceded!(IF, tuple!(label, first!(if_block))) => {
                    |(label, (mut cond_instrs, result_type, then_instrs, else_instrs))| {
                        let mut instrs = vec![];

                        instrs.append(&mut cond_instrs);
                        instrs.push(Instr::If(
                            label,
                            result_type,
                            then_instrs,
                            else_instrs,
                        ));

                        instrs
                    }
                }
            ),
            RPAR
        )
    )
);

named!(
    if_block<(Vec<Instr>, BlockType, Vec<Instr>, Vec<Instr>)>,
    parsing!(
        IfBlock,
        map!(
            pair!(opt!(complete!(first!(block_type))), first!(if_)),
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
    tuple!(
        first!(expr),
        delimited!(first!(LPAR), preceded!(THEN, first!(instr_list)), first!(RPAR)),
        opt!(delimited!(
            first!(LPAR),
            preceded!(ELSE, first!(instr_list)),
            first!(RPAR)
        ))
    )
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
        LPAR,
        preceded!(OFFSET, first!(init_expr)),
        RPAR
    )
);

#[cfg(test)]
mod tests {
    use std::str;

    use nom::IResult::*;
    use parity_wasm::elements::ValueType::*;

    use super::*;
    use ast::{empty_block, Constant, Var};
    use ast::Instr::*;

    #[test]
    fn parse_expr() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"(block)", Done(&[][..], vec![empty_block()])),
            (
                b"(block $l)",
                Done(&[][..], vec![Block(Some("l".to_owned()), BlockType::NoResult, vec![])]),
            ),
            (
                b"(block (nop))",
                Done(&[][..], vec![Block(None, BlockType::NoResult, vec![Nop])]),
            ),
            (
                b"(block (result i32) (i32.const 7))",
                Done(&[][..], vec![Block(None, BlockType::Value(I32), vec![Instr::i32(7)])]),
            ),
            (
                b"(block (call $dummy) (call $dummy) (call $dummy) (call $dummy))",
                Done(
                    &[][..],
                    vec![
                        Block(
                            None,
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
                            None,
                            BlockType::Value(I32),
                            vec![
                                Block(
                                    None,
                                    BlockType::NoResult,
                                    vec![Call(Var::Id("dummy".to_owned())), empty_block(), Nop],
                                ),
                                Block(
                                    None,
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
                    vec![
                        GetLocal(Var::Index(0)),
                        If(None, BlockType::NoResult, Default::default(), Default::default()),
                    ],
                ),
            ),
            (
                b"(if (get_local 0) (then) (else))",
                Done(
                    &[][..],
                    vec![
                        GetLocal(Var::Index(0)),
                        If(None, BlockType::NoResult, Default::default(), Default::default()),
                    ],
                ),
            ),
            (
                b"(if $l (get_local 0) (then))",
                Done(
                    &[][..],
                    vec![
                        GetLocal(Var::Index(0)),
                        If(Some("l".to_owned()), BlockType::NoResult, vec![], Default::default()),
                    ],
                ),
            ),
            (
                b"(if $l (get_local 0) (then) (else))",
                Done(
                    &[][..],
                    vec![
                        GetLocal(Var::Index(0)),
                        If(Some("l".to_owned()), BlockType::NoResult, vec![], Default::default()),
                    ],
                ),
            ),
            (
                b"(if (get_local 0) (then (nop)))",
                Done(
                    &[][..],
                    vec![
                        GetLocal(Var::Index(0)),
                        If(None, BlockType::NoResult, vec![Nop], Default::default()),
                    ],
                ),
            ),
            (
                b"(if (get_local 0) (then (nop)) (else (nop)))",
                Done(
                    &[][..],
                    vec![
                        GetLocal(Var::Index(0)),
                        If(None, BlockType::NoResult, vec![Nop], vec![Nop]),
                    ],
                ),
            ),
            (
                b"(if (result i32) (get_local 0) (then (nop)) (else (nop)))",
                Done(
                    &[][..],
                    vec![
                        GetLocal(Var::Index(0)),
                        If(None, BlockType::Value(I32), vec![Nop], vec![Nop]),
                    ],
                ),
            ),
            (
                b"(if (get_local 0) (then (call $dummy) (call $dummy) (call $dummy)))",
                Done(
                    &[][..],
                    vec![
                        GetLocal(Var::Index(0)),
                        If(
                            None,
                            BlockType::NoResult,
                            vec![
                                Call(Var::Id("dummy".to_owned())),
                                Call(Var::Id("dummy".to_owned())),
                                Call(Var::Id("dummy".to_owned())),
                            ],
                            Default::default(),
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
                            None,
                            BlockType::NoResult,
                            Default::default(),
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
                            None,
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
                            None,
                            BlockType::Value(I32),
                            vec![
                                GetLocal(Var::Index(1)),
                                If(
                                    None,
                                    BlockType::NoResult,
                                    vec![Call(Var::Id("dummy".to_owned())), empty_block(), Nop],
                                    Default::default(),
                                ),
                                GetLocal(Var::Index(1)),
                                If(
                                    None,
                                    BlockType::NoResult,
                                    Default::default(),
                                    vec![Call(Var::Id("dummy".to_owned())), empty_block(), Nop],
                                ),
                                GetLocal(Var::Index(1)),
                                If(
                                    None,
                                    BlockType::Value(I32),
                                    vec![Call(Var::Id("dummy".to_owned())), Instr::i32(9)],
                                    vec![Call(Var::Id("dummy".to_owned())), Instr::i32(10)],
                                ),
                            ],
                            vec![
                                GetLocal(Var::Index(1)),
                                If(
                                    None,
                                    BlockType::NoResult,
                                    vec![Call(Var::Id("dummy".to_owned())), empty_block(), Nop],
                                    Default::default(),
                                ),
                                GetLocal(Var::Index(1)),
                                If(
                                    None,
                                    BlockType::NoResult,
                                    Default::default(),
                                    vec![Call(Var::Id("dummy".to_owned())), empty_block(), Nop],
                                ),
                                GetLocal(Var::Index(1)),
                                If(
                                    None,
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
