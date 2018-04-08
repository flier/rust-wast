use itertools;

use super::{id, inline_export, inline_import, instr_list, opt_bind_var, typeuse, value_type, value_type_list, FUNC,
            LOCAL, LPAR, RPAR};
use ast::{Function, Local, Var};

named!(
    pub func<(Option<Var>, Function)>,
    parsing!(
        Function,
        delimited!(
            LPAR,
            preceded!(
                FUNC,
                pair!(opt_bind_var, first!(func_fields))
            ),
            RPAR
        )
    )
);

named!(
    func_fields<Function>,
    alt!(
        tuple!(inline_import, first!(typeuse)) => {
            |((module_name, item_name), (func_idx, func_type))|
                Function {
                    func_idx,
                    func_type,
                    locals: vec![],
                    body: vec![],
                }
        } |
        tuple!(inline_export, typeuse, many0!(local), instr_list) => {
            |(export_name, (func_idx, func_type), locals, body)|
                Function {
                    func_idx,
                    func_type,
                    locals: itertools::flatten(locals).collect(),
                    body,
                }
        } |
        tuple!(typeuse, many0!(local), instr_list) => {
            |((func_idx, func_type), locals, body)|
                Function {
                    func_idx,
                    func_type,
                    locals: itertools::flatten(locals).collect(),
                    body,
                }
        }
    )
);

named!(
    local<Vec<Local>>,
    parsing!(
        Local,
        delimited!(
            LPAR,
            preceded!(
                LOCAL,
                alt!(
                    pair!(first!(id), first!(value_type)) => { |(id, vt)|
                        vec![Local::new(id, vt)]
                    } |
                    value_type_list => { |types: Vec<_>|
                        types.into_iter().map(|value_type| Local::value(value_type)).collect()
                    }
                )
            ),
            RPAR
        )
    )
);

#[cfg(test)]
mod tests {
    use std::str;

    use nom::IResult::Done;
    use pretty_env_logger;
    use parity_wasm::elements::{FunctionType, ValueType::*};

    use super::*;
    use ast::{Constant, Instr};

    #[test]
    fn parse_function() {
        let _ = pretty_env_logger::try_init();

        let tests: Vec<(&[u8], _)> = vec![
            (
                b"(func)",
                Done(
                    &[][..],
                    (
                        None,
                        Function {
                            func_idx: None,
                            func_type: None,
                            locals: vec![],
                            body: vec![],
                        },
                    ),
                ),
            ),
            (
                b"(func $f)",
                Done(
                    &[][..],
                    (
                        Some(Var::Id("f".to_owned())),
                        Function {
                            func_idx: None,
                            func_type: None,
                            locals: vec![],
                            body: vec![],
                        },
                    ),
                ),
            ),
            (
                b"(func (local))",
                Done(
                    &[][..],
                    (
                        None,
                        Function {
                            func_idx: None,
                            func_type: None,
                            locals: vec![],
                            body: vec![],
                        },
                    ),
                ),
            ),
            (
                b"(func (local) (local))",
                Done(
                    &[][..],
                    (
                        None,
                        Function {
                            func_idx: None,
                            func_type: None,
                            locals: vec![],
                            body: vec![],
                        },
                    ),
                ),
            ),
            (
                b"(func (local i32))",
                Done(
                    &[][..],
                    (
                        None,
                        Function {
                            func_idx: None,
                            func_type: None,
                            locals: vec![Local::i32()],
                            body: vec![],
                        },
                    ),
                ),
            ),
            (
                b"(func (local $x i32))",
                Done(
                    &[][..],
                    (
                        None,
                        Function {
                            func_idx: None,
                            func_type: None,
                            locals: vec![Local::new("x", I32)],
                            body: vec![],
                        },
                    ),
                ),
            ),
            (
                b"(func (local i32 f64 i64))",
                Done(
                    &[][..],
                    (
                        None,
                        Function {
                            func_idx: None,
                            func_type: None,
                            locals: vec![Local::i32(), Local::f64(), Local::i64()],
                            body: vec![],
                        },
                    ),
                ),
            ),
            (
                b"(func (local i32) (local f64))",
                Done(
                    &[][..],
                    (
                        None,
                        Function {
                            func_idx: None,
                            func_type: None,
                            locals: vec![Local::i32(), Local::f64()],
                            body: vec![],
                        },
                    ),
                ),
            ),
            (
                b"(func (local i32 f32) (local $x i64) (local) (local i32 f64))",
                Done(
                    &[][..],
                    (
                        None,
                        Function {
                            func_idx: None,
                            func_type: None,
                            locals: vec![
                                Local::i32(),
                                Local::f32(),
                                Local::new("x", I64),
                                Local::i32(),
                                Local::f64(),
                            ],
                            body: vec![],
                        },
                    ),
                ),
            ),
            (
                b"(func (param))",
                Done(
                    &[][..],
                    (
                        None,
                        Function {
                            func_idx: None,
                            func_type: Some(FunctionType::default()),
                            locals: vec![],
                            body: vec![],
                        },
                    ),
                ),
            ),
            (
                b"(func (param) (param))",
                Done(
                    &[][..],
                    (
                        None,
                        Function {
                            func_idx: None,
                            func_type: Some(FunctionType::default()),
                            locals: vec![],
                            body: vec![],
                        },
                    ),
                ),
            ),
            (
                b"(func (param i32))",
                Done(
                    &[][..],
                    (
                        None,
                        Function {
                            func_idx: None,
                            func_type: Some(FunctionType::new(vec![I32], None)),
                            locals: vec![],
                            body: vec![],
                        },
                    ),
                ),
            ),
            (
                b"(func (param $x i32))",
                Done(
                    &[][..],
                    (
                        None,
                        Function {
                            func_idx: None,
                            func_type: Some(FunctionType::new(vec![I32], None)),
                            locals: vec![],
                            body: vec![],
                        },
                    ),
                ),
            ),
            (
                b"(func (param i32 f64 i64))",
                Done(
                    &[][..],
                    (
                        None,
                        Function {
                            func_idx: None,
                            func_type: Some(FunctionType::new(vec![I32, F64, I64], None)),
                            locals: vec![],
                            body: vec![],
                        },
                    ),
                ),
            ),
            (
                b"(func (param i32) (param f64))",
                Done(
                    &[][..],
                    (
                        None,
                        Function {
                            func_idx: None,
                            func_type: Some(FunctionType::new(vec![I32, F64], None)),
                            locals: vec![],
                            body: vec![],
                        },
                    ),
                ),
            ),
            (
                b"(func (param i32 f32) (param $x i64) (param) (param i32 f64))",
                Done(
                    &[][..],
                    (
                        None,
                        Function {
                            func_idx: None,
                            func_type: Some(FunctionType::new(vec![I32, F32, I64, I32, F64], None)),
                            locals: vec![],
                            body: vec![],
                        },
                    ),
                ),
            ),
            (
                b"(func (result i32) (unreachable))",
                Done(
                    &[][..],
                    (
                        None,
                        Function {
                            func_idx: None,
                            func_type: Some(FunctionType::new(vec![], Some(I32))),
                            locals: vec![],
                            body: vec![Instr::Unreachable],
                        },
                    ),
                ),
            ),
            (
                b"(func $complex
                    (param i32 f32) (param $x i64) (param) (param i32)
                    (result) (result i32) (result)
                    (local f32) (local $y i32) (local i64 i32) (local) (local f64 i32)
                    (unreachable) (unreachable)
                )",
                Done(
                    &[][..],
                    (
                        Some(Var::id("complex")),
                        Function {
                            func_idx: None,
                            func_type: Some(FunctionType::new(vec![I32, F32, I64, I32], Some(I32))),
                            locals: vec![
                                Local::f32(),
                                Local::new("y", I32),
                                Local::i64(),
                                Local::i32(),
                                Local::f64(),
                                Local::i32(),
                            ],
                            body: vec![Instr::Unreachable, Instr::Unreachable],
                        },
                    ),
                ),
            ),
            (
                b"(func (type $sig-1))",
                Done(
                    &[][..],
                    (
                        None,
                        Function {
                            func_idx: Some(Var::id("sig-1")),
                            func_type: None,
                            locals: vec![],
                            body: vec![],
                        },
                    ),
                ),
            ),
            (
                b"(func (export \"type-use-1\") (type $sig-1))",
                Done(
                    &[][..],
                    (
                        None,
                        Function {
                            func_idx: Some(Var::id("sig-1")),
                            func_type: None,
                            locals: vec![],
                            body: vec![],
                        },
                    ),
                ),
            ),
            (
                b"(func (export \"type-use-7\")
                    (type $sig-4) (param i32) (param f64 i32) (result i32) (i32.const 0)
                )",
                Done(
                    &[][..],
                    (
                        None,
                        Function {
                            func_idx: Some(Var::id("sig-4")),
                            func_type: Some(FunctionType::new(vec![I32, F64, I32], Some(I32))),
                            locals: vec![],
                            body: vec![Instr::Const(Constant::I32(0))],
                        },
                    ),
                ),
            ),
            (
                b"(func (import \"spectest\" \"print_i32\") (param i32))",
                Done(
                    &[][..],
                    (
                        None,
                        Function {
                            func_idx: None,
                            func_type: Some(FunctionType::new(vec![I32], None)),
                            locals: vec![],
                            body: vec![],
                        },
                    ),
                ),
            ),
            (
                br#"(func (import "spectest" "print_i32") (type $forward))"#,
                Done(
                    &[][..],
                    (
                        None,
                        Function {
                            func_idx: Some(Var::id("forward")),
                            func_type: None,
                            locals: vec![],
                            body: vec![],
                        },
                    ),
                ),
            ),
        ];

        for (code, result) in tests {
            let res = func(code);

            trace_parse_error!(code, res);

            assert_eq!(res, result, "parse function: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }
}
