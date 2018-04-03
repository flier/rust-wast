use super::{init_expr, offset, var, var_list};
use ast::{Elem, Var};

named!(
    pub elem<Elem>,
    parsing!(
        Elem,
        map!(
            delimited!(
                tag!("("),
                preceded!(
                    first!(tag!("elem")),
                    tuple!(
                        opt!(first!(var)),
                        alt_complete!(first!(offset) | first!(init_expr)),
                        var_list
                    )
                ),
                tag!(")")
            ),
            |(table_index, offset, elements)| Elem {
                table_index: table_index.unwrap_or(Var::Index(0)),
                offset,
                elements,
            }
        )
    )
);

#[cfg(test)]
mod tests {
    use std::str;

    use nom::IResult::Done;
    use parity_wasm::elements::{InitExpr, Opcode::*};
    use pretty_env_logger;

    use super::*;

    #[test]
    fn parse_elem() {
        let _ = pretty_env_logger::try_init();

        let tests: Vec<(&[u8], _)> = vec![
            (
                b"(elem (i32.const 0))",
                Elem {
                    table_index: Var::Index(0),
                    offset: InitExpr::new(vec![I32Const(0)]),
                    elements: vec![],
                },
            ),
            (
                b"(elem (i32.const 0) $f $f)",
                Elem {
                    table_index: Var::Index(0),
                    offset: InitExpr::new(vec![I32Const(0)]),
                    elements: vec![Var::Name("f".to_owned()), Var::Name("f".to_owned())],
                },
            ),
            (
                b"(elem (offset (i32.const 0)))",
                Elem {
                    table_index: Var::Index(0),
                    offset: InitExpr::new(vec![I32Const(0)]),
                    elements: vec![],
                },
            ),
            (
                b"(elem (offset (i32.const 0)) $f $f)",
                Elem {
                    table_index: Var::Index(0),
                    offset: InitExpr::new(vec![I32Const(0)]),
                    elements: vec![Var::Name("f".to_owned()), Var::Name("f".to_owned())],
                },
            ),
            (
                b"(elem 0 (i32.const 0))",
                Elem {
                    table_index: Var::Index(0),
                    offset: InitExpr::new(vec![I32Const(0)]),
                    elements: vec![],
                },
            ),
            (
                b"(elem 0x0 (i32.const 0) $f $f)",
                Elem {
                    table_index: Var::Index(0),
                    offset: InitExpr::new(vec![I32Const(0)]),
                    elements: vec![Var::Name("f".to_owned()), Var::Name("f".to_owned())],
                },
            ),
            (
                b"(elem 0x000 (offset (i32.const 0)))",
                Elem {
                    table_index: Var::Index(0),
                    offset: InitExpr::new(vec![I32Const(0)]),
                    elements: vec![],
                },
            ),
            (
                b"(elem 0 (offset (i32.const 0)) $f $f)",
                Elem {
                    table_index: Var::Index(0),
                    offset: InitExpr::new(vec![I32Const(0)]),
                    elements: vec![Var::Name("f".to_owned()), Var::Name("f".to_owned())],
                },
            ),
            (
                b"(elem $t (i32.const 0))",
                Elem {
                    table_index: Var::Name("t".to_owned()),
                    offset: InitExpr::new(vec![I32Const(0)]),
                    elements: vec![],
                },
            ),
            (
                b"(elem $t (i32.const 0) $f $f)",
                Elem {
                    table_index: Var::Name("t".to_owned()),
                    offset: InitExpr::new(vec![I32Const(0)]),
                    elements: vec![Var::Name("f".to_owned()), Var::Name("f".to_owned())],
                },
            ),
            (
                b"(elem $t (offset (i32.const 0)))",
                Elem {
                    table_index: Var::Name("t".to_owned()),
                    offset: InitExpr::new(vec![I32Const(0)]),
                    elements: vec![],
                },
            ),
            (
                b"(elem $t (offset (i32.const 0)) $f $f)",
                Elem {
                    table_index: Var::Name("t".to_owned()),
                    offset: InitExpr::new(vec![I32Const(0)]),
                    elements: vec![Var::Name("f".to_owned()), Var::Name("f".to_owned())],
                },
            ),
        ];

        for (code, ref value) in tests {
            let res = elem(code);

            trace_parse_error!(code, res);

            assert_eq!(res, Done(&[][..], value.clone()), "parse elem: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }
}
