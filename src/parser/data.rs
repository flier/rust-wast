use super::{init_expr, offset, string_list, var, DATA, LPAR, RPAR};
use ast::{Data, Var};

named!(
    pub data<Data>,
    parsing!(
        Data,
        dbg_dmp!(map!(
            delimited!(
                LPAR,
                preceded!(
                    DATA,
                    tuple!(
                        opt!(first!(var)),
                        alt_complete!(first!(offset) | first!(init_expr)),
                        string_list
                    )
                ),
                RPAR
            ),
            |(mem_index, offset, strs)| Data {
                mem_index: mem_index.unwrap_or(Var::Index(0)),
                offset,
                value: strs.into_iter().fold(vec![], |mut value, s| {
                    value.extend_from_slice(s.as_bytes());
                    value
                }),
            }
        ))
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
    fn parse_data() {
        let _ = pretty_env_logger::try_init();

        let tests: Vec<(&[u8], _)> = vec![
            (
                br#"(data (i32.const 0))"#,
                Data {
                    mem_index: Var::Index(0),
                    offset: InitExpr::new(vec![I32Const(0)]),
                    value: vec![],
                },
            ),
            (
                br#"(data (i32.const 1) "a" "" "bcd")"#,
                Data {
                    mem_index: Var::Index(0),
                    offset: InitExpr::new(vec![I32Const(1)]),
                    value: b"abcd".to_vec(),
                },
            ),
            (
                br#"(data (offset (i32.const 0)))"#,
                Data {
                    mem_index: Var::Index(0),
                    offset: InitExpr::new(vec![I32Const(0)]),
                    value: vec![],
                },
            ),
            (
                br#"(data (offset (i32.const 0)) "" "a" "bc" "")"#,
                Data {
                    mem_index: Var::Index(0),
                    offset: InitExpr::new(vec![I32Const(0)]),
                    value: b"abc".to_vec(),
                },
            ),
            (
                br#"(data 0 (i32.const 0))"#,
                Data {
                    mem_index: Var::Index(0),
                    offset: InitExpr::new(vec![I32Const(0)]),
                    value: vec![],
                },
            ),
            (
                br#"(data 0x0 (i32.const 1) "a" "" "bcd")"#,
                Data {
                    mem_index: Var::Index(0),
                    offset: InitExpr::new(vec![I32Const(1)]),
                    value: b"abcd".to_vec(),
                },
            ),
            (
                br#"(data 0x000 (offset (i32.const 0)))"#,
                Data {
                    mem_index: Var::Index(0),
                    offset: InitExpr::new(vec![I32Const(0)]),
                    value: vec![],
                },
            ),
            (
                br#"(data 0 (offset (i32.const 0)) "" "a" "bc" "")"#,
                Data {
                    mem_index: Var::Index(0),
                    offset: InitExpr::new(vec![I32Const(0)]),
                    value: b"abc".to_vec(),
                },
            ),
            (
                br#"(data $m (i32.const 0))"#,
                Data {
                    mem_index: Var::Id("m".to_owned()),
                    offset: InitExpr::new(vec![I32Const(0)]),
                    value: vec![],
                },
            ),
            (
                br#"(data $m (i32.const 1) "a" "" "bcd")"#,
                Data {
                    mem_index: Var::Id("m".to_owned()),
                    offset: InitExpr::new(vec![I32Const(1)]),
                    value: b"abcd".to_vec(),
                },
            ),
            (
                br#"(data $m (offset (i32.const 0)))"#,
                Data {
                    mem_index: Var::Id("m".to_owned()),
                    offset: InitExpr::new(vec![I32Const(0)]),
                    value: vec![],
                },
            ),
            (
                br#"(data $m (offset (i32.const 0)) "" "a" "bc" "")"#,
                Data {
                    mem_index: Var::Id("m".to_owned()),
                    offset: InitExpr::new(vec![I32Const(0)]),
                    value: b"abc".to_vec(),
                },
            ),
        ];

        for (code, ref value) in tests {
            let res = data(code);

            trace_parse_error!(code, res);

            assert_eq!(res, Done(&[][..], value.clone()), "parse data: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }
}
