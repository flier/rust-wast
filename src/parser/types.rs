use itertools;
use parity_wasm::elements::{FunctionType, GlobalType, MemoryType, TableType, ValueType};

use super::{id, nat32, ANYFUNC, LPAR, MUT, PARAM, RESULT, RPAR};
use ast::Var;

named!(pub value_type_list<Vec<ValueType>>, many0!(first!(value_type)));

/// value_type: i32 | i64 | f32 | f64
named!(pub value_type<ValueType>, alt!(int_type | float_type ));

named!(
    pub int_type<ValueType>,
    alt!(
        tag!("i32") => { |_| ValueType::I32 } |
        tag!("i64") => { |_| ValueType::I64 }
    )
);

named!(
    pub float_type<ValueType>,
    alt!(
        tag!("f32") => { |_| ValueType::F32 } |
        tag!("f64") => { |_| ValueType::F64 }
    )
);

named!(
    pub func_type<Option<FunctionType>>,
    parsing!(FuncType,
        map!(
            pair!(many0!(param), many0!(result)),
            |(params, results)|
                if params.is_empty() && results.is_empty() {
                    None
                } else {
                    Some(FunctionType::new(
                        itertools::flatten(params).map(|(_, vt)| vt).collect(),
                        itertools::flatten(results).next()
                    ))
                }
        )
    )
);

named!(
    param<Vec<(Option<Var>, ValueType)>>,
    parsing!(Param,
        delimited!(
            LPAR,
            preceded!(
                PARAM,
                alt!(
                    pair!(first!(id), first!(value_type)) => { |(id, vt): (&str, _)|
                        vec![(Some(Var::Id(id.to_owned())), vt)]
                    } |
                    value_type_list => { |params: Vec<_>|
                        params.into_iter().map(|vt| (None, vt)).collect()
                    }
                )
            ),
            RPAR
        )
    )
);

named!(
    result<Vec<ValueType>>,
    parsing!(Result, delimited!(LPAR, preceded!(RESULT, value_type_list), RPAR))
);

named!(pub limits<(u32, Option<u32>)>, pair!(nat32, opt!(first!(nat32))));

named!(
    pub memory_type<MemoryType>,
    map!(pair!(nat32, opt!(first!(nat32))), |(min, max)| MemoryType::new(min, max))
);

named!(
    pub table_type<TableType>,
    map!(pair!(limits, first!(elem_type)), |((min, max), _)| TableType::new(
        min,
        max
    ))
);

named!(pub elem_type, call!(ANYFUNC));

named!(
    pub global_type<GlobalType>,
    alt!(
        value_type      => { |ty| GlobalType::new(ty, false) } |
        mut_value_type  => { |ty| GlobalType::new(ty, true) }
    )
);

named!(
    mut_value_type<ValueType>,
    delimited!(LPAR, preceded!(MUT, first!(value_type)), RPAR)
);

#[cfg(test)]
mod tests {
    use std::str;

    use nom::IResult::Done;
    use parity_wasm::elements::ValueType::*;

    use super::*;

    #[test]
    fn parse_global_type() {
        let tests: Vec<(&[u8], _, _)> = vec![(b"i32", I32, false), (b"(mut i64)", I64, true)];

        for (code, value_type, is_mutable) in tests {
            let (remaining, global_type) = global_type(code).unwrap();

            assert!(remaining.is_empty());
            assert_eq!(global_type.content_type(), value_type);
            assert_eq!(global_type.is_mutable(), is_mutable);
        }
    }

    #[test]
    fn parse_func_type() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"", Done(&[][..], None)),
            (b"(param)", Done(&[][..], Some(FunctionType::new(vec![], None)))),
            (b"(param i32)", Done(&[][..], Some(FunctionType::new(vec![I32], None)))),
            (
                b"(param f64 i32 i64)",
                Done(&[][..], Some(FunctionType::new(vec![F64, I32, I64], None))),
            ),
            (
                b"(param f64 i32 i64) (result f64)",
                Done(&[][..], Some(FunctionType::new(vec![F64, I32, I64], Some(F64)))),
            ),
        ];

        for (code, result) in tests {
            assert_eq!(func_type(code), result, "parse func_type: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_param() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"(param)", Done(&[][..], vec![])),
            (
                b"(param $arg i32)",
                Done(&[][..], vec![(Some(Var::Id("arg".to_owned())), I32)]),
            ),
            (
                b"(param f64 i32 i64)",
                Done(&[][..], vec![(None, F64), (None, I32), (None, I64)]),
            ),
        ];

        for &(code, ref result) in tests.iter() {
            assert_eq!(param(code), *result, "parse param: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_result() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"(result)", Done(&[][..], vec![])),
            (b"(result i32)", Done(&[][..], vec![I32])),
            (b"(result i32 i64)", Done(&[][..], vec![I32, I64])),
        ];

        for &(code, ref res) in tests.iter() {
            assert_eq!(result(code), *res, "parse result: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }
}
