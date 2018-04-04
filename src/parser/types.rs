use itertools;
use parity_wasm::elements::{FunctionType, GlobalType, MemoryType, TableType, ValueType};

use super::{id, F32, F64, I32, I64, nat32, ANYFUNC, LPAR, MUT, PARAM, RESULT, RPAR};
use ast::Var;

named!(pub value_type_list<Vec<ValueType>>, many0!(first!(value_type)));

/// value_type: i32 | i64 | f32 | f64
named!(pub value_type<ValueType>, alt!(int_type | float_type ));

named!(
    pub int_type<ValueType>,
    alt!(
        I32 => { |_| ValueType::I32 } |
        I64 => { |_| ValueType::I64 }
    )
);

named!(
    pub float_type<ValueType>,
    alt!(
        F32 => { |_| ValueType::F32 } |
        F64 => { |_| ValueType::F64 }
    )
);

named!(
    pub func_type<Option<FunctionType>>,
    parsing!(FuncType,
        map!(
            pair!(many0!(first!(param)), opt!(complete!(first!(result)))),
            |(params, result_type)|
                if params.is_empty() && result_type.is_none() {
                    None
                } else {
                    Some(FunctionType::new(
                        itertools::flatten(params).map(|(_, vt)| vt).collect(),
                        result_type.unwrap_or_default()
                    ))
                }
        )
    )
);

named!(
    param<Vec<(Option<Var>, ValueType)>>,
    delimited!(
        LPAR,
        preceded!(
            PARAM,
            alt_complete!(
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
);

named!(
    result<Option<ValueType>>,
    delimited!(LPAR, preceded!(RESULT, opt!(first!(value_type))), RPAR)
);

named!(pub limits<(u32, Option<u32>)>, pair!(first!(nat32), opt!(first!(nat32))));

named!(
    pub memory_type<MemoryType>,
    map!(pair!(first!(nat32), opt!(first!(nat32))), |(min, max)| MemoryType::new(min, max))
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
        first!(value_type)      => { |ty| GlobalType::new(ty, false) } |
        first!(mut_value_type)  => { |ty| GlobalType::new(ty, true) }
    )
);

named!(
    mut_value_type<ValueType>,
    delimited!(LPAR, preceded!(MUT, first!(value_type)), RPAR)
);

#[cfg(test)]
mod tests {
    use std::str;

    use nom::Err::Position;
    use nom::ErrorKind::Tag;
    use nom::IResult::{Done, Error};
    use parity_wasm::elements::ValueType;

    use super::*;

    #[test]
    fn parse_global_type() {
        let tests: Vec<(&[u8], _, _)> = vec![(b"i32", ValueType::I32, false), (b"(mut i64)", ValueType::I64, true)];

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
            (
                b"(param i32)",
                Done(&[][..], Some(FunctionType::new(vec![ValueType::I32], None))),
            ),
            (
                b"(param f64 i32 i64)",
                Done(
                    &[][..],
                    Some(FunctionType::new(
                        vec![ValueType::F64, ValueType::I32, ValueType::I64],
                        None,
                    )),
                ),
            ),
            (
                b"(param f64 i32 i64) (result f64)",
                Done(
                    &[][..],
                    Some(FunctionType::new(
                        vec![ValueType::F64, ValueType::I32, ValueType::I64],
                        Some(ValueType::F64),
                    )),
                ),
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
                Done(&[][..], vec![(Some(Var::Id("arg".to_owned())), ValueType::I32)]),
            ),
            (
                b"(param f64 i32 i64)",
                Done(
                    &[][..],
                    vec![(None, ValueType::F64), (None, ValueType::I32), (None, ValueType::I64)],
                ),
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
            (b"(result)", Done(&[][..], None)),
            (b"(result i32)", Done(&[][..], Some(ValueType::I32))),
            (b"(result i32 i64)", Error(Position(Tag, &b"i64)"[..]))),
        ];

        for &(code, ref res) in tests.iter() {
            assert_eq!(result(code), *res, "parse result: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }
}
