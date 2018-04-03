use itertools;
use parity_wasm::elements::{FunctionType, GlobalType, MemoryType, TableType, ValueType};

use super::nat32;

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
            pair!(many0!(first!(param)), opt!(complete!(first!(result)))),
            |(params, result_type)|
                if params.is_empty() && result_type.is_none() {
                    None
                } else {
                    Some(FunctionType::new(
                        itertools::flatten(params).collect(),
                        result_type.unwrap_or_default()
                    ))
                }
        )
    )
);

named!(
    param<Vec<ValueType>>,
    delimited!(tag!("("), preceded!(first!(tag!("param")), value_type_list), tag!(")"))
);

named!(
    result<Option<ValueType>>,
    delimited!(
        tag!("("),
        preceded!(first!(tag!("result")), opt!(first!(value_type))),
        tag!(")")
    )
);

named!(pub limits<(u32, Option<u32>)>, pair!(first!(nat32), opt!(first!(nat32))));

named!(
    pub memory_type<MemoryType>,
    map!(ws!(pair!(nat32, opt!(nat32))), |(min, max)| MemoryType::new(min, max))
);

named!(
    pub table_type<TableType>,
    map!(pair!(limits, first!(elem_type)), |((min, max), _)| TableType::new(
        min,
        max
    ))
);

named!(pub elem_type, tag!("anyfunc"));

named!(
    pub global_type<GlobalType>,
    alt!(
        value_type => { |ty| GlobalType::new(ty, false) } |
        mut_value_type => { |ty| GlobalType::new(ty, true) }
    )
);

named!(
    mut_value_type<ValueType>,
    ws!(delimited!(tag!("("), preceded!(tag!("mut"), value_type), tag!(")")))
);

#[cfg(test)]
mod tests {
    use std::str;

    use nom::Err::Position;
    use nom::ErrorKind::Tag;
    use nom::IResult::{Done, Error};
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
            (b"(param i32)", Done(&[][..], vec![I32])),
            (b"(param f64 i32 i64)", Done(&[][..], vec![F64, I32, I64])),
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
            (b"(result i32)", Done(&[][..], Some(I32))),
            (b"(result i32 i64)", Error(Position(Tag, &b" i64)"[..]))),
        ];

        for &(code, ref res) in tests.iter() {
            assert_eq!(result(code), *res, "parse result: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }
}
