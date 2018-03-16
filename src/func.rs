use parity_wasm::elements::{Func, FunctionType, NameMap, ValueType};

use parse::{value_type, value_type_list, var, Var};
use ops::type_use;

named_args!(
    pub func_type<'a>(types: &'a NameMap)<(Option<u32>, Option<FunctionType>)>,
    alt!(
        map_res!(var, |var: Var| var.resolve_ref(types)) => { |idx| (Some(idx), None) } |
        ws!(pair!(opt!(apply!(type_use, types)), opt!(complete!(func_sig))))
    )
);

named!(
    func_sig<FunctionType>,
    map!(
        ws!(pair!(many0!(param), opt!(complete!(result)))),
        |(params, return_type)| FunctionType::new(
            params.into_iter().flat_map(|types| types).collect(),
            return_type.unwrap_or_default()
        )
    )
);

named!(
    pub param<Vec<ValueType>>,
    ws!(delimited!(
        tag!("("),
        preceded!(tag!("param"), value_type_list),
        tag!(")")
    ))
);

named!(
    pub result<Option<ValueType>>,
    ws!(delimited!(
        tag!("("),
        preceded!(tag!("result"), opt!(value_type)),
        tag!(")")
    ))
);

#[cfg(test)]
mod tests {
    use std::str;

    use nom::{ErrorKind, IResult};

    use super::*;

    #[test]
    fn parse_param() {
        let param_tests: Vec<(&[u8], _)> = vec![
            (b"(param)", IResult::Done(&[][..], vec![])),
            (b"(param i32)", IResult::Done(&[][..], vec![ValueType::I32])),
            (
                b"(param f64 i32 i64)",
                IResult::Done(
                    &[][..],
                    vec![ValueType::F64, ValueType::I32, ValueType::I64],
                ),
            ),
        ];

        for &(code, ref result) in param_tests.iter() {
            assert_eq!(param(code), *result, "parse func_param: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_result() {
        let result_tests: Vec<(&[u8], _)> = vec![
            (b"(result)", IResult::Done(&[][..], None)),
            (
                b"(result i32)",
                IResult::Done(&[][..], Some(ValueType::I32)),
            ),
            (b"(result i32 i64)", IResult::Error(ErrorKind::Tag)),
        ];

        for &(code, ref res) in result_tests.iter() {
            assert_eq!(result(code), *res, "parse func_result: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }
}
