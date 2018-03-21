use parity_wasm::elements::{FunctionNameSection, FunctionType, ValueType};
use parity_wasm::builder::{ExportBuilder, FunctionBuilder, ImportBuilder};

use parse::{string, value_type, value_type_list, var, Var};
use ops::type_use;

named_args!(
    pub func<'a>(funcs: &'a FunctionNameSection)<(Option<Var>, Option<FunctionBuilder>)>,
    ws!(delimited!(
        tag!("("),
        preceded!(tag!("func"), pair!(opt!(complete!(var)), opt!(complete!(apply!(func_fields, funcs))))),
        tag!(")")
    ))
);

named_args!(
    func_fields<'a>(funcs: &'a FunctionNameSection)<FunctionBuilder>,
    ws!(alt_complete!(
        pair!(opt!(apply!(type_use, funcs)), func_type) => { |_| FunctionBuilder::new() } |
        tuple!(inline_import, opt!(apply!(type_use, funcs)), func_type) => { |_| FunctionBuilder::new() } |
        pair!(inline_export, apply!(func_fields, funcs)) => { |_| FunctionBuilder::new() }
    ))
);

named!(
    pub func_type<FunctionType>,
    parsing!(FuncType,
        map!(
            ws!(pair!(many0!(param), opt!(complete!(result)))),
            |(params, result_type)| FunctionType::new(
                params.into_iter().flat_map(|param| param).collect(),
                result_type.unwrap_or_default()
            )
        )
    )
);

named!(
    pub func_fields_param<Vec<ValueType>>,
    ws!(delimited!(
        tag!("("),
        preceded!(tag!("param"), alt!(
            preceded!(var, value_type) => { |ty| vec![ty] } |
            value_type_list => { |types| types }
        )),
        tag!(")")
    ))
);

named!(
    inline_import<ImportBuilder>,
    map!(
        ws!(delimited!(
            tag!("("),
            preceded!(tag!("import"), pair!(string, string)),
            tag!(")")
        )),
        |(module, field)| ImportBuilder::new().path(&module, &field)
    )
);

named!(
    inline_export<ExportBuilder>,
    map!(
        ws!(delimited!(
            tag!("("),
            preceded!(tag!("export"), string),
            tag!(")")
        )),
        |field| ExportBuilder::new().field(&field)
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

    use nom::{self, ErrorKind, IResult};

    use super::*;

    #[test]
    fn parse_param() {
        let tests: Vec<(&[u8], _)> = vec![
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

        for &(code, ref result) in tests.iter() {
            assert_eq!(param(code), *result, "parse func_param: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_result() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"(result)", IResult::Done(&[][..], None)),
            (
                b"(result i32)",
                IResult::Done(&[][..], Some(ValueType::I32)),
            ),
            (
                b"(result i32 i64)",
                IResult::Error(nom::Err::Position(ErrorKind::Tag, &b"i64)"[..])),
            ),
        ];

        for &(code, ref res) in tests.iter() {
            assert_eq!(result(code), *res, "parse func_result: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_inline_import() {
        let tests: Vec<(&[u8], _)> = vec![(b"(import \"m\" \"a\")", ("m", "a"))];

        for (code, result) in tests {
            let res = inline_import(code);

            assert!(res.is_done(), "parse `{}` failed", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_inline_export() {
        let tests: Vec<(&[u8], _)> = vec![(b"(export \"a\")", "a")];

        for (code, result) in tests {
            let res = inline_export(code);

            assert!(res.is_done(), "parse `{}` failed", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_func() {
        let tests: Vec<(&[u8], _)> = vec![
            (
                b"(func)",
                IResult::Error(nom::Err::Position(ErrorKind::Tag, &b"func)"[..])),
            ),
        ];

        for &(code, ref res) in tests.iter() {
            assert_eq!(result(code), *res, "parse func: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }
}
