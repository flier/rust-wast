use parity_wasm::elements::{FunctionType, ValueType};

use super::{bind_var, value_type, value_type_list, var};
use ast::Var;

named!(
    pub type_def<(Option<Var>, FunctionType)>,
    parsing!(
        TypeDef,
        delimited!(
            tag!("("),
            preceded!(first!(tag!("type")), pair!(opt!(first!(bind_var)), first!(def_type))),
            tag!(")")
        )
    )
);

named!(
    def_type<FunctionType>,
    map!(
        delimited!(
            tag!("("),
            preceded!(first!(tag!("func")), opt!(first!(func_type))),
            tag!(")")
        ),
        |ft| ft.unwrap_or_default()
    )
);

named!(
    func_type<FunctionType>,
    parsing!(
        FuncType,
        map!(
            pair!(many0!(first!(param)), opt!(complete!(first!(result)))),
            |(params, result_type)| FunctionType::new(
                params.into_iter().flat_map(|param| param).collect(),
                result_type.unwrap_or_default()
            )
        )
    )
);

named!(
    param<Vec<ValueType>>,
    delimited!(
        tag!("("),
        preceded!(
            first!(tag!("param")),
            alt!(
                pair!(first!(var), first!(value_type)) => { |(name, vt)| vec![vt] } |
                first!(value_type_list)
            )
        ),
        tag!(")")
    )
);

named!(
    result<Option<ValueType>>,
    delimited!(
        tag!("("),
        preceded!(first!(tag!("result")), opt!(first!(value_type))),
        tag!(")")
    )
);

#[cfg(test)]
mod tests {
    use std::str;

    use nom::IResult::Done;
    use parity_wasm::elements::ValueType::*;
    use pretty_env_logger;

    use super::*;

    #[test]
    fn parse_typedef() {
        let _ = pretty_env_logger::try_init();

        let tests: Vec<(&[u8], _)> = vec![
            (b"(type (func))", (None, FunctionType::new(vec![], None))),
            (
                b"(type $t (func))",
                (Some(Var::Name("t".to_owned())), FunctionType::new(vec![], None)),
            ),
            (b"(type (func (param i32)))", (None, FunctionType::new(vec![I32], None))),
            (
                b"(type (func (param $x i32)))",
                (None, FunctionType::new(vec![I32], None)),
            ),
            (
                b"(type (func (result i32)))",
                (None, FunctionType::new(vec![], Some(I32))),
            ),
            (
                b"(type (func (param i32) (result i32)))",
                (None, FunctionType::new(vec![I32], Some(I32))),
            ),
            (
                b"(type (func (param $x i32) (result i32)))",
                (None, FunctionType::new(vec![I32], Some(I32))),
            ),
            (
                b"(type (func (param f32 f64)))",
                (None, FunctionType::new(vec![F32, F64], None)),
            ),
            (
                b"(type (func (param f32) (param f64)))",
                (None, FunctionType::new(vec![F32, F64], None)),
            ),
            (
                b"(type (func (param $x f32) (param f64)))",
                (None, FunctionType::new(vec![F32, F64], None)),
            ),
            (
                b"(type (func (param f32) (param $y f64)))",
                (None, FunctionType::new(vec![F32, F64], None)),
            ),
            (
                b"(type (func (param $x f32) (param $y f64)))",
                (None, FunctionType::new(vec![F32, F64], None)),
            ),
            (
                b"(type (func (param f32 f64) (param $x i32) (param f64 i32 i32)))",
                (None, FunctionType::new(vec![F32, F64, I32, F64, I32, I32], None)),
            ),
            (
                b"(type (func (param) (param $x f32) (param) (param) (param f64 i32) (param)))",
                (None, FunctionType::new(vec![F32, F64, I32], None)),
            ),
        ];

        for (code, ref func_type) in tests {
            assert_eq!(
                type_def(code),
                Done(&[][..], func_type.clone()),
                "parse typedef: {}",
                unsafe { str::from_utf8_unchecked(code) },
            );
        }
    }
}
