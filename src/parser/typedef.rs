use parity_wasm::elements::FunctionType;

use super::{func_type, opt_bind_var, FUNC, LPAR, RPAR, TYPE};
use ast::Var;

named!(
    pub typedef<(Option<Var>, FunctionType)>,
    parsing!(
        TypeDef,
        delimited!(
            LPAR,
            preceded!(TYPE, pair!(opt_bind_var, first!(def_type))),
            RPAR
        )
    )
);

named!(
    def_type<FunctionType>,
    map!(delimited!(LPAR, preceded!(FUNC, func_type), RPAR), |ft| {
        ft.unwrap_or_default()
    })
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
                (Some(Var::Id("t".to_owned())), FunctionType::new(vec![], None)),
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
                typedef(code),
                Done(&[][..], func_type.clone()),
                "parse typedef: {}",
                unsafe { str::from_utf8_unchecked(code) },
            );
        }
    }
}
