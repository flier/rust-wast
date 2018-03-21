use nom::IResult;
use failure::Error;

use parity_wasm::builder::{signature, ModuleBuilder};
use parity_wasm::elements::{FunctionType, Module, Type, ValueType};

use parse::{value_type, value_type_list, var, Context, TypeSectionExt, Var};

fn module(input: &[u8]) -> IResult<&[u8], Module> {
    let mut ctxt = Context::default();

    map!(
        input,
        ws!(delimited!(
            tag!("("),
            preceded!(
                tag!("module"),
                pair!(opt!(var), many0!(apply!(module_field, &mut ctxt)))
            ),
            tag!(")")
        )),
        |_| ModuleBuilder::new()
            .with_signatures(
                ctxt.types
                    .types()
                    .iter()
                    .map(|ty| {
                        let &Type::Function(ref ft) = ty;

                        signature()
                            .with_params(ft.params().to_vec())
                            .with_return_type(ft.return_type())
                            .build_sig()
                    })
                    .collect()
            )
            .build()
    )
}

named_args!(
    module_field<'a>(ctxt: &'a mut Context)<()>,
    alt!(
        type_def => { |(bind, func_type)| {
            trace!("typedef {:?} = {:?}", bind, func_type);

            let type_ref = ctxt.types.get_or_insert(func_type);

            if let Some(Var::Name(name)) = bind {
                ctxt.typedefs.insert(name, type_ref);
            }
        }}
    )
);

named!(
    type_def<(Option<Var>, FunctionType)>,
    parsing!(
        TypeDef,
        ws!(delimited!(
            tag!("("),
            preceded!(tag!("type"), pair!(opt!(var), def_type)),
            tag!(")")
        ))
    )
);

named!(
    def_type<FunctionType>,
    map!(
        ws!(delimited!(
            tag!("("),
            preceded!(tag!("func"), opt!(func_type)),
            tag!(")")
        )),
        |ft| ft.unwrap_or_default()
    )
);

named!(
    func_type<FunctionType>,
    parsing!(
        FuncType,
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
    pub param<Vec<ValueType>>,
    ws!(delimited!(
        tag!("("),
        preceded!(
            tag!("param"),
            alt!(
                pair!(var, value_type) => { |(name, vt)| vec![vt] } |
                value_type_list
            )
        ),
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

    use pretty_env_logger;
    use parity_wasm::elements::ValueType::*;

    use super::*;

    #[test]
    fn parse_typedef() {
        let _ = pretty_env_logger::try_init();

        let tests: Vec<(&[u8], _)> = vec![
            (
                b"(module (type (func)))",
                vec![Type::Function(FunctionType::new(vec![], None))],
            ),
            (
                b"(module (type $t (func)))",
                vec![Type::Function(FunctionType::new(vec![], None))],
            ),
            (
                b"(module (type (func (param i32))))",
                vec![Type::Function(FunctionType::new(vec![I32], None))],
            ),
            (
                b"(module (type (func (param $x i32))))",
                vec![Type::Function(FunctionType::new(vec![I32], None))],
            ),
            (
                b"(module (type (func (result i32))))",
                vec![Type::Function(FunctionType::new(vec![], Some(I32)))],
            ),
            (
                b"(module (type (func (param i32) (result i32))))",
                vec![Type::Function(FunctionType::new(vec![I32], Some(I32)))],
            ),
            (
                b"(module (type (func (param $x i32) (result i32))))",
                vec![Type::Function(FunctionType::new(vec![I32], Some(I32)))],
            ),
            (
                b"(module (type (func (param f32 f64))))",
                vec![Type::Function(FunctionType::new(vec![F32, F64], None))],
            ),
            (
                b"(module (type (func (param f32) (param f64))))",
                vec![Type::Function(FunctionType::new(vec![F32, F64], None))],
            ),
            (
                b"(module (type (func (param $x f32) (param f64))))",
                vec![Type::Function(FunctionType::new(vec![F32, F64], None))],
            ),
            (
                b"(module (type (func (param f32) (param $y f64))))",
                vec![Type::Function(FunctionType::new(vec![F32, F64], None))],
            ),
            (
                b"(module (type (func (param $x f32) (param $y f64))))",
                vec![Type::Function(FunctionType::new(vec![F32, F64], None))],
            ),
            (
                b"(module (type (func (param f32 f64) (param $x i32) (param f64 i32 i32))))",
                vec![Type::Function(FunctionType::new(vec![F32, F64, I32, F64, I32, I32], None))],
            ),
            (
                b"(module (type (func (param) (param $x f32) (param) (param) (param f64 i32) (param))))",
                vec![Type::Function(FunctionType::new(vec![F32, F64, I32], None))],
            ),
        ];

        for (code, ref types) in tests {
            let res = module(code);

            assert!(
                res.is_done(),
                "parse typedef: {}, res: {:?}",
                unsafe { str::from_utf8_unchecked(code) },
                res
            );

            let (remaining, m) = res.unwrap();

            assert!(remaining.is_empty());
            assert_eq!(m.type_section().unwrap().types(), types.as_slice());
        }
    }
}
