use nom::IResult;

use parity_wasm::builder::{signature, ModuleBuilder};
use parity_wasm::elements::{FunctionType, GlobalType, Module, Type, ValueType};

use parse::{value_type, value_type_list, var, Context, IndexSpace, Var};
use ast::{instr_list, Global};

fn module(input: &[u8]) -> IResult<&[u8], Module> {
    let mut ctxt = Context::default();

    map!(
        input,
        ws!(delimited!(
            tag!("("),
            preceded!(
                first!(tag!("module")),
                pair!(opt!(first!(var)), many0!(first!(apply!(module_field, &mut ctxt))))
            ),
            tag!(")")
        )),
        |_| {
            let mut builder = ModuleBuilder::new().with_signatures(
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
                    .collect(),
            );

            for global in &ctxt.globals {
                builder = builder.with_global(global.eval(&ctxt));
            }

            builder.build()
        }
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
        }} |
        apply!(global, ctxt) => { |(bind, global)| {
            trace!("global {:?} = {:?}", bind, global);

            let global_ref = ctxt.globals.get_or_insert(global);

            if let Some(Var::Name(name)) = bind {
                ctxt.global_names.insert(name, global_ref);
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
            preceded!(first!(tag!("type")), pair!(opt!(first!(var)), first!(def_type))),
            tag!(")")
        ))
    )
);

named!(
    def_type<FunctionType>,
    map!(
        ws!(delimited!(
            tag!("("),
            preceded!(first!(tag!("func")), opt!(first!(func_type))),
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
            ws!(pair!(many0!(first!(param)), opt!(complete!(first!(result))))),
            |(params, result_type)| FunctionType::new(
                params.into_iter().flat_map(|param| param).collect(),
                result_type.unwrap_or_default()
            )
        )
    )
);

named!(
    param<Vec<ValueType>>,
    ws!(delimited!(
        tag!("("),
        preceded!(
            first!(tag!("param")),
            alt!(
                pair!(first!(var), first!(value_type)) => { |(name, vt)| vec![vt] } |
                first!(value_type_list)
            )
        ),
        tag!(")")
    ))
);

named!(
    result<Option<ValueType>>,
    ws!(delimited!(
        tag!("("),
        preceded!(first!(tag!("result")), opt!(first!(value_type))),
        tag!(")")
    ))
);

named_args!(
    global<'a>(ctxt: &'a mut Context)<(Option<Var>, Global)>,
    parsing!(
        Global,
        ws!(delimited!(
            tag!("("),
            preceded!(first!(tag!("global")), pair!(opt!(first!(var)), first!(apply!(global_fields, ctxt)))),
            tag!(")")
        ))
    )
);

named_args!(
    global_fields<'a>(ctxt: &'a mut Context)<Global>,
    alt!(
        pair!(global_type, apply!(instr_list, ctxt)) => {
            |(global_type, init_expr)| Global{ global_type, init_expr }
        }
    )
);

named!(
    global_type<GlobalType>,
    alt!(
        value_type => { |ty| GlobalType::new(ty, false) } |
        mut_value_type => { |ty| GlobalType::new(ty, true) }
    )
);

named!(
    mut_value_type<ValueType>,
    ws!(delimited!(
        tag!("("),
        preceded!(tag!("mut"), value_type),
        tag!(")")
    ))
);

#[cfg(test)]
mod tests {
    use std::str;
    use std::mem;

    use pretty_env_logger;
    use parity_wasm::elements::ValueType::*;
    use parity_wasm::elements::Opcode::*;

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

    #[test]
    fn parse_global() {
        let _ = pretty_env_logger::try_init();

        let tests: Vec<(&[u8], _)> = vec![
            (
                b"(module (global $a i32 (i32.const -2)))",
                (Some("a"), I32, false, I32Const(-2)),
            ),
            (
                b"(module (global (;1;) f32 (f32.const -3)))",
                (None, F32, false, F32Const(unsafe { mem::transmute(-3.0f32) })),
            ),
            (
                b"(module (global (;2;) f64 (f64.const -4)))",
                (None, F64, false, F64Const(unsafe { mem::transmute(-4.0f64) })),
            ),
            (
                b"(module (global $b i64 (i64.const -5)))",
                (Some("b"), I64, false, I64Const(-5)),
            ),
            (
                b"(module (global $x (mut i32) (i32.const -12)))",
                (Some("x"), I32, true, I32Const(-12)),
            ),
            (
                b"(module (global (;5;) (mut f32) (f32.const -13)))",
                (None, F32, true, F32Const(unsafe { mem::transmute(-13.0f32) })),
            ),
            (
                b"(module (global (;6;) (mut f64) (f64.const -14)))",
                (None, F64, true, F64Const(unsafe { mem::transmute(-14.0f64) })),
            ),
            (
                b"(module (global $y (mut i64) (i64.const -15)))",
                (Some("y"), I64, true, I64Const(-15)),
            ),
        ];

        for (idx, (code, (name, global_type, is_mutable, constant))) in tests.into_iter().enumerate() {
            let res = module(code);

            assert!(
                res.is_done(),
                "parse global: {}, res: {:?}",
                unsafe { str::from_utf8_unchecked(code) },
                res
            );

            let (remaining, m) = res.unwrap();

            assert!(remaining.is_empty());
            assert_eq!(
                m.global_section()
                    .unwrap()
                    .entries()
                    .first()
                    .map(|global| (
                        global.global_type().content_type(),
                        global.global_type().is_mutable(),
                        global.init_expr().code().first().cloned().unwrap(),
                    ))
                    .unwrap(),
                (global_type, is_mutable, constant.clone()),
                "parse global: {}",
                unsafe { str::from_utf8_unchecked(code) },
            );
        }
    }
}
