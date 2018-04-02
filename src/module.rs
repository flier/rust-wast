use nom::IResult;
use failure::Error;

use parity_wasm::builder::{signature, ModuleBuilder};
use parity_wasm::elements::{FunctionType, GlobalType, Module, TableType, Type, ValueType};

use ast::{instr_list, Global, Table};
use parse::{elem_type, string, table_type, value_type, value_type_list, var, var_list, Context, IndexSpace, Var};

fn module(input: &[u8]) -> IResult<&[u8], Module> {
    let mut ctxt = Context::default();

    map_res!(
        input,
        ws!(delimited!(
            tag!("("),
            preceded!(
                first!(tag!("module")),
                pair!(opt!(first!(var)), many0!(first!(apply!(module_field, &mut ctxt))))
            ),
            tag!(")")
        )),
        |_| -> Result<Module, Error> {
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
                builder = builder.with_global(global.eval(&ctxt)?);
            }

            for table in &ctxt.tables {
                builder.push_table(table.eval(&ctxt)?);
            }

            Ok(builder.build())
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
        }} |
        apply!(table, ctxt) => { |(bind, table)| {
            trace!("table {:?} = {:?}", bind, table);

            let table_ref = ctxt.tables.get_or_insert(table);

            if let Some(Var::Name(name)) = bind {
                ctxt.table_names.insert(name, table_ref);
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
        pair!(first!(global_type), apply!(instr_list, ctxt)) => {
            |(global_type, init_expr)| Global { global_type, init_expr }
        } |
        pair!(first!(inline_import), first!(global_type)) => {
            |((module_name, item_name), global_type)| Global { global_type, init_expr: vec![] }
        } |
        pair!(first!(inline_export), first!(apply!(global_fields, ctxt))) => {
            |(export_name, global)| global
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
        preceded!(first!(tag!("mut")), first!(value_type)),
        tag!(")")
    ))
);

named!(
    inline_import<(String, String)>,
    ws!(delimited!(
        tag!("("),
        preceded!(first!(tag!("import")), pair!(first!(string), first!(string))),
        tag!(")")
    ))
);

named!(
    inline_export<String>,
    ws!(delimited!(
        tag!("("),
        preceded!(first!(tag!("export")), first!(string)),
        tag!(")")
    ))
);

named_args!(
    table<'a>(ctxt: &'a mut Context)<(Option<Var>, Table)>,
    ws!(delimited!(
        tag!("("),
        preceded!(first!(tag!("table")), pair!(opt!(first!(var)), first!(apply!(table_fields, ctxt)))),
        tag!(")")
    ))
);

named_args!(
    table_fields<'a>(ctxt: &'a mut Context)<Table>,
    alt!(
        first!(table_type) => { |table_type| Table { table_type, elements: vec![] } } |
        pair!(first!(inline_import), first!(table_type)) => {
            |((module_name, item_name), table_type)| Table { table_type, elements: vec![] }
        } |
        pair!(first!(inline_export), first!(apply!(table_fields, ctxt))) => {
            |(export_name, table_def)| table_def
        } |
        preceded!(
            first!(elem_type),
            ws!(delimited!(
                tag!("("),
                preceded!(first!(tag!("elem")), first!(var_list)),
                tag!(")")
            ))
        ) => { |elements: Vec<_>|
            Table {
                table_type: TableType::new(elements.len() as u32, Some(elements.len() as u32)),
                elements
            }
        }
    )
);

#[cfg(test)]
mod tests {
    use std::mem;
    use std::str;

    use parity_wasm::elements::Opcode::*;
    use parity_wasm::elements::ValueType::*;
    use pretty_env_logger;

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
                vec![
                    Type::Function(FunctionType::new(vec![F32, F64, I32, F64, I32, I32], None)),
                ],
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
                (Some("a"), I32, false, Some(I32Const(-2))),
            ),
            (
                b"(module (global (;1;) f32 (f32.const -3)))",
                (None, F32, false, Some(F32Const(unsafe { mem::transmute(-3.0f32) }))),
            ),
            (
                b"(module (global (;2;) f64 (f64.const -4)))",
                (None, F64, false, Some(F64Const(unsafe { mem::transmute(-4.0f64) }))),
            ),
            (
                b"(module (global $b i64 (i64.const -5)))",
                (Some("b"), I64, false, Some(I64Const(-5))),
            ),
            (
                b"(module (global $x (mut i32) (i32.const -12)))",
                (Some("x"), I32, true, Some(I32Const(-12))),
            ),
            (
                b"(module (global (;5;) (mut f32) (f32.const -13)))",
                (None, F32, true, Some(F32Const(unsafe { mem::transmute(-13.0f32) }))),
            ),
            (
                b"(module (global (;6;) (mut f64) (f64.const -14)))",
                (None, F64, true, Some(F64Const(unsafe { mem::transmute(-14.0f64) }))),
            ),
            (
                b"(module (global $y (mut i64) (i64.const -15)))",
                (Some("y"), I64, true, Some(I64Const(-15))),
            ),
            (
                br#"(module (global (import "spectest" "global_i32") i32))"#,
                (Some("spectest.global_i32"), I32, false, None),
            ),
            (
                br#"(module (global (export "global-i32") i32 (i32.const 55)))"#,
                (Some("global_i32"), I32, false, Some(I32Const(55))),
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
                        global.init_expr().code().first().cloned(),
                    ))
                    .unwrap(),
                (global_type, is_mutable, constant.clone()),
                "parse global: {}",
                unsafe { str::from_utf8_unchecked(code) },
            );
        }
    }
}
