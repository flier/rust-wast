use failure::Error;
use nom::IResult;

use parity_wasm::builder::{signature, ModuleBuilder};
use parity_wasm::elements::{FunctionType, GlobalType, InitExpr, MemoryType, Module, TableType, Type, ValueType};

use ast::{init_expr, Context, Data, Elem, Global, Memory, Table};
use parse::{elem_type, memory_type, name, string, string_list, table_type, value_type, value_type_list, var, var_list,
            IndexSpace, Var};

fn module(input: &[u8]) -> IResult<&[u8], Module> {
    let mut ctxt = Context::default();

    map_res!(
        input,
        delimited!(
            tag!("("),
            preceded!(
                first!(tag!("module")),
                pair!(opt!(first!(var)), many0!(first!(apply!(module_field, &mut ctxt))))
            ),
            tag!(")")
        ),
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
        global => { |(bind, global)| {
            trace!("global {:?} = {:?}", bind, global);

            let global_ref = ctxt.globals.get_or_insert(global);

            if let Some(Var::Name(name)) = bind {
                ctxt.global_names.insert(name, global_ref);
            }
        }} |
        table => { |(bind, table)| {
            trace!("table {:?} = {:?}", bind, table);

            let table_ref = ctxt.tables.get_or_insert(table);

            if let Some(Var::Name(name)) = bind {
                ctxt.table_names.insert(name, table_ref);
            }
        }} |
        elem => { |elem| {
            trace!("elem {:?}", elem);

            ctxt.elems.get_or_insert(elem);
        }} |
        memory => { |(bind, memory)| {
            trace!("memory {:?} = {:?}", bind, memory);

            let mem_ref = ctxt.memories.get_or_insert(memory);

            if let Some(Var::Name(name)) = bind {
                ctxt.memory_names.insert(name, mem_ref);
            }
        }} |
        data => { |data| {
            trace!("data {:?}", data);

            ctxt.data.get_or_insert(data);
        }} |
        start => { |entry| {
            trace!("start {:?}", entry);

            ctxt.entry = Some(entry);
        }}
    )
);

named!(bind_var<Var>, map!(name, |s| Var::Name(s.to_owned())));

named!(
    type_def<(Option<Var>, FunctionType)>,
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

named!(
    global<(Option<Var>, Global)>,
    parsing!(
        Global,
        delimited!(
            tag!("("),
            preceded!(
                first!(tag!("global")),
                pair!(opt!(first!(bind_var)), first!(global_fields))
            ),
            tag!(")")
        )
    )
);

named!(
    global_fields<Global>,
    alt!(
        pair!(first!(global_type), first!(init_expr)) => {
            |(global_type, init_expr)| Global { global_type, init_expr: init_expr }
        } |
        pair!(first!(inline_import), first!(global_type)) => {
            |((module_name, item_name), global_type)| Global { global_type, init_expr: InitExpr::new(vec![]) }
        } |
        pair!(first!(inline_export), first!(global_fields)) => {
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
    delimited!(tag!("("), preceded!(first!(tag!("mut")), first!(value_type)), tag!(")"))
);

named!(
    inline_import<(String, String)>,
    delimited!(
        tag!("("),
        preceded!(first!(tag!("import")), pair!(first!(string), first!(string))),
        tag!(")")
    )
);

named!(
    inline_export<String>,
    delimited!(tag!("("), preceded!(first!(tag!("export")), first!(string)), tag!(")"))
);

named!(
    table<(Option<Var>, Table)>,
    parsing!(
        Table,
        dbg_dmp!(delimited!(
            tag!("("),
            preceded!(
                first!(tag!("table")),
                pair!(opt!(first!(bind_var)), first!(table_fields))
            ),
            tag!(")")
        ))
    )
);

named!(
    table_fields<Table>,
    alt_complete!(
        first!(table_type) => { |table_type| Table { table_type, elements: vec![] } } |
        pair!(first!(inline_import), first!(table_type)) => {
            |((module_name, item_name), table_type)| Table { table_type, elements: vec![] }
        } |
        pair!(first!(inline_export), first!(table_fields)) => {
            |(export_name, table)| table
        } |
        preceded!(
            first!(elem_type),
            first!(delimited!(
                tag!("("),
                preceded!(first!(tag!("elem")), var_list),
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

named!(
    elem<Elem>,
    parsing!(
        Elem,
        map!(
            delimited!(
                tag!("("),
                preceded!(
                    first!(tag!("elem")),
                    tuple!(
                        opt!(first!(var)),
                        alt_complete!(first!(offset) | first!(init_expr)),
                        var_list
                    )
                ),
                tag!(")")
            ),
            |(table_index, offset, elements)| Elem {
                table_index: table_index.unwrap_or(Var::Index(0)),
                offset,
                elements,
            }
        )
    )
);

named!(
    memory<(Option<Var>, Memory)>,
    parsing!(
        Memory,
        dbg_dmp!(delimited!(
            tag!("("),
            preceded!(
                first!(tag!("memory")),
                pair!(opt!(first!(bind_var)), first!(memory_fields))
            ),
            tag!(")")
        ))
    )
);

named!(
    memory_fields<Memory>,
    alt!(
        first!(memory_type) => { |memory_type| Memory { memory_type, elements: vec![] } } |
        pair!(first!(inline_import), first!(memory_type)) => {
            |((module_name, item_name), memory_type)| Memory { memory_type, elements: vec![] }
        } |
        pair!(first!(inline_export), first!(memory_fields)) => {
            |(export_name, memory)| memory
        } |
        delimited!(
            tag!("("),
            preceded!(first!(tag!("data")), string_list),
            tag!(")")
        ) => { |strs: Vec<String>|
        {
            let elements = strs.into_iter().fold(vec![], |mut value, s| {
                value.extend_from_slice(s.as_bytes());
                value
            });

            Memory{
                memory_type: MemoryType::new(elements.len() as u32, Some(elements.len() as u32)),
                elements,
            }
        }}
    )
);

named!(
    data<Data>,
    parsing!(
        Data,
        dbg_dmp!(map!(
            delimited!(
                tag!("("),
                preceded!(
                    first!(tag!("data")),
                    tuple!(
                        opt!(first!(var)),
                        alt_complete!(first!(offset) | first!(init_expr)),
                        string_list
                    )
                ),
                tag!(")")
            ),
            |(mem_index, offset, strs)| Data {
                mem_index: mem_index.unwrap_or(Var::Index(0)),
                offset,
                value: strs.into_iter().fold(vec![], |mut value, s| {
                    value.extend_from_slice(s.as_bytes());
                    value
                }),
            }
        ))
    )
);

named!(
    offset<InitExpr>,
    delimited!(
        tag!("("),
        preceded!(first!(tag!("offset")), first!(init_expr)),
        tag!(")")
    )
);

named!(
    start<Var>,
    delimited!(tag!("("), preceded!(first!(tag!("start")), first!(var)), tag!(")"))
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

    #[test]
    fn parse_table() {
        let _ = pretty_env_logger::try_init();

        let tests: Vec<(&[u8], _)> = vec![
            (
                b"(table anyfunc (elem $f))",
                (
                    None,
                    Table {
                        table_type: TableType::new(1, Some(1)),
                        elements: vec![Var::Name("f".to_owned())],
                    },
                ),
            ),
            (
                br#"(table (import "spectest" "table") 10 20 anyfunc)"#,
                (
                    None,
                    Table {
                        table_type: TableType::new(10, Some(20)),
                        elements: vec![],
                    },
                ),
            ),
            (
                br#"(table (export "table-10-inf") 10 anyfunc)"#,
                (
                    None,
                    Table {
                        table_type: TableType::new(10, None),
                        elements: vec![],
                    },
                ),
            ),
        ];

        for (code, ref result) in tests {
            let res = table(code);

            trace_parse_error!(code, res);

            assert_eq!(res, IResult::Done(&[][..], result.clone()), "parse table: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_memory() {
        let _ = pretty_env_logger::try_init();

        let tests: Vec<(&[u8], _)> = vec![
            (
                b"(memory 0 0)",
                (
                    None,
                    Memory {
                        memory_type: MemoryType::new(0, Some(0)),
                        elements: vec![],
                    },
                ),
            ),
            (
                b"(memory 0 1)",
                (
                    None,
                    Memory {
                        memory_type: MemoryType::new(0, Some(1)),
                        elements: vec![],
                    },
                ),
            ),
            (
                b"(memory 1 256)",
                (
                    None,
                    Memory {
                        memory_type: MemoryType::new(1, Some(256)),
                        elements: vec![],
                    },
                ),
            ),
            (
                b"(memory 0 65536)",
                (
                    None,
                    Memory {
                        memory_type: MemoryType::new(0, Some(65536)),
                        elements: vec![],
                    },
                ),
            ),
            (
                b"(memory (data))",
                (
                    None,
                    Memory {
                        memory_type: MemoryType::new(0, Some(0)),
                        elements: vec![],
                    },
                ),
            ),
            (
                br#"(memory (data ""))"#,
                (
                    None,
                    Memory {
                        memory_type: MemoryType::new(0, Some(0)),
                        elements: vec![],
                    },
                ),
            ),
            (
                br#"(memory (data "x"))"#,
                (
                    None,
                    Memory {
                        memory_type: MemoryType::new(1, Some(1)),
                        elements: b"x".to_vec(),
                    },
                ),
            ),
            (
                br#"(memory (import "spectest" "memory") 1 2)"#,
                (
                    None,
                    Memory {
                        memory_type: MemoryType::new(1, Some(2)),
                        elements: vec![],
                    },
                ),
            ),
            (
                br#"(memory (export "memory-2-inf") 2)"#,
                (
                    None,
                    Memory {
                        memory_type: MemoryType::new(2, None),
                        elements: vec![],
                    },
                ),
            ),
        ];

        for (code, ref result) in tests {
            let res = memory(code);

            trace_parse_error!(code, res);

            assert_eq!(
                res,
                IResult::Done(&[][..], result.clone()),
                "parse memory: {}",
                unsafe { str::from_utf8_unchecked(code) }
            );
        }
    }

    #[test]
    fn parse_data() {
        let _ = pretty_env_logger::try_init();

        let tests: Vec<(&[u8], _)> = vec![
            (
                br#"(data (i32.const 0))"#,
                Data {
                    mem_index: Var::Index(0),
                    offset: InitExpr::new(vec![I32Const(0)]),
                    value: vec![],
                },
            ),
            (
                br#"(data (i32.const 1) "a" "" "bcd")"#,
                Data {
                    mem_index: Var::Index(0),
                    offset: InitExpr::new(vec![I32Const(1)]),
                    value: b"abcd".to_vec(),
                },
            ),
            (
                br#"(data (offset (i32.const 0)))"#,
                Data {
                    mem_index: Var::Index(0),
                    offset: InitExpr::new(vec![I32Const(0)]),
                    value: vec![],
                },
            ),
            (
                br#"(data (offset (i32.const 0)) "" "a" "bc" "")"#,
                Data {
                    mem_index: Var::Index(0),
                    offset: InitExpr::new(vec![I32Const(0)]),
                    value: b"abc".to_vec(),
                },
            ),
            (
                br#"(data 0 (i32.const 0))"#,
                Data {
                    mem_index: Var::Index(0),
                    offset: InitExpr::new(vec![I32Const(0)]),
                    value: vec![],
                },
            ),
            (
                br#"(data 0x0 (i32.const 1) "a" "" "bcd")"#,
                Data {
                    mem_index: Var::Index(0),
                    offset: InitExpr::new(vec![I32Const(1)]),
                    value: b"abcd".to_vec(),
                },
            ),
            (
                br#"(data 0x000 (offset (i32.const 0)))"#,
                Data {
                    mem_index: Var::Index(0),
                    offset: InitExpr::new(vec![I32Const(0)]),
                    value: vec![],
                },
            ),
            (
                br#"(data 0 (offset (i32.const 0)) "" "a" "bc" "")"#,
                Data {
                    mem_index: Var::Index(0),
                    offset: InitExpr::new(vec![I32Const(0)]),
                    value: b"abc".to_vec(),
                },
            ),
            (
                br#"(data $m (i32.const 0))"#,
                Data {
                    mem_index: Var::Name("m".to_owned()),
                    offset: InitExpr::new(vec![I32Const(0)]),
                    value: vec![],
                },
            ),
            (
                br#"(data $m (i32.const 1) "a" "" "bcd")"#,
                Data {
                    mem_index: Var::Name("m".to_owned()),
                    offset: InitExpr::new(vec![I32Const(1)]),
                    value: b"abcd".to_vec(),
                },
            ),
            (
                br#"(data $m (offset (i32.const 0)))"#,
                Data {
                    mem_index: Var::Name("m".to_owned()),
                    offset: InitExpr::new(vec![I32Const(0)]),
                    value: vec![],
                },
            ),
            (
                br#"(data $m (offset (i32.const 0)) "" "a" "bc" "")"#,
                Data {
                    mem_index: Var::Name("m".to_owned()),
                    offset: InitExpr::new(vec![I32Const(0)]),
                    value: b"abc".to_vec(),
                },
            ),
        ];

        for (code, ref value) in tests {
            let res = data(code);

            trace_parse_error!(code, res);

            assert_eq!(res, IResult::Done(&[][..], value.clone()), "parse data: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_elem() {
        let _ = pretty_env_logger::try_init();

        let tests: Vec<(&[u8], _)> = vec![
            (
                b"(elem (i32.const 0))",
                Elem {
                    table_index: Var::Index(0),
                    offset: InitExpr::new(vec![I32Const(0)]),
                    elements: vec![],
                },
            ),
            (
                b"(elem (i32.const 0) $f $f)",
                Elem {
                    table_index: Var::Index(0),
                    offset: InitExpr::new(vec![I32Const(0)]),
                    elements: vec![Var::Name("f".to_owned()), Var::Name("f".to_owned())],
                },
            ),
            (
                b"(elem (offset (i32.const 0)))",
                Elem {
                    table_index: Var::Index(0),
                    offset: InitExpr::new(vec![I32Const(0)]),
                    elements: vec![],
                },
            ),
            (
                b"(elem (offset (i32.const 0)) $f $f)",
                Elem {
                    table_index: Var::Index(0),
                    offset: InitExpr::new(vec![I32Const(0)]),
                    elements: vec![Var::Name("f".to_owned()), Var::Name("f".to_owned())],
                },
            ),
            (
                b"(elem 0 (i32.const 0))",
                Elem {
                    table_index: Var::Index(0),
                    offset: InitExpr::new(vec![I32Const(0)]),
                    elements: vec![],
                },
            ),
            (
                b"(elem 0x0 (i32.const 0) $f $f)",
                Elem {
                    table_index: Var::Index(0),
                    offset: InitExpr::new(vec![I32Const(0)]),
                    elements: vec![Var::Name("f".to_owned()), Var::Name("f".to_owned())],
                },
            ),
            (
                b"(elem 0x000 (offset (i32.const 0)))",
                Elem {
                    table_index: Var::Index(0),
                    offset: InitExpr::new(vec![I32Const(0)]),
                    elements: vec![],
                },
            ),
            (
                b"(elem 0 (offset (i32.const 0)) $f $f)",
                Elem {
                    table_index: Var::Index(0),
                    offset: InitExpr::new(vec![I32Const(0)]),
                    elements: vec![Var::Name("f".to_owned()), Var::Name("f".to_owned())],
                },
            ),
            (
                b"(elem $t (i32.const 0))",
                Elem {
                    table_index: Var::Name("t".to_owned()),
                    offset: InitExpr::new(vec![I32Const(0)]),
                    elements: vec![],
                },
            ),
            (
                b"(elem $t (i32.const 0) $f $f)",
                Elem {
                    table_index: Var::Name("t".to_owned()),
                    offset: InitExpr::new(vec![I32Const(0)]),
                    elements: vec![Var::Name("f".to_owned()), Var::Name("f".to_owned())],
                },
            ),
            (
                b"(elem $t (offset (i32.const 0)))",
                Elem {
                    table_index: Var::Name("t".to_owned()),
                    offset: InitExpr::new(vec![I32Const(0)]),
                    elements: vec![],
                },
            ),
            (
                b"(elem $t (offset (i32.const 0)) $f $f)",
                Elem {
                    table_index: Var::Name("t".to_owned()),
                    offset: InitExpr::new(vec![I32Const(0)]),
                    elements: vec![Var::Name("f".to_owned()), Var::Name("f".to_owned())],
                },
            ),
        ];

        for (code, ref value) in tests {
            let res = elem(code);

            trace_parse_error!(code, res);

            assert_eq!(res, IResult::Done(&[][..], value.clone()), "parse elem: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_start() {
        let _ = pretty_env_logger::try_init();

        let tests: Vec<(&[u8], _)> = vec![
            (b"(start $main)", Var::Name("main".to_owned())),
            (b"(start 2)", Var::Index(2)),
        ];

        for (code, ref value) in tests {
            let res = start(code);

            trace_parse_error!(code, res);

            assert_eq!(res, IResult::Done(&[][..], value.clone()), "parse start: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }
}
