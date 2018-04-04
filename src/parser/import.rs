use super::{global_type, memory_type, opt_bind_var, string, table_type, typeuse, FUNC, GLOBAL, IMPORT, LPAR, MEMORY,
            RPAR, TABLE};
use ast::{Import, ImportDesc, Var};

named!(
    pub inline_import<(String, String)>,
    delimited!(
        LPAR,
        preceded!(IMPORT, pair!(first!(string), first!(string))),
        RPAR
    )
);

named!(
    pub import<(Option<Var>, Import)>,
    map!(
        delimited!(
            LPAR,
            preceded!(IMPORT, tuple!(first!(string), first!(string), first!(importdesc))),
            RPAR
        ),
        |(module, name, (id, desc))| (id, Import { module, name, desc })
    )
);

named!(
    importdesc<(Option<Var>, ImportDesc)>,
    delimited!(
        LPAR,
        alt!(
            preceded!(FUNC, pair!(opt_bind_var, typeuse)) => {
                |(bind, (type_use, func_type))| (bind, ImportDesc::Function(type_use, func_type))
            } |
            preceded!(TABLE, pair!(opt_bind_var, table_type)) => {
                |(bind, table_type)| (bind, ImportDesc::Table(table_type))
            } |
            preceded!(MEMORY, pair!(opt_bind_var, memory_type)) => {
                |(bind, memory_type)| (bind, ImportDesc::Memory(memory_type))
            } |
            preceded!(GLOBAL, pair!(opt_bind_var, global_type)) => {
                |(bind, global_type)| (bind, ImportDesc::Global(global_type))
            }
        ),
        RPAR
    )
);

#[cfg(test)]
mod tests {
    use std::str;

    use nom::IResult::Done;
    use parity_wasm::elements::{FunctionType, GlobalType, MemoryType, TableType, ValueType::*};

    use super::*;

    #[test]
    fn parse_inline_import() {
        let tests: Vec<(&[u8], _)> = vec![(b"(import \"m\" \"a\")", ("m", "a"))];

        for (code, (module, field)) in tests {
            assert_eq!(
                inline_import(code),
                Done(&[][..], (module.to_owned(), field.to_owned())),
                "parse inline_import: {}",
                unsafe { str::from_utf8_unchecked(code) }
            );
        }
    }

    #[test]
    fn parse_import() {
        let tests: Vec<(&[u8], _)> = vec![
            (
                br#"(import "spectest" "print_i32" (func (param i32)))"#,
                (
                    None,
                    Import {
                        module: "spectest".to_owned(),
                        name: "print_i32".to_owned(),
                        desc: ImportDesc::Function(None, Some(FunctionType::new(vec![I32], None))),
                    },
                ),
            ),
            (
                br#"(import "test" "func-i64->i64" (func $i64->i64 (param i64) (result i64)))"#,
                (
                    Some(Var::Id("i64->i64".to_owned())),
                    Import {
                        module: "test".to_owned(),
                        name: "func-i64->i64".to_owned(),
                        desc: ImportDesc::Function(None, Some(FunctionType::new(vec![I64], Some(I64)))),
                    },
                ),
            ),
            (
                br#"(import "spectest" "table" (table 10 20 anyfunc))"#,
                (
                    None,
                    Import {
                        module: "spectest".to_owned(),
                        name: "table".to_owned(),
                        desc: ImportDesc::Table(TableType::new(10, Some(20))),
                    },
                ),
            ),
            (
                br#"(import "spectest" "memory" (memory 1 2))"#,
                (
                    None,
                    Import {
                        module: "spectest".to_owned(),
                        name: "memory".to_owned(),
                        desc: ImportDesc::Memory(MemoryType::new(1, Some(2))),
                    },
                ),
            ),
            (
                br#"(import "spectest" "global_i32" (global i32))"#,
                (
                    None,
                    Import {
                        module: "spectest".to_owned(),
                        name: "global_i32".to_owned(),
                        desc: ImportDesc::Global(GlobalType::new(I32, false)),
                    },
                ),
            ),
            (
                br#"(import "spectest" "global_i32" (global $x i32))"#,
                (
                    Some(Var::Id("x".to_owned())),
                    Import {
                        module: "spectest".to_owned(),
                        name: "global_i32".to_owned(),
                        desc: ImportDesc::Global(GlobalType::new(I32, false)),
                    },
                ),
            ),
        ];

        for (code, (module, field)) in tests {
            assert_eq!(
                import(code),
                Done(&[][..], (module.to_owned(), field.to_owned())),
                "parse import: {}",
                unsafe { str::from_utf8_unchecked(code) }
            );
        }
    }
}
