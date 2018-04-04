use parity_wasm::elements::TableType;

use super::{opt_bind_var, elem_type, inline_export, inline_import, table_type, var_list, ELEM, LPAR, RPAR, TABLE};
use ast::{Table, Var};

named!(
    pub table<(Option<Var>, Table)>,
    parsing!(
        Table,
        dbg_dmp!(delimited!(
            LPAR,
            preceded!(
                TABLE,
                pair!(opt_bind_var, first!(table_fields))
            ),
            RPAR
        ))
    )
);

named!(
    table_fields<Table>,
    alt_complete!(
        table_type => { |table_type| Table { table_type, elements: vec![] } } |
        pair!(inline_import, first!(table_type)) => {
            |((module_name, item_name), table_type)| Table { table_type, elements: vec![] }
        } |
        pair!(inline_export, first!(table_fields)) => {
            |(export_name, table)| table
        } |
        preceded!(
            elem_type,
            first!(delimited!(
                LPAR,
                preceded!(ELEM, var_list),
                RPAR
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
    use std::str;

    use nom::IResult::Done;
    use pretty_env_logger;

    use super::*;

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
                        elements: vec![Var::Id("f".to_owned())],
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

            assert_eq!(res, Done(&[][..], result.clone()), "parse table: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }
}
