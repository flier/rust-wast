use parity_wasm::elements::{ExportEntry, External, FunctionNameSection, FunctionType, ImportEntry, Internal, Type,
                            TypeSection};

use ast::Var;
use func::func_type;
use parser::{global_type, memory_type, name, string, table_type, var};

pub trait FunctionTypeExt {
    fn is_empty(&self) -> bool;
}

impl FunctionTypeExt for FunctionType {
    fn is_empty(&self) -> bool {
        self.params().is_empty() && self.return_type().is_none()
    }
}

pub trait IndexSpace {
    type Element: PartialEq;

    fn get(&mut self, element: &Self::Element) -> Option<usize>;

    fn get_or_insert(&mut self, element: Self::Element) -> usize;
}

impl<T> IndexSpace for Vec<T>
where
    T: PartialEq,
{
    type Element = T;

    fn get(&mut self, element: &Self::Element) -> Option<usize> {
        self.iter().position(|el| el == element)
    }

    fn get_or_insert(&mut self, element: Self::Element) -> usize {
        self.get(&element).unwrap_or_else(|| {
            let idx = self.len();

            self.push(element);

            idx
        })
    }
}

impl IndexSpace for TypeSection {
    type Element = FunctionType;

    fn get(&mut self, func_type: &FunctionType) -> Option<usize> {
        self.types()
            .iter()
            .position(|ty| Type::Function(func_type.clone()) == *ty)
    }

    fn get_or_insert(&mut self, func_type: FunctionType) -> usize {
        self.get(&func_type).unwrap_or_else(|| {
            let idx = self.types().len();

            self.types_mut().push(Type::Function(func_type));

            idx
        })
    }
}

named_args!(
    import<'a>(funcs: &'a mut TypeSection)<ImportEntry>,
    map!(
        dbg_dmp!(ws!(tuple!(tag!("import"), string, string, apply!(imkind, funcs)))),
        |(_, module, field, external)| ImportEntry::new(module, field, external)
    )
);

named_args!(
    imkind<'a>(funcs: &'a mut TypeSection)<External>,
    delimited!(
        tag!("("),
        alt!(
            ws!(tuple!(tag!("func"), opt!(name), func_type)) => {
                |(_, name, ty)| External::Function(0 /*funcs.get_or_insert(ty) as u32*/)
            } |
            ws!(tuple!(tag!("global"), opt!(name), global_type)) => {
                |(_, name, ty)| External::Global(ty)
            } |
            ws!(tuple!(tag!("table"), opt!(name), table_type)) => {
                |(_, name, ty)| External::Table(ty)
            } |
            ws!(tuple!(tag!("memory"), opt!(name), memory_type)) => {
                |(_, name, ty)| External::Memory(ty)
            }
        ),
        tag!(")")
    )
);

named_args!(
    export_entry<'a>(funcs: &'a FunctionNameSection)<ExportEntry>,
    ws!(
        do_parse!(
            field: ws!(preceded!(tag!("export"), string)) >>
            res: ws!(delimited!(
                tag!("("),
                pair!(
                    alt!(tag!("func") | tag!("table") | tag!("memory") | tag!("global")),
                    map!(var, |var: Var| 0)//var.resolve(funcs.names()))
                ),
                tag!(")")
            )) >>
            entry: switch!(value!(res.0),
                b"func" => value!(ExportEntry::new(field, Internal::Function(res.1))) |
                b"table" => value!(ExportEntry::new(field, Internal::Table(res.1))) |
                b"memory" => value!(ExportEntry::new(field, Internal::Memory(res.1))) |
                b"global" => value!(ExportEntry::new(field, Internal::Global(res.1)))
            ) >>
            ( entry )
        )
    )
);

#[cfg(test)]
mod tests {
    use std::str;

    use parity_wasm::elements::{FunctionType, ValueType};

    use super::*;

    #[test]
    fn parse_export_entry() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"export \"a\" (func 123)", "a"),
            (b"export \"a\" (global 123)", "a"),
            (b"export \"a\" (table $a)", "a"),
            (b"export \"a\" (memory $a)", "a"),
        ];

        let mut funcs = FunctionNameSection::default();

        funcs.names_mut().insert(123, "a".to_owned());

        for (code, field) in tests {
            let (remaining, export) = export_entry(code, &funcs).unwrap();

            assert!(remaining.is_empty());
            assert_eq!(export.field(), field);
        }
    }

    #[test]
    fn parse_import() {
        let tests: Vec<(&[u8], _)> = vec![
            (
                br#"import "spectest" "print_i32" (func (param i64))"#,
                ("spectest", "print_i32", External::Function(0)),
            ),
            (
                br#"import "spectest" "print_f64_f64" (func $print_f64_f64 (param f64 f64))"#,
                ("spectest", "print_f64_f64", External::Function(0)),
            ),
        ];

        let mut funcs = TypeSection::with_types(vec![
            Type::Function(FunctionType::new(vec![ValueType::I32], None)),
            Type::Function(FunctionType::new(vec![ValueType::I64], None)),
        ]);

        for (code, (module, field, external)) in tests {
            let res = import(code, &mut funcs);

            trace_parse_error!(code, res);

            assert!(
                res.is_done(),
                "parse code: {}, err: {:?}",
                unsafe { str::from_utf8_unchecked(code) },
                res
            );

            let (remaining, import) = res.unwrap();

            assert_eq!(remaining, &b""[..], "parse code: {}", unsafe {
                str::from_utf8_unchecked(code)
            },);
            assert_eq!(import.module(), module);
            assert_eq!(import.field(), field);
            assert_eq!(format!("{:?}", import.external()), format!("{:?}", external));
        }
    }
}
