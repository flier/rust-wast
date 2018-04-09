use super::{funcidx, globalidx, memidx, string, tableidx, EXPORT, FUNC, GLOBAL, LPAR, MEMORY, RPAR, TABLE};
use ast::{Export, ExportDesc};

named!(
    pub inline_export<String>,
    parsing!(InlineExport,
        delimited!(LPAR, preceded!(EXPORT, first!(string)), RPAR)
    )
);

named!(
    pub export<Export>,
    parsing!(Export,
        map!(
            delimited!(
                LPAR,
                preceded!(EXPORT, tuple!(first!(string), first!(export_desc))),
                RPAR
            ),
            |(name, desc)| Export { name, desc }
        )
    )
);

named!(
    export_desc<ExportDesc>,
    delimited!(
        LPAR,
        alt_complete!(
            preceded!(FUNC, funcidx) => {
                |idx| ExportDesc::Function(idx)
            } |
            preceded!(TABLE, tableidx) => {
                |idx| ExportDesc::Table(idx)
            } |
            preceded!(MEMORY, memidx) => {
                |idx| ExportDesc::Memory(idx)
            } |
            preceded!(GLOBAL, globalidx) => {
                |idx| ExportDesc::Global(idx)
            }
        ),
        RPAR
    )
);

#[cfg(test)]
mod tests {
    use std::str;

    use nom::IResult::Done;

    use super::*;
    use ast::Var;

    #[test]
    fn parse_inline_export() {
        let tests: Vec<(&[u8], _)> = vec![(b"(export \"a\")", "a"), (b"(export \"type-use-7\")", "type-use-7")];

        for (code, result) in tests {
            assert_eq!(
                inline_export(code),
                Done(&[][..], result.to_owned()),
                "parse `{}` failed",
                unsafe { str::from_utf8_unchecked(code) }
            );
        }
    }

    #[test]
    fn parse_export() {
        let tests: Vec<(&[u8], _)> = vec![
            (
                br#"(export "a" (func 0))"#,
                Export {
                    name: "a".to_owned(),
                    desc: ExportDesc::Function(Var::Index(0)),
                },
            ),
            (
                br#"(export "a" (global $a))"#,
                Export {
                    name: "a".to_owned(),
                    desc: ExportDesc::Global(Var::Id("a".to_owned())),
                },
            ),
            (
                br#"(export "a" (table 0))"#,
                Export {
                    name: "a".to_owned(),
                    desc: ExportDesc::Table(Var::Index(0)),
                },
            ),
            (
                br#"(export "a" (memory $a))"#,
                Export {
                    name: "a".to_owned(),
                    desc: ExportDesc::Memory(Var::Id("a".to_owned())),
                },
            ),
        ];

        for (code, result) in tests {
            assert_eq!(
                export(code),
                Done(&[][..], result.to_owned()),
                "parse export: {}",
                unsafe { str::from_utf8_unchecked(code) }
            );
        }
    }
}
