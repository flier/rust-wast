use parity_wasm::builder::{ExportBuilder, FunctionBuilder, ImportBuilder};
use parity_wasm::elements::{FunctionNameSection, ValueType};

use ast::Var;
use parser::{func_type, string, type_use, value_type, value_type_list, var};

named_args!(
    pub func<'a>(funcs: &'a FunctionNameSection)<(Option<Var>, Option<FunctionBuilder>)>,
    ws!(delimited!(
        tag!("("),
        preceded!(tag!("func"), pair!(opt!(complete!(var)), opt!(complete!(apply!(func_fields, funcs))))),
        tag!(")")
    ))
);

named_args!(
    func_fields<'a>(funcs: &'a FunctionNameSection)<FunctionBuilder>,
    ws!(alt_complete!(
        pair!(opt!(type_use), func_type) => { |_| FunctionBuilder::new() } |
        tuple!(inline_import, opt!(type_use), func_type) => { |_| FunctionBuilder::new() } |
        pair!(inline_export, apply!(func_fields, funcs)) => { |_| FunctionBuilder::new() }
    ))
);

named!(
    pub func_fields_param<Vec<ValueType>>,
    ws!(delimited!(
        tag!("("),
        preceded!(tag!("param"), alt!(
            preceded!(var, value_type) => { |ty| vec![ty] } |
            value_type_list => { |types| types }
        )),
        tag!(")")
    ))
);

named!(
    inline_import<ImportBuilder>,
    map!(
        ws!(delimited!(
            tag!("("),
            preceded!(tag!("import"), pair!(string, string)),
            tag!(")")
        )),
        |(module, field)| ImportBuilder::new().path(&module, &field)
    )
);

named!(
    inline_export<ExportBuilder>,
    map!(
        ws!(delimited!(tag!("("), preceded!(tag!("export"), string), tag!(")"))),
        |field| ExportBuilder::new().field(&field)
    )
);

#[cfg(test)]
mod tests {
    use std::str;

    use super::*;

    #[test]
    fn parse_inline_import() {
        let tests: Vec<(&[u8], _)> = vec![(b"(import \"m\" \"a\")", ("m", "a"))];

        for (code, result) in tests {
            let res = inline_import(code);

            assert!(res.is_done(), "parse `{}` failed", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_inline_export() {
        let tests: Vec<(&[u8], _)> = vec![(b"(export \"a\")", "a")];

        for (code, result) in tests {
            let res = inline_export(code);

            assert!(res.is_done(), "parse `{}` failed", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }
}
