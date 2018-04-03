use parity_wasm::builder::FunctionBuilder;
use parity_wasm::elements::{FunctionNameSection, ValueType};

use ast::Var;
use parser::{func_type, inline_export, inline_import, type_use, value_type, value_type_list, var};

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
