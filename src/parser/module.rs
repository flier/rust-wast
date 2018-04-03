use std::collections::HashMap;

use failure::{err_msg, Error};
use nom::IResult;

use parity_wasm::builder::{signature, ModuleBuilder, TableDefinition, TableEntryDefinition};
use parity_wasm::elements::{FunctionNameSection, GlobalEntry, InitExpr, Module, NameMap, Type, TypeSection};

use super::{data, elem, global, memory, table, type_def, var};
use ast::{Data, Elem, Global, Memory, Table, Var};
use errors::WastError::NotFound;
use parse::IndexSpace;

#[derive(Clone, Debug, Default)]
pub struct Context {
    pub types: TypeSection,
    pub typedefs: HashMap<String, usize>,
    pub tables: Vec<Table>,
    pub table_names: HashMap<String, usize>,
    pub elems: Vec<Elem>,
    pub memories: Vec<Memory>,
    pub memory_names: HashMap<String, usize>,
    pub data: Vec<Data>,
    pub funcs: FunctionNameSection,
    pub locals: NameMap,
    pub globals: Vec<Global>,
    pub global_names: HashMap<String, usize>,
    pub labels: NameMap,
    pub entry: Option<Var>,
}

pub trait Resolvable {
    type Output;

    fn resolve(&self, ctxt: &Context) -> Result<Self::Output, Error>;
}

impl Resolvable for Var {
    type Output = u32;

    fn resolve(&self, ctxt: &Context) -> Result<Self::Output, Error> {
        match *self {
            Var::Index(index) => Ok(index),
            Var::Name(ref value) => ctxt.locals
                .iter()
                .find(|&(_, name)| name == value)
                .map(|(idx, _)| idx)
                .ok_or_else(|| NotFound(value.to_owned()).into()),
        }
    }
}

impl Resolvable for Global {
    type Output = GlobalEntry;

    fn resolve(&self, ctxt: &Context) -> Result<Self::Output, Error> {
        Ok(GlobalEntry::new(self.global_type.clone(), self.init_expr.clone()))
    }
}

impl Resolvable for Table {
    type Output = TableDefinition;

    fn resolve(&self, ctxt: &Context) -> Result<Self::Output, Error> {
        let elements = self.elements
            .iter()
            .map(|elem| {
                match *elem {
                    Var::Index(idx) => Ok(idx),
                    Var::Name(ref name) => ctxt.funcs
                        .names()
                        .iter()
                        .position(|(_, ref elem)| *elem == name)
                        .map(|idx| idx as u32)
                        .ok_or_else(|| err_msg("undefined element")),
                }.map(|idx| TableEntryDefinition {
                    offset: InitExpr::empty(),
                    values: vec![idx],
                })
            })
            .collect::<Result<Vec<TableEntryDefinition>, Error>>()?;

        Ok(TableDefinition {
            min: self.table_type.limits().initial(),
            max: self.table_type.limits().maximum(),
            elements,
        })
    }
}

pub fn module(input: &[u8]) -> IResult<&[u8], Module> {
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
                builder = builder.with_global(global.resolve(&ctxt)?);
            }

            for table in &ctxt.tables {
                builder.push_table(table.resolve(&ctxt)?);
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

named!(
    start<Var>,
    delimited!(tag!("("), preceded!(first!(tag!("start")), first!(var)), tag!(")"))
);

#[cfg(test)]
mod tests {
    use std::str;

    use super::*;

    #[test]
    fn parse_start() {
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
