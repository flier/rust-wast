use std::collections::HashMap;

use failure::{err_msg, Error};
use nom::IResult;

use parity_wasm::builder::{signature, ModuleBuilder, TableDefinition, TableEntryDefinition};
use parity_wasm::elements::{FunctionType, GlobalEntry, InitExpr, Module, NameMap, Type, TypeSection};

use super::{data, elem, export, func, global, import, memory, table, typedef, var, LPAR, MODULE, RPAR, START};
use ast::{Data, Elem, Export, Function, Global, Import, Memory, Table, Var};
use errors::WastError::NotFound;

#[derive(Clone, Debug, Default)]
pub struct Context {
    pub types: TypeSection,
    pub typedefs: HashMap<String, usize>,
    pub imports: Vec<Import>,
    pub import_names: HashMap<String, usize>,
    pub funcs: Vec<Function>,
    pub func_names: HashMap<String, usize>,
    pub tables: Vec<Table>,
    pub table_names: HashMap<String, usize>,
    pub memories: Vec<Memory>,
    pub memory_names: HashMap<String, usize>,
    pub globals: Vec<Global>,
    pub global_names: HashMap<String, usize>,
    pub locals: NameMap,
    pub exports: Vec<Export>,
    pub entry: Option<Var>,
    pub elems: Vec<Elem>,
    pub data: Vec<Data>,
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
            Var::Id(ref value) => ctxt.locals
                .iter()
                .find(|&(_, id)| id == value)
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
                    Var::Id(ref id) => ctxt.func_names
                        .get(id)
                        .map(|&idx| idx as u32)
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

named!(pub typeidx<Var>, first!(var));
named!(pub funcidx<Var>, first!(var));
named!(pub tableidx<Var>, first!(var));
named!(pub memidx<Var>, first!(var));
named!(pub globalidx<Var>, first!(var));
named!(pub localidx<Var>, first!(var));
named!(pub labelidx<Var>, first!(var));

pub fn module(input: &[u8]) -> IResult<&[u8], Module> {
    let mut ctxt = Context::default();

    map_res!(
        input,
        delimited!(
            LPAR,
            preceded!(
                MODULE,
                pair!(opt!(first!(var)), many0!(first!(apply!(module_field, &mut ctxt))))
            ),
            RPAR
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
        typedef => { |(bind, func_type)| {
            trace!("typedef {:?} = {:?}", bind, func_type);

            let type_ref = ctxt.types.get_or_insert(func_type);

            if let Some(Var::Id(id)) = bind {
                ctxt.typedefs.insert(id, type_ref);
            }
        }} |
        import => { |(bind, import)| {
            trace!("import {:?} = {:?}", bind, import);

            let import_ref = ctxt.imports.get_or_insert(import);

            if let Some(Var::Id(id)) = bind {
                ctxt.import_names.insert(id, import_ref);
            }
        }} |
        func => { |(bind, func)| {
            trace!("func {:?} = {:?}", bind, func);

            let func_ref = ctxt.funcs.get_or_insert(func);

            if let Some(Var::Id(id)) = bind {
                ctxt.func_names.insert(id, func_ref);
            }
        }} |
        table => { |(bind, table)| {
            trace!("table {:?} = {:?}", bind, table);

            let table_ref = ctxt.tables.get_or_insert(table);

            if let Some(Var::Id(id)) = bind {
                ctxt.table_names.insert(id, table_ref);
            }
        }} |
        memory => { |(bind, memory)| {
            trace!("memory {:?} = {:?}", bind, memory);

            let mem_ref = ctxt.memories.get_or_insert(memory);

            if let Some(Var::Id(id)) = bind {
                ctxt.memory_names.insert(id, mem_ref);
            }
        }} |
        global => { |(bind, global)| {
            trace!("global {:?} = {:?}", bind, global);

            let global_ref = ctxt.globals.get_or_insert(global);

            if let Some(Var::Id(id)) = bind {
                ctxt.global_names.insert(id, global_ref);
            }
        }} |
        export => { |export| {
            trace!("export {:?}", export);

            ctxt.exports.get_or_insert(export);
        }} |
        start => { |entry| {
            trace!("start {:?}", entry);

            ctxt.entry = Some(entry);
        }} |
        elem => { |elem| {
            trace!("elem {:?}", elem);

            ctxt.elems.get_or_insert(elem);
        }} |
        data => { |data| {
            trace!("data {:?}", data);

            ctxt.data.get_or_insert(data);
        }}
    )
);

named!(start<Var>, delimited!(LPAR, preceded!(START, first!(var)), RPAR));

#[cfg(test)]
mod tests {
    use std::str;

    use super::*;

    #[test]
    fn parse_start() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"(start $main)", Var::Id("main".to_owned())),
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
