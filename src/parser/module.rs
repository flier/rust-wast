use super::{data, elem, export, func, global, import, memory, opt_bind_var, start, table, typedef, var, LPAR, MODULE,
            RPAR};
use ast::{Var, ModuleField};

named!(pub typeidx<Var>, first!(var));
named!(pub funcidx<Var>, first!(var));
named!(pub tableidx<Var>, first!(var));
named!(pub memidx<Var>, first!(var));
named!(pub globalidx<Var>, first!(var));
named!(pub localidx<Var>, first!(var));
named!(pub labelidx<Var>, first!(var));

named!(module_var_opt<Option<Var>>, call!(opt_bind_var));

named!(
    pub module<(Option<Var>, Vec<ModuleField>)>,
    parsing!(
        Module,
        delimited!(
            LPAR,
            preceded!(MODULE, pair!(module_var_opt, module_fields)),
            RPAR
        )
    )
);

named!(pub inline_module<Vec<ModuleField>>, call!(module_fields));
named!(pub inline_module1<Vec<ModuleField>>, call!(module_fields1));

named!(module_fields<Vec<ModuleField>>, many0!(first!(module_field)));
named!(module_fields1<Vec<ModuleField>>, many1!(first!(module_field)));

named!(
    module_field<ModuleField>,
    alt!(
        typedef => { |(bind, func_type)|
            ModuleField::TypeDef(bind, func_type)
        } |
        import => { |(bind, import)|
            ModuleField::Import(bind, import)
        } |
        func => { |(bind, func)|
            ModuleField::Function(bind, func)
        } |
        table => { |(bind, table)|
            ModuleField::Table(bind, table)
        } |
        memory => { |(bind, memory)|
            ModuleField::Memory(bind, memory)
        } |
        global => { |(bind, global)|
            ModuleField::Global(bind, global)
        } |
        export => { |export|
            ModuleField::Export(export)
        } |
        start => { |entry|
            ModuleField::Start(entry)
        } |
        elem => { |elem|
            ModuleField::Element(elem)
        } |
        data => { |data|
            ModuleField::Data(data)
        }
    )
);
