use std::mem;
use std::collections::HashMap;

use failure::{err_msg, Error};
use itertools;
use parity_wasm::builder::{TableDefinition, TableEntryDefinition};
use parity_wasm::elements::{BlockType, FunctionNameSection, FunctionType, GlobalEntry, GlobalType, InitExpr, NameMap,
                            Opcode, TableType, Type, TypeSection, ValueType};

use func::func_type;
use ops::{align, binary, compare, convert, mem_size, offset, sign, test, unary};
use parse::{value_type, var, FunctionTypeExt, IndexSpace, Var, float32, float64, int32, int64};

#[derive(Clone, Debug, Default)]
pub struct Context {
    pub types: TypeSection,
    pub typedefs: HashMap<String, usize>,
    pub tables: Vec<Table>,
    pub table_names: HashMap<String, usize>,
    pub elems: Vec<Elem>,
    pub memories: NameMap,
    pub data: Vec<Data>,
    pub funcs: FunctionNameSection,
    pub locals: NameMap,
    pub globals: Vec<Global>,
    pub global_names: HashMap<String, usize>,
    pub labels: NameMap,
}

#[derive(Clone, Debug)]
pub struct Global {
    pub global_type: GlobalType,
    pub init_expr: InitExpr,
}

impl PartialEq for Global {
    fn eq(&self, other: &Self) -> bool {
        self.global_type.content_type() == other.global_type.content_type()
            && self.global_type.is_mutable() == other.global_type.is_mutable()
            && self.init_expr.code() == other.init_expr.code()
    }
}

impl Global {
    pub fn eval(&self, ctxt: &Context) -> Result<GlobalEntry, Error> {
        Ok(GlobalEntry::new(self.global_type.clone(), self.init_expr.clone()))
    }
}

#[derive(Clone, Debug)]
pub struct Table {
    pub table_type: TableType,
    pub elements: Vec<Var>,
}

impl PartialEq for Table {
    fn eq(&self, other: &Self) -> bool {
        self.table_type.elem_type() == other.table_type.elem_type()
            && self.table_type.limits().initial() == other.table_type.limits().initial()
            && self.table_type.limits().maximum() == other.table_type.limits().maximum()
            && self.elements == other.elements
    }
}

impl Table {
    pub fn eval(&self, ctxt: &Context) -> Result<TableDefinition, Error> {
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

#[derive(Clone, Debug)]
pub struct Data {
    pub mem_index: Var,
    pub offset: InitExpr,
    pub value: Vec<u8>,
}

impl PartialEq for Data {
    fn eq(&self, other: &Self) -> bool {
        self.mem_index == other.mem_index && self.offset.code() == other.offset.code() && self.value == other.value
    }
}

#[derive(Clone, Debug)]
pub struct Elem {
    pub table_index: Var,
    pub offset: InitExpr,
    pub elements: Vec<Var>,
}

impl PartialEq for Elem {
    fn eq(&self, other: &Self) -> bool {
        self.table_index == other.table_index && self.offset.code() == other.offset.code()
            && self.elements == other.elements
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Instr {
    /// trap unconditionally
    Unreachable,
    /// do nothing
    Nop,
    /// execute in sequence
    Block(BlockType, Vec<Instr>),
    /// loop header
    Loop(BlockType, Vec<Instr>),
    /// conditional
    If(BlockType, Vec<Instr>, Vec<Instr>),
    /// break to n-th surrounding label
    Br(Var),
    /// conditional break
    BrIf(Var),
    /// indexed break
    BrTable(Vec<Var>, Var),
    /// break from function body
    Return,
    /// call function
    Call(Var),
    /// call function through table
    CallIndirect(Var),
    /// forget a value
    Drop,
    /// branchless conditional
    Select,
    /// read local variable
    GetLocal(Var),
    /// write local variable
    SetLocal(Var),
    /// write local variable and keep value
    TeeLocal(Var),
    /// read global variable
    GetGlobal(Var),
    /// write global variable
    SetGlobal(Var),
    /// read memory at address
    Load(Load),
    /// write memory at address
    Store(Store),
    /// size of linear memory
    CurrentMemory,
    /// grow linear memory
    GrowMemory,
    /// constant
    Const(Constant),
    /// numeric test
    Test(Opcode),
    /// numeric comparison
    Compare(Opcode),
    /// unary numeric operator
    Unary(Opcode),
    /// binary numeric operator
    Binary(Opcode),
    /// conversion
    Convert(Opcode),
}

impl Instr {
    pub fn i32(v: i32) -> Self {
        Instr::Const(Constant::I32(v))
    }

    pub fn i64(v: i64) -> Self {
        Instr::Const(Constant::I64(v))
    }

    pub fn f32(v: f32) -> Self {
        Instr::Const(Constant::F32(v))
    }

    pub fn f64(v: f64) -> Self {
        Instr::Const(Constant::F64(v))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Constant {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

impl From<Constant> for Instr {
    fn from(constant: Constant) -> Self {
        Instr::Const(constant)
    }
}

impl From<Constant> for Opcode {
    fn from(constant: Constant) -> Self {
        match constant {
            Constant::I32(v) => Opcode::I32Const(v),
            Constant::I64(v) => Opcode::I64Const(v),
            Constant::F32(v) => Opcode::F32Const(unsafe { mem::transmute(v) }),
            Constant::F64(v) => Opcode::F64Const(unsafe { mem::transmute(v) }),
        }
    }
}

/// <val_type>.const <value>
named!(
    constant<Constant>,
    ws!(switch!(recognize!(pair!(value_type, tag!(".const"))),
            b"i32.const" => map!(int32, |n| Constant::I32(n)) |
            b"i64.const" => map!(int64, |n| Constant::I64(n)) |
            b"f32.const" => map!(float32, |v| Constant::F32(v)) |
            b"f64.const" => map!(float64, |v| Constant::F64(v))
        ))
);

#[derive(Clone, Debug, PartialEq)]
pub enum Load {
    I32(u32, u32),
    I64(u32, u32),
    F32(u32, u32),
    F64(u32, u32),
    I8AsI32(u32, u32),
    U8AsI32(u32, u32),
    I16AsI32(u32, u32),
    U16AsI32(u32, u32),
    I8AsI64(u32, u32),
    U8AsI64(u32, u32),
    I16AsI64(u32, u32),
    U16AsI64(u32, u32),
    I32AsI64(u32, u32),
    U32AsI64(u32, u32),
}

impl From<Load> for Instr {
    fn from(load: Load) -> Self {
        Instr::Load(load)
    }
}

impl From<Load> for Opcode {
    fn from(load: Load) -> Self {
        match load {
            Load::I32(flags, offset) => Opcode::I32Load(flags, offset),
            Load::I64(flags, offset) => Opcode::I64Load(flags, offset),
            Load::F32(flags, offset) => Opcode::F32Load(flags, offset),
            Load::F64(flags, offset) => Opcode::F64Load(flags, offset),
            Load::I8AsI32(flags, offset) => Opcode::I32Load8S(flags, offset),
            Load::U8AsI32(flags, offset) => Opcode::I32Load8U(flags, offset),
            Load::I16AsI32(flags, offset) => Opcode::I32Load16S(flags, offset),
            Load::U16AsI32(flags, offset) => Opcode::I32Load16U(flags, offset),
            Load::I8AsI64(flags, offset) => Opcode::I64Load8S(flags, offset),
            Load::U8AsI64(flags, offset) => Opcode::I64Load8U(flags, offset),
            Load::I16AsI64(flags, offset) => Opcode::I64Load16S(flags, offset),
            Load::U16AsI64(flags, offset) => Opcode::I64Load16U(flags, offset),
            Load::I32AsI64(flags, offset) => Opcode::I64Load32S(flags, offset),
            Load::U32AsI64(flags, offset) => Opcode::I64Load32U(flags, offset),
        }
    }
}

/// <val_type>.load((8|16|32)_<sign>)? <offset>? <align>?
#[cfg_attr(rustfmt, rustfmt_skip)]
named!(
    load<Load>,
    ws!(do_parse!(
        op: recognize!(tuple!(value_type, tag!(".load"), opt!(complete!(tuple!(mem_size, tag!("_"), sign))))) >>
        offset: map!(opt!(complete!(offset)), |n| n.unwrap_or_default()) >>
        align: verify!(
            map!(opt!(complete!(align)), |n| n.unwrap_or_default()),
            |n: u32| n == 0 || n.is_power_of_two()
        ) >>
        opcode: switch!(value!(op),
            b"i32.load" => value!(Load::I32(align, offset)) |
            b"i64.load" => value!(Load::I64(align, offset)) |
            b"f32.load" => value!(Load::F32(align, offset)) |
            b"f64.load" => value!(Load::F64(align, offset)) |
            b"i32.load8_s" => value!(Load::I8AsI32(align, offset)) |
            b"i32.load8_u" => value!(Load::U8AsI32(align, offset)) |
            b"i32.load16_s" => value!(Load::I16AsI32(align, offset)) |
            b"i32.load16_u" => value!(Load::U16AsI32(align, offset)) |
            b"i64.load8_s" => value!(Load::I8AsI64(align, offset)) |
            b"i64.load8_u" => value!(Load::U8AsI64(align, offset)) |
            b"i64.load16_s" => value!(Load::I16AsI64(align, offset)) |
            b"i64.load16_u" => value!(Load::U16AsI64(align, offset)) |
            b"i64.load32_s" => value!(Load::I32AsI64(align, offset)) |
            b"i64.load32_u" => value!(Load::U32AsI64(align, offset))
        ) >>
        ( opcode )
    ))
);

#[derive(Clone, Debug, PartialEq)]
pub enum Store {
    I32(u32, u32),
    I64(u32, u32),
    F32(u32, u32),
    F64(u32, u32),
    I32AsI8(u32, u32),
    I32AsI16(u32, u32),
    I64AsI8(u32, u32),
    I64AsI16(u32, u32),
    I64AsI32(u32, u32),
}

impl From<Store> for Instr {
    fn from(store: Store) -> Self {
        Instr::Store(store)
    }
}

impl From<Store> for Opcode {
    fn from(store: Store) -> Self {
        match store {
            Store::I32(flags, offset) => Opcode::I32Store(flags, offset),
            Store::I64(flags, offset) => Opcode::I64Store(flags, offset),
            Store::F32(flags, offset) => Opcode::F32Store(flags, offset),
            Store::F64(flags, offset) => Opcode::F64Store(flags, offset),
            Store::I32AsI8(flags, offset) => Opcode::I32Store8(flags, offset),
            Store::I32AsI16(flags, offset) => Opcode::I32Store16(flags, offset),
            Store::I64AsI8(flags, offset) => Opcode::I64Store8(flags, offset),
            Store::I64AsI16(flags, offset) => Opcode::I64Store16(flags, offset),
            Store::I64AsI32(flags, offset) => Opcode::I64Store32(flags, offset),
        }
    }
}

/// <val_type>.store(8|16|32)? <offset>? <align>?
#[cfg_attr(rustfmt, rustfmt_skip)]
named!(
    pub store<Store>,
    ws!(do_parse!(
        op: recognize!(tuple!(
                value_type,
                tag!(".store"),
                opt!(complete!(mem_size))
            )) >>
        offset: map!(opt!(complete!(offset)), |n| n.unwrap_or_default()) >>
        align: verify!(
            map!(opt!(complete!(align)), |n| n.unwrap_or_default()),
            |n: u32| n == 0 || n.is_power_of_two()
        ) >>
        opcode: switch!(value!(op),
            b"i32.store" => value!(Store::I32(align, offset)) |
            b"i64.store" => value!(Store::I64(align, offset)) |
            b"f32.store" => value!(Store::F32(align, offset)) |
            b"f64.store" => value!(Store::F64(align, offset)) |
            b"i32.store8" => value!(Store::I32AsI8(align, offset)) |
            b"i32.store16" => value!(Store::I32AsI16(align, offset)) |
            b"i64.store8" => value!(Store::I64AsI8(align, offset)) |
            b"i64.store16" => value!(Store::I64AsI16(align, offset)) |
            b"i64.store32" => value!(Store::I64AsI32(align, offset))
        ) >>
        ( opcode )
    ))
);

named_args!(
    pub instr_list<'a>(ctxt: &'a mut Context)<Vec<Instr>>,
    map!(ws!(many0!(apply!(instr, ctxt))), |instrs| itertools::flatten(instrs).collect())
);

named_args!(pub const_expr<'a>(ctxt: &'a mut Context)<Vec<Instr>>, apply!(instr_list, ctxt));

named_args!(
    pub init_expr<'a>(ctxt: &'a mut Context)<InitExpr>,
    map_res!(apply!(const_expr, ctxt), |instrs: Vec<Instr>| -> Result<InitExpr, Error> {
        let opcodes = instrs
            .into_iter()
            .map(|instr| match instr {
                Instr::Const(constant) => Ok(constant.into()),
                _ => bail!("constant expression required"),
            })
            .collect::<Result<Vec<Opcode>, Error>>()?;

        Ok(InitExpr::new(opcodes))
    })
);

named_args!(
    instr<'a>(ctxt: &'a mut Context)<Vec<Instr>>,
    parsing!(Instr,
        ws!(alt!(
            plain_instr => { |instr| vec![instr] } |
            apply!(call_instr, ctxt) => { |instr| vec![instr] } |
            apply!(block_instr, ctxt) => { |instr| vec![instr] } |
            apply!(expr, ctxt)
        ))
    )
);

named!(
    plain_instr<Instr>,
    parsing!(
        PlainInstr,
        alt_complete!(
            tag!("unreachable") => { |_| Instr::Unreachable } |
            tag!("nop") => { |_| Instr::Nop } |
            ws!(preceded!(tag!("br"), var)) => { |var| Instr::Br(var) } |
            ws!(preceded!(tag!("br_if"), var)) => { |var| Instr::BrIf(var) } |
            ws!(preceded!(tag!("br_table"), pair!(var, many0!(var)))) => {
                |(default, targets)| Instr::BrTable(targets, default)
            } |
            tag!("return") => { |_| Instr::Return } |
            ws!(preceded!(tag!("call"), var)) => { |var| Instr::Call(var) } |
            tag!("drop") => { |_| Instr::Drop } |
            tag!("select") => { |_| Instr::Select } |
            ws!(preceded!(tag!("get_local"), var)) => { |var| Instr::GetLocal(var) } |
            ws!(preceded!(tag!("set_local"), var)) => { |var| Instr::SetLocal(var) } |
            ws!(preceded!(tag!("tee_local"), var)) => { |var| Instr::TeeLocal(var) } |
            ws!(preceded!(tag!("get_global"), var)) => { |var| Instr::GetGlobal(var) } |
            ws!(preceded!(tag!("set_global"), var)) => { |var| Instr::SetGlobal(var) } |
            load => { |load| Instr::Load(load) } |
            store => { |store| Instr::Store(store) } |
            tag!("current_memory") => { |_| Instr::CurrentMemory } |
            tag!("grow_memory") => { |_| Instr::GrowMemory } |
            constant => { |constant| Instr::Const(constant) } |
            test => { |test| Instr::Test(test) } |
            compare => { |compare| Instr::Compare(compare) } |
            unary => { |unary| Instr::Unary(unary) } |
            binary => { |binary| Instr::Binary(binary) } |
            convert => { |convert| Instr::Convert(convert) }
        )
    )
);

named_args!(
    call_instr<'a>(ctxt: &'a mut Context)<Instr>,
    ws!(preceded!(tag!("call_indirect"), apply!(call_instr_type, ctxt)))
);

named_args!(
    call_instr_type<'a>(ctxt: &'a mut Context)<Instr>,
    alt!(
        map!(var, |var| Instr::CallIndirect(var)) |
        map_res!(
            ws!(pair!(opt!(type_use), func_type)),
            |(type_use, func_type): (Option<Var>, FunctionType)| {
                trace!("call_indirect (type {:?}) {:?}", type_use, func_type);

                match type_use {
                    Some(var) => {
                        if !func_type.is_empty() &&
                           ctxt.types.types()
                                .get(var.resolve_ref(ctxt.funcs.names())? as usize)
                                .map_or(true, |ty| *ty == Type::Function(func_type))
                        {
                            bail!("inline function type does not match explicit type")
                        }

                        Ok(Instr::CallIndirect(var))
                    },
                    None => {
                        Ok(Instr::CallIndirect(Var::Index(ctxt.types.get_or_insert(func_type) as u32)))
                    }
                }
            }
        )
    )
);

named!(
    type_use<Var>,
    ws!(delimited!(tag!("("), preceded!(tag!("type"), var), tag!(")")))
);

// named!(call_instr_instr<Instr>, tag!(""));

named_args!(
    block_instr<'a>(ctxt: &'a mut Context)<Instr>,
    parsing!(BlockInstr,
        ws!(alt_complete!(
            tuple!(tag!("block"), opt!(var), apply!(block, ctxt), tag!("end"), opt!(var)) => {
                |(_, _, (block_type, instrs), _, _)| Instr::Block(block_type, instrs)
            } |
            tuple!(tag!("loop"), opt!(var), apply!(block, ctxt), tag!("end"), opt!(var)) => {
                |(_, _, (block_type, instrs), _, _)| Instr::Loop(block_type, instrs)
            } |
            tuple!(tag!("if"), opt!(var), apply!(block, ctxt), tag!("end"), opt!(var)) => {
                |(_, _, (block_type, then), _, _)| Instr::If(block_type, then, vec![])
            } |
            tuple!(tag!("if"), opt!(var), apply!(block, ctxt), tag!("else"), opt!(var), apply!(instr_list, ctxt), tag!("end"), opt!(var)) => {
                |(_, _, (block_type, then), _, _, else_, _, _)| Instr::If(block_type, then, else_)
            }
        ))
    )
);

named_args!(
    block<'a>(ctxt: &'a mut Context)<(BlockType, Vec<Instr>)>,
    parsing!(Block,
        map!(
            pair!(opt!(block_type), apply!(instr_list, ctxt)),
            |(block_type, instrs)| (
                block_type.map_or(BlockType::NoResult, BlockType::Value),
                instrs
            )
        )
    )
);

named!(
    block_type<ValueType>,
    ws!(delimited!(tag!("("), preceded!(tag!("result"), value_type), tag!(")")))
);

named_args!(expr_list<'a>(ctxt: &'a mut Context)<Vec<Instr>>,
    map!(ws!(many0!(apply!(expr, ctxt))), |instrs| itertools::flatten(instrs).collect())
);

named_args!(expr<'a>(ctxt: &'a mut Context)<Vec<Instr>>,
    parsing!(Expr,
        ws!(delimited!(
            tag!("("),
            alt!(
                plain_instr => {
                    |instr| vec![instr]
                } |
                apply!(call_instr, ctxt) => {
                    |instr| vec![instr]
                } |
                tuple!(tag!("block"), opt!(var), apply!(block, ctxt)) => {
                    |(_, label, (block_type, instrs))| vec![Instr::Block(block_type, instrs)]
                } |
                tuple!(tag!("loop"), opt!(var), apply!(block, ctxt)) => {
                    |(_, label, (block_type, instrs))| vec![Instr::Loop(block_type, instrs)]
                } |
                tuple!(tag!("if"), opt!(var), apply!(if_block, ctxt)) => {
                    |(_, label, (mut cond, block_type, then, else_))| {
                        let mut instrs = vec![];

                        instrs.append(&mut cond);
                        instrs.push(Instr::If(block_type, then, else_));

                        instrs
                    }
                }
            ),
            tag!(")")
        ))
    )
);

named_args!(
    if_block<'a>(ctxt: &'a mut Context)<(Vec<Instr>, BlockType, Vec<Instr>, Vec<Instr>)>,
    parsing!(IfBlock,
        map!(
            pair!(opt!(block_type), apply!(if_, ctxt)),
            |(block_type, (cond, then, else_))| (
                cond,
                block_type.map_or(BlockType::NoResult, BlockType::Value),
                then,
                else_.unwrap_or_default()
            )
        )
    )
);

named_args!(
    if_<'a>(ctxt: &'a mut Context)<(Vec<Instr>, Vec<Instr>, Option<Vec<Instr>>)>,
    ws!(tuple!(
        apply!(expr, ctxt),
        delimited!(
            tag!("("),
                preceded!(tag!("then"), apply!(instr_list, ctxt)),
            tag!(")")
        ),
        opt!(delimited!(
            tag!("("),
                preceded!(tag!("else"), apply!(instr_list, ctxt)),
            tag!(")")
        ))
    ))
);

#[cfg(test)]
mod tests {
    use std::str;

    use nom::IResult::*;
    use parity_wasm::elements::ValueType::*;
    use pretty_env_logger;

    use super::*;
    use super::Instr::*;

    #[test]
    fn parse_plain_instr() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"unreachable", Done(&[][..], Unreachable)),
            (b"nop", Done(&[][..], Nop)),
            (b"br 123", Done(&[][..], Br(Var::Index(123)))),
            (b"br $end", Done(&[][..], Br(Var::Name("end".to_owned())))),
            (b"br_if 123", Done(&[][..], BrIf(Var::Index(123)))),
            (b"br_if $end", Done(&[][..], BrIf(Var::Name("end".to_owned())))),
            (b"br_table 0", Done(&[][..], BrTable(vec![], Var::Index(0)))),
            (
                b"br_table 0 1 2 3",
                Done(
                    &[][..],
                    BrTable(vec![Var::Index(1), Var::Index(2), Var::Index(3)], Var::Index(0)),
                ),
            ),
            (b"return", Done(&[][..], Return)),
            (b"call 123", Done(&[][..], Call(Var::Index(123)))),
            (b"call $name", Done(&[][..], Call(Var::Name("name".to_owned())))),
            (b"get_local 123", Done(&[][..], GetLocal(Var::Index(123)))),
            (
                b"get_local $name",
                Done(&[][..], GetLocal(Var::Name("name".to_owned()))),
            ),
            (b"set_local 123", Done(&[][..], SetLocal(Var::Index(123)))),
            (
                b"set_local $name",
                Done(&[][..], SetLocal(Var::Name("name".to_owned()))),
            ),
            (b"tee_local 123", Done(&[][..], TeeLocal(Var::Index(123)))),
            (
                b"tee_local $name",
                Done(&[][..], TeeLocal(Var::Name("name".to_owned()))),
            ),
            (b"get_global 123", Done(&[][..], GetGlobal(Var::Index(123)))),
            (
                b"get_global $name",
                Done(&[][..], GetGlobal(Var::Name("name".to_owned()))),
            ),
            (b"set_global 123", Done(&[][..], SetGlobal(Var::Index(123)))),
            (
                b"set_global $name",
                Done(&[][..], SetGlobal(Var::Name("name".to_owned()))),
            ),
            (b"current_memory", Done(&[][..], CurrentMemory)),
            (b"grow_memory", Done(&[][..], GrowMemory)),
        ];

        for &(code, ref result) in tests.iter() {
            assert_eq!(plain_instr(code), *result, "parse instr: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_call_instr() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"call_indirect 123", Done(&[][..], CallIndirect(Var::Index(123)))),
            (
                b"call_indirect $hello",
                Done(&[][..], CallIndirect(Var::Name("hello".to_owned()))),
            ),
            (
                b"call_indirect (type $hello)",
                Done(&[][..], CallIndirect(Var::Name("hello".to_owned()))),
            ),
            (b"call_indirect (param i32)", Done(&[][..], CallIndirect(Var::Index(0)))),
            (b"call_indirect (param i64)", Done(&[][..], CallIndirect(Var::Index(1)))),
            (
                b"call_indirect (param i32) (param i64) (result f32)",
                Done(&[][..], CallIndirect(Var::Index(2))),
            ),
        ];

        let mut ctxt = Context::default();

        ctxt.types
            .types_mut()
            .push(Type::Function(FunctionType::new(vec![I32], None)));

        for &(code, ref result) in tests.iter() {
            assert_eq!(call_instr(code, &mut ctxt), *result, "parse instr: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }

        assert_eq!(
            ctxt.types.types(),
            &[
                Type::Function(FunctionType::new(vec![I32], None)),
                Type::Function(FunctionType::new(vec![I64], None)),
                Type::Function(FunctionType::new(vec![I32, I64], Some(F32))),
            ]
        );
    }

    #[test]
    fn parse_block_instr() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"block\nend", Done(&[][..], Block(BlockType::NoResult, vec![]))),
            (
                b"block (result i32)\nend",
                Done(&[][..], Block(BlockType::Value(I32), vec![])),
            ),
            (
                b"block $done\nend $done",
                Done(&[][..], Block(BlockType::NoResult, vec![])),
            ),
            (
                b"block $done (result i32)\nend $done",
                Done(&[][..], Block(BlockType::Value(I32), vec![])),
            ),
            (
                b"block $done (result i32) nop\nend $done",
                Done(&[][..], Block(BlockType::Value(I32), vec![Nop])),
            ),
            (
                b"loop $done (result i32) unreachable\nend $done",
                Done(&[][..], Loop(BlockType::Value(I32), vec![Unreachable])),
            ),
            (
                b"if $done (result i32) nop unreachable\nend $done",
                Done(&[][..], If(BlockType::Value(I32), vec![Nop, Unreachable], vec![])),
            ),
            (
                b"if $done (result i32) nop\nelse unreachable\nend $done",
                Done(&[][..], If(BlockType::Value(I32), vec![Nop], vec![Unreachable])),
            ),
        ];

        let mut ctxt = Context::default();

        for &(code, ref result) in tests.iter() {
            assert_eq!(block_instr(code, &mut ctxt), *result, "parse instr: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_expr() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"(block)", Done(&[][..], vec![Block(BlockType::NoResult, vec![])])),
            (b"(block $l)", Done(&[][..], vec![Block(BlockType::NoResult, vec![])])),
            (
                b"(block (nop))",
                Done(&[][..], vec![Block(BlockType::NoResult, vec![Nop])]),
            ),
            (
                b"(block (result i32) (i32.const 7))",
                Done(&[][..], vec![Block(BlockType::Value(I32), vec![Instr::i32(7)])]),
            ),
            (
                b"(block (call $dummy) (call $dummy) (call $dummy) (call $dummy))",
                Done(
                    &[][..],
                    vec![
                        Block(
                            BlockType::NoResult,
                            vec![
                                Call(Var::Name("dummy".to_owned())),
                                Call(Var::Name("dummy".to_owned())),
                                Call(Var::Name("dummy".to_owned())),
                                Call(Var::Name("dummy".to_owned())),
                            ],
                        ),
                    ],
                ),
            ),
            (
                b"(block (result i32)
                    (block (call $dummy) (block) (nop))
                    (block (result i32) (call $dummy) (i32.const 9))
                )",
                Done(
                    &[][..],
                    vec![
                        Block(
                            BlockType::Value(I32),
                            vec![
                                Block(
                                    BlockType::NoResult,
                                    vec![
                                        Call(Var::Name("dummy".to_owned())),
                                        Block(BlockType::NoResult, vec![]),
                                        Nop,
                                    ],
                                ),
                                Block(
                                    BlockType::Value(I32),
                                    vec![Call(Var::Name("dummy".to_owned())), Const(Constant::I32(9))],
                                ),
                            ],
                        ),
                    ],
                ),
            ),
            (
                b"(if (get_local 0) (then))",
                Done(
                    &[][..],
                    vec![GetLocal(Var::Index(0)), If(BlockType::NoResult, vec![], vec![])],
                ),
            ),
            (
                b"(if (get_local 0) (then) (else))",
                Done(
                    &[][..],
                    vec![GetLocal(Var::Index(0)), If(BlockType::NoResult, vec![], vec![])],
                ),
            ),
            (
                b"(if $l (get_local 0) (then))",
                Done(
                    &[][..],
                    vec![GetLocal(Var::Index(0)), If(BlockType::NoResult, vec![], vec![])],
                ),
            ),
            (
                b"(if $l (get_local 0) (then) (else))",
                Done(
                    &[][..],
                    vec![GetLocal(Var::Index(0)), If(BlockType::NoResult, vec![], vec![])],
                ),
            ),
            (
                b"(if (get_local 0) (then (nop)))",
                Done(
                    &[][..],
                    vec![GetLocal(Var::Index(0)), If(BlockType::NoResult, vec![Nop], vec![])],
                ),
            ),
            (
                b"(if (get_local 0) (then (nop)) (else (nop)))",
                Done(
                    &[][..],
                    vec![GetLocal(Var::Index(0)), If(BlockType::NoResult, vec![Nop], vec![Nop])],
                ),
            ),
            (
                b"(if (result i32) (get_local 0) (then (nop)) (else (nop)))",
                Done(
                    &[][..],
                    vec![GetLocal(Var::Index(0)), If(BlockType::Value(I32), vec![Nop], vec![Nop])],
                ),
            ),
            (
                b"(if (get_local 0) (then (call $dummy) (call $dummy) (call $dummy)))",
                Done(
                    &[][..],
                    vec![
                        GetLocal(Var::Index(0)),
                        If(
                            BlockType::NoResult,
                            vec![
                                Call(Var::Name("dummy".to_owned())),
                                Call(Var::Name("dummy".to_owned())),
                                Call(Var::Name("dummy".to_owned())),
                            ],
                            vec![],
                        ),
                    ],
                ),
            ),
            (
                b"(if (get_local 0) (then) (else (call $dummy) (call $dummy) (call $dummy)))",
                Done(
                    &[][..],
                    vec![
                        GetLocal(Var::Index(0)),
                        If(
                            BlockType::NoResult,
                            vec![],
                            vec![
                                Call(Var::Name("dummy".to_owned())),
                                Call(Var::Name("dummy".to_owned())),
                                Call(Var::Name("dummy".to_owned())),
                            ],
                        ),
                    ],
                ),
            ),
            (
                b"(if (result i32) (get_local 0)
                    (then (call $dummy) (call $dummy) (i32.const 8))
                    (else (call $dummy) (call $dummy) (i32.const 9))
                )",
                Done(
                    &[][..],
                    vec![
                        GetLocal(Var::Index(0)),
                        If(
                            BlockType::Value(I32),
                            vec![
                                Call(Var::Name("dummy".to_owned())),
                                Call(Var::Name("dummy".to_owned())),
                                Instr::i32(8),
                            ],
                            vec![
                                Call(Var::Name("dummy".to_owned())),
                                Call(Var::Name("dummy".to_owned())),
                                Instr::i32(9),
                            ],
                        ),
                    ],
                ),
            ),
            (
                b"(if (result i32) (get_local 0)
                    (then
                        (if (get_local 1) (then (call $dummy) (block) (nop)))
                        (if (get_local 1) (then) (else (call $dummy) (block) (nop)))
                        (if (result i32) (get_local 1)
                            (then (call $dummy) (i32.const 9))
                            (else (call $dummy) (i32.const 10))
                        )
                    )
                    (else
                        (if (get_local 1) (then (call $dummy) (block) (nop)))
                        (if (get_local 1) (then) (else (call $dummy) (block) (nop)))
                        (if (result i32) (get_local 1)
                            (then (call $dummy) (i32.const 10))
                            (else (call $dummy) (i32.const 11))
                        )
                    )
                )",
                Done(
                    &[][..],
                    vec![
                        GetLocal(Var::Index(0)),
                        If(
                            BlockType::Value(I32),
                            vec![
                                GetLocal(Var::Index(1)),
                                If(
                                    BlockType::NoResult,
                                    vec![
                                        Call(Var::Name("dummy".to_owned())),
                                        Block(BlockType::NoResult, vec![]),
                                        Nop,
                                    ],
                                    vec![],
                                ),
                                GetLocal(Var::Index(1)),
                                If(
                                    BlockType::NoResult,
                                    vec![],
                                    vec![
                                        Call(Var::Name("dummy".to_owned())),
                                        Block(BlockType::NoResult, vec![]),
                                        Nop,
                                    ],
                                ),
                                GetLocal(Var::Index(1)),
                                If(
                                    BlockType::Value(I32),
                                    vec![Call(Var::Name("dummy".to_owned())), Instr::i32(9)],
                                    vec![Call(Var::Name("dummy".to_owned())), Instr::i32(10)],
                                ),
                            ],
                            vec![
                                GetLocal(Var::Index(1)),
                                If(
                                    BlockType::NoResult,
                                    vec![
                                        Call(Var::Name("dummy".to_owned())),
                                        Block(BlockType::NoResult, vec![]),
                                        Nop,
                                    ],
                                    vec![],
                                ),
                                GetLocal(Var::Index(1)),
                                If(
                                    BlockType::NoResult,
                                    vec![],
                                    vec![
                                        Call(Var::Name("dummy".to_owned())),
                                        Block(BlockType::NoResult, vec![]),
                                        Nop,
                                    ],
                                ),
                                GetLocal(Var::Index(1)),
                                If(
                                    BlockType::Value(I32),
                                    vec![Call(Var::Name("dummy".to_owned())), Instr::i32(10)],
                                    vec![Call(Var::Name("dummy".to_owned())), Instr::i32(11)],
                                ),
                            ],
                        ),
                    ],
                ),
            ),
        ];

        let mut ctxt = Context::default();

        for &(code, ref result) in tests.iter() {
            assert_eq!(expr(code, &mut ctxt), *result, "parse expr: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }
}
