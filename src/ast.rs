use std::mem;

use parity_wasm::elements::{BlockType, FunctionType, GlobalType, InitExpr, MemoryType, Opcode, TableType};

#[derive(Clone, Debug, PartialEq)]
pub enum Var {
    Index(u32),
    Id(String),
}

impl Var {
    pub fn index(v: u32) -> Self {
        Var::Index(v)
    }

    pub fn id<S: Into<String>>(s: S) -> Self {
        Var::Id(s.into())
    }
}

#[derive(Clone, Debug)]
pub struct Global {
    pub global_type: GlobalType,
    pub init_expr: InitExpr,
}

impl Global {
    pub fn new(global_type: GlobalType, init_expr: InitExpr) -> Self {
        Global { global_type, init_expr }
    }
}

impl PartialEq for Global {
    fn eq(&self, other: &Self) -> bool {
        self.global_type.content_type() == other.global_type.content_type()
            && self.global_type.is_mutable() == other.global_type.is_mutable()
            && self.init_expr.code() == other.init_expr.code()
    }
}

#[derive(Clone, Debug)]
pub struct Memory {
    pub memory_type: MemoryType,
    pub elements: Vec<u8>,
}

impl PartialEq for Memory {
    fn eq(&self, other: &Self) -> bool {
        self.memory_type.limits().initial() == other.memory_type.limits().initial()
            && self.memory_type.limits().maximum() == other.memory_type.limits().maximum()
            && self.elements == other.elements
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
    CallIndirect(Option<Var>, Option<FunctionType>),
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
