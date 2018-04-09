use std::mem;

use parity_wasm::elements::{BlockType, FunctionType, GlobalType, InitExpr, MemoryType, Opcode, TableType, ValueType};

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

#[derive(Clone, Debug, PartialEq)]
pub struct Import {
    pub module: String,
    pub name: String,
    pub desc: ImportDesc,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ImportDesc {
    Function(Option<Var>, Option<FunctionType>),
    Table(TableType),
    Memory(MemoryType),
    Global(GlobalType),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Export {
    pub name: String,
    pub desc: ExportDesc,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExportDesc {
    Function(Var),
    Table(Var),
    Memory(Var),
    Global(Var),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Global {
    pub global_type: GlobalType,
    pub init_expr: InitExpr,
}

impl Global {
    pub fn new(global_type: GlobalType, init_expr: InitExpr) -> Self {
        Global { global_type, init_expr }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Memory {
    pub memory_type: MemoryType,
    pub elements: Vec<u8>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Table {
    pub table_type: TableType,
    pub elements: Vec<Var>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Data {
    pub mem_index: Var,
    pub offset: InitExpr,
    pub value: Vec<u8>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Elem {
    pub table_index: Var,
    pub offset: InitExpr,
    pub elements: Vec<Var>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub func_idx: Option<Var>,
    pub func_type: Option<FunctionType>,
    pub locals: Vec<Local>,
    pub body: Vec<Instr>,
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Local {
    pub id: Option<Var>,
    pub value_type: Option<ValueType>,
}

impl Local {
    pub fn new<S: Into<String>>(id: S, value_type: ValueType) -> Self {
        Local {
            id: Some(Var::id(id)),
            value_type: Some(value_type),
        }
    }

    pub fn value(value_type: ValueType) -> Self {
        Local {
            id: None,
            value_type: Some(value_type),
        }
    }

    pub fn i32() -> Self {
        Local {
            id: None,
            value_type: Some(ValueType::I32),
        }
    }

    pub fn i64() -> Self {
        Local {
            id: None,
            value_type: Some(ValueType::I64),
        }
    }

    pub fn f32() -> Self {
        Local {
            id: None,
            value_type: Some(ValueType::F32),
        }
    }

    pub fn f64() -> Self {
        Local {
            id: None,
            value_type: Some(ValueType::F64),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ModuleField {
    TypeDef(Option<Var>, FunctionType),
    Import(Option<Var>, Import),
    Function(Option<Var>, Function),
    Table(Option<Var>, Table),
    Memory(Option<Var>, Memory),
    Global(Option<Var>, Global),
    Export(Export),
    Start(Var),
    Element(Elem),
    Data(Data),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Assertion {
    Malformed(ScriptModule, String),
    Invalid(ScriptModule, String),
    Unlinkable(ScriptModule, String),
    Return(Action, Vec<Constant>),
    ReturnCanonicalNan(Action),
    ReturnArithmeticNan(Action),
    Trap(Action, String),
    Exhaustion(Action, String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ScriptModule {
    Encoded(Option<Var>, Vec<u8>),
    Quoted(Option<Var>, String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Action {
    Invoke(Option<Var>, String, Vec<Constant>),
    Get(Option<Var>, String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Cmd {
    Action(Action),
    Assertion(Assertion),
    ScriptModule(ScriptModule),
    Register(String, Option<Var>),
    Meta(Meta),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Meta {
    Script(Option<Var>, Vec<Cmd>),
    Input(Option<Var>, String),
    Output(Option<Var>, Option<String>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Script {
    Commands(Vec<Cmd>),
    InlineModule(Vec<ModuleField>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Instr {
    /// trap unconditionally
    Unreachable,
    /// do nothing
    Nop,
    /// execute in sequence
    Block(Option<String>, BlockType, Vec<Instr>),
    /// loop header
    Loop(Option<String>, BlockType, Vec<Instr>),
    /// conditional
    If(Option<String>, BlockType, Vec<Instr>, Vec<Instr>),
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

pub fn empty_block() -> Instr {
    Instr::Block(None, BlockType::NoResult, vec![])
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
