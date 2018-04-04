use std::collections::HashMap;

use nom::{ErrorKind, IResult};
use nom::{generate_colors, prepare_errors, print_codes, print_offsets};

#[derive(Debug, Fail)]
pub enum WastError {
    #[fail(display = "out of range: {}", _0)]
    OutOfRange(isize),
    #[fail(display = "not found: {}", _0)]
    NotFound(String),
    #[fail(display = "mismatching label")]
    MismatchingLabel,
}

#[repr(u32)]
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Parsing {
    Num,
    HexNum,
    Nat,
    Nat32,
    Integer,
    Int32,
    Int64,
    Floating,
    Id,
    Str,
    Value,
    Var,
    Comment,
    Block,
    Instr,
    PlainInstr,
    BlockInstr,
    Expr,
    IfBlock,
    FuncType,
    TypeDef,
    TypeUse,
    Global,
    Table,
    Memory,
    Data,
    Elem,
    Start,

    MismatchingLabel,
}

impl From<Parsing> for ErrorKind {
    fn from(v: Parsing) -> Self {
        ErrorKind::Custom(v as u32)
    }
}

lazy_static! {
    static ref PARSING_VALUES: HashMap<u32, &'static str> = {
        let mut values = HashMap::new();

        values.insert(Parsing::Num as u32, "num");
        values.insert(Parsing::HexNum as u32, "hexnum");
        values.insert(Parsing::Nat as u32, "nat");
        values.insert(Parsing::Nat32 as u32, "nat32");
        values.insert(Parsing::Integer as u32, "int");
        values.insert(Parsing::Int32 as u32, "int32");
        values.insert(Parsing::Int64 as u32, "int64");
        values.insert(Parsing::Floating as u32, "float");
        values.insert(Parsing::Id as u32, "id");
        values.insert(Parsing::Str as u32, "str");
        values.insert(Parsing::Value as u32, "value");
        values.insert(Parsing::Var as u32, "var");
        values.insert(Parsing::Comment as u32, "comment");
        values.insert(Parsing::Block as u32, "block");
        values.insert(Parsing::Instr as u32, "instr");
        values.insert(Parsing::PlainInstr as u32, "plain_instr");
        values.insert(Parsing::BlockInstr as u32, "block_instr");
        values.insert(Parsing::Expr as u32, "expr");
        values.insert(Parsing::IfBlock as u32, "if_block");
        values.insert(Parsing::FuncType as u32, "func_type");
        values.insert(Parsing::TypeDef as u32, "typedef");
        values.insert(Parsing::TypeUse as u32, "typeuse");
        values.insert(Parsing::Global as u32, "global");
        values.insert(Parsing::Table as u32, "table");
        values.insert(Parsing::Memory as u32, "memory");
        values.insert(Parsing::Data as u32, "data");
        values.insert(Parsing::Elem as u32, "elem");
        values.insert(Parsing::Start as u32, "start");

        values
    };
}

#[macro_export]
macro_rules! parsing {
    ($input:expr, $code:ident, $submac:ident!( $($args:tt)* )) => {
        match $submac!($input, $($args)*) {
            ::nom::IResult::Incomplete(needed) => {
                trace!("parse {:?} incomplete, {:?}", $crate::errors::Parsing::$code, needed);

                ::nom::IResult::Incomplete(needed)
            },
            ::nom::IResult::Done(i, o)    => ::nom::IResult::Done(i, o),
            ::nom::IResult::Error(e)      => {
                let err = ::nom::IResult::Error(error_node_position!(
                    ::nom::ErrorKind::Custom($crate::errors::Parsing::$code as u32), $input, e));

                $crate::errors::trace_parse_error($input, err.clone());

                err
            }
        }
    };
}

pub fn trace_parse_error<O>(input: &[u8], res: IResult<&[u8], O>) {
    if let Some(v) = prepare_errors(input, res) {
        let colors = generate_colors(&v);

        trace!(
            "parser `{}` failed\n{}",
            print_codes(colors, PARSING_VALUES.clone()),
            print_offsets(input, 0, &v)
        );
    }
}

#[macro_export]
macro_rules! trace_parse_error {
    ($input: expr, $res: expr) => {
        if $res.is_err() {
            $crate::errors::trace_parse_error($input, $res.clone())
        }
    };
}
