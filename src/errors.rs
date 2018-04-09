use std::collections::HashMap;

use nom::{ErrorKind, IResult};
use nom::{prepare_errors, print_codes, print_offsets, error_to_u32};

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
    Begin = 128,
    Action,
    Assertion,
    Block,
    BlockInstr,
    Comment,
    Data,
    Elem,
    Export,
    Expr,
    Floating,
    Function,
    FuncType,
    Global,
    HexNum,
    Id,
    IfBlock,
    Import,
    InlineExport,
    InlineImport,
    Instr,
    Int32,
    Int64,
    Integer,
    Local,
    Memory,
    Nat,
    Nat32,
    Num,
    Param,
    PlainInstr,
    Result,
    Start,
    Str,
    Table,
    TypeDef,
    TypeUse,
    Value,
    Var,
}

impl From<Parsing> for ErrorKind {
    fn from(v: Parsing) -> Self {
        ErrorKind::Custom(v as u32)
    }
}

lazy_static! {
    static ref PARSING_VALUES: HashMap<u32, &'static str> = {
        let mut values = HashMap::new();

        values.insert(error_to_u32::<u32>(&ErrorKind::Tag), "tag");
        values.insert(error_to_u32::<u32>(&ErrorKind::MapRes), "map_res");
        values.insert(error_to_u32::<u32>(&ErrorKind::MapOpt), "map_opt");
        values.insert(error_to_u32::<u32>(&ErrorKind::Alt), "alt");
        values.insert(error_to_u32::<u32>(&ErrorKind::IsNot), "is_not");
        values.insert(error_to_u32::<u32>(&ErrorKind::IsA), "is_a");
        values.insert(error_to_u32::<u32>(&ErrorKind::SeparatedList), "separated_list");
        values.insert(
            error_to_u32::<u32>(&ErrorKind::SeparatedNonEmptyList),
            "separated_nonempty_list",
        );
        values.insert(error_to_u32::<u32>(&ErrorKind::Many0), "many0");
        values.insert(error_to_u32::<u32>(&ErrorKind::Many1), "many1");
        values.insert(error_to_u32::<u32>(&ErrorKind::ManyTill), "many_till");
        values.insert(error_to_u32::<u32>(&ErrorKind::Count), "count");
        values.insert(
            error_to_u32::<u32>(&ErrorKind::TakeUntilAndConsume),
            "take_until_and_consume",
        );
        values.insert(error_to_u32::<u32>(&ErrorKind::TakeUntil), "take_until");
        values.insert(
            error_to_u32::<u32>(&ErrorKind::TakeUntilEitherAndConsume),
            "take_until_either_and_consume",
        );
        values.insert(error_to_u32::<u32>(&ErrorKind::TakeUntilEither), "take_until_either");
        values.insert(error_to_u32::<u32>(&ErrorKind::LengthValue), "length_value");
        values.insert(error_to_u32::<u32>(&ErrorKind::TagClosure), "tag_closure");
        values.insert(error_to_u32::<u32>(&ErrorKind::Alpha), "alpha");
        values.insert(error_to_u32::<u32>(&ErrorKind::Digit), "digit");
        values.insert(error_to_u32::<u32>(&ErrorKind::HexDigit), "hex_digit");
        values.insert(error_to_u32::<u32>(&ErrorKind::OctDigit), "oct_digit");
        values.insert(error_to_u32::<u32>(&ErrorKind::AlphaNumeric), "alphanumeric");
        values.insert(error_to_u32::<u32>(&ErrorKind::Space), "space");
        values.insert(error_to_u32::<u32>(&ErrorKind::MultiSpace), "multispace");
        values.insert(error_to_u32::<u32>(&ErrorKind::LengthValueFn), "length_value");
        values.insert(error_to_u32::<u32>(&ErrorKind::Eof), "eof");
        values.insert(error_to_u32::<u32>(&ErrorKind::ExprOpt), "expr_opt");
        values.insert(error_to_u32::<u32>(&ErrorKind::ExprRes), "expr_res");
        values.insert(error_to_u32::<u32>(&ErrorKind::CondReduce), "cond_reduce");
        values.insert(error_to_u32::<u32>(&ErrorKind::Switch), "switch");
        values.insert(error_to_u32::<u32>(&ErrorKind::TagBits), "tag_bits");
        values.insert(error_to_u32::<u32>(&ErrorKind::OneOf), "one_of");
        values.insert(error_to_u32::<u32>(&ErrorKind::NoneOf), "none_of");
        values.insert(error_to_u32::<u32>(&ErrorKind::Char), "char");
        values.insert(error_to_u32::<u32>(&ErrorKind::CrLf), "crlf");
        values.insert(error_to_u32::<u32>(&ErrorKind::RegexpMatch), "regexp_match");
        values.insert(error_to_u32::<u32>(&ErrorKind::RegexpMatches), "regexp_matches");
        values.insert(error_to_u32::<u32>(&ErrorKind::RegexpFind), "find");
        values.insert(error_to_u32::<u32>(&ErrorKind::RegexpCapture), "capture");
        values.insert(error_to_u32::<u32>(&ErrorKind::RegexpCaptures), "captures");
        values.insert(error_to_u32::<u32>(&ErrorKind::TakeWhile1), "take_while1");
        values.insert(error_to_u32::<u32>(&ErrorKind::Complete), "complete");
        values.insert(error_to_u32::<u32>(&ErrorKind::Fix), "fix");
        values.insert(error_to_u32::<u32>(&ErrorKind::Escaped), "escaped");
        values.insert(error_to_u32::<u32>(&ErrorKind::EscapedTransform), "escaped_transform");
        values.insert(error_to_u32::<u32>(&ErrorKind::TagStr), "tag_s");
        values.insert(error_to_u32::<u32>(&ErrorKind::IsNotStr), "is_not_s");
        values.insert(error_to_u32::<u32>(&ErrorKind::IsAStr), "is_a_s");
        values.insert(error_to_u32::<u32>(&ErrorKind::TakeWhile1Str), "take_while1_s");
        values.insert(error_to_u32::<u32>(&ErrorKind::NonEmpty), "non_empty");
        values.insert(error_to_u32::<u32>(&ErrorKind::ManyMN), "many_m_n");
        values.insert(
            error_to_u32::<u32>(&ErrorKind::TakeUntilAndConsumeStr),
            "take_until_and_consume_s",
        );
        values.insert(error_to_u32::<u32>(&ErrorKind::TakeUntilStr), "take_until_s");
        values.insert(error_to_u32::<u32>(&ErrorKind::Not), "not");
        values.insert(error_to_u32::<u32>(&ErrorKind::Permutation), "permutation");
        values.insert(error_to_u32::<u32>(&ErrorKind::Verify), "verify");
        values.insert(error_to_u32::<u32>(&ErrorKind::TakeTill1), "take_till1");

        values.insert(Parsing::Action as u32, "action");
        values.insert(Parsing::Assertion as u32, "assert");
        values.insert(Parsing::Block as u32, "block");
        values.insert(Parsing::BlockInstr as u32, "block_instr");
        values.insert(Parsing::Comment as u32, "comment");
        values.insert(Parsing::Data as u32, "data");
        values.insert(Parsing::Elem as u32, "elem");
        values.insert(Parsing::Export as u32, "export");
        values.insert(Parsing::Expr as u32, "expr");
        values.insert(Parsing::Floating as u32, "float");
        values.insert(Parsing::Function as u32, "function");
        values.insert(Parsing::FuncType as u32, "func_type");
        values.insert(Parsing::Global as u32, "global");
        values.insert(Parsing::HexNum as u32, "hexnum");
        values.insert(Parsing::Id as u32, "id");
        values.insert(Parsing::IfBlock as u32, "if_block");
        values.insert(Parsing::Import as u32, "import");
        values.insert(Parsing::InlineExport as u32, "inline_export");
        values.insert(Parsing::InlineImport as u32, "inline_import");
        values.insert(Parsing::Instr as u32, "instr");
        values.insert(Parsing::Int32 as u32, "int32");
        values.insert(Parsing::Int64 as u32, "int64");
        values.insert(Parsing::Integer as u32, "int");
        values.insert(Parsing::Local as u32, "local");
        values.insert(Parsing::Memory as u32, "memory");
        values.insert(Parsing::Nat as u32, "nat");
        values.insert(Parsing::Nat32 as u32, "nat32");
        values.insert(Parsing::Num as u32, "num");
        values.insert(Parsing::Param as u32, "param");
        values.insert(Parsing::PlainInstr as u32, "plain_instr");
        values.insert(Parsing::Result as u32, "result");
        values.insert(Parsing::Start as u32, "start");
        values.insert(Parsing::Str as u32, "str");
        values.insert(Parsing::Table as u32, "table");
        values.insert(Parsing::TypeDef as u32, "typedef");
        values.insert(Parsing::TypeUse as u32, "typeuse");
        values.insert(Parsing::Value as u32, "value");
        values.insert(Parsing::Var as u32, "var");

        values
    };
}

#[macro_export]
macro_rules! parsing {
    ($input:expr, $code:ident, $submac:ident!( $($args:tt)* )) => {
        match $submac!($input, $($args)*) {
            ::nom::IResult::Incomplete(needed)  => ::nom::IResult::Incomplete(needed),
            ::nom::IResult::Done(i, o)          => ::nom::IResult::Done(i, o),
            ::nom::IResult::Error(e)            => {
                let res = ::nom::IResult::Error(error_node_position!(
                    ::nom::ErrorKind::Custom($crate::errors::Parsing::$code as u32), $input, e));

                trace_parse_error!($input, res);

                res
            }
        }
    };
}

pub fn trace_parse_error<O>(input: &[u8], res: IResult<&[u8], O>) {
    if let Some(v) = prepare_errors(input, res) {
        let colors = generate_colors(&v);

        trace!(
            "parser failed: {}\n{}",
            print_codes(colors, PARSING_VALUES.clone()),
            print_offsets(input, 0, &v)
        );
    }
}

pub fn generate_colors(v: &[(ErrorKind<u32>, usize, usize)]) -> HashMap<u32, u8> {
    v.iter()
        .map(|&(ref c, _, _)| {
            if let &ErrorKind::Custom(code) = c {
                code
            } else {
                error_to_u32(c)
            }
        })
        .fold((HashMap::new(), 0), |(mut colors, color), code| {
            colors.insert(code, color + 31);

            (colors, color + 1 % 7)
        })
        .0
}

#[macro_export]
macro_rules! trace_parse_error {
    ($input: expr, $res: expr) => {
        if $res.is_err() {
            $crate::errors::trace_parse_error($input, $res.clone())
        }
    };
}
