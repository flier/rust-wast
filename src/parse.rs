use std::{i32, i64};
use std::str::{self, FromStr};

use failure::Error;
use nom::{self, ErrorKind, IResult};
use parity_wasm::elements::{ExportEntry, GlobalType, Internal, NameMap, ValueType};

use errors::WastError;

/// num:    <digit> (_? <digit>)*
named!(
    pub num<String>,
    map!(map_res!(recognize!(separated_nonempty_list_complete!(
        tag!("_"),
        nom::digit
    )), str::from_utf8), |s: &str| s.replace("_", ""))
);

/// hexnum: <hexdigit> (_? <hexdigit>)*
named!(
    pub hexnum<String>,
    map!(map_res!(recognize!(separated_nonempty_list_complete!(
        tag!("_"),
        nom::hex_digit
    )), str::from_utf8), |s: &str| s.replace("_", ""))
);

/// nat:    <num> | 0x<hexnum>
named!(
    pub nat<isize>,
    alt_complete!(
        preceded!(
            tag!("0x"),
            map_res!(hexnum, |s: String| isize::from_str_radix(&s, 16))
        ) |
        map_res!(num, |s: String| isize::from_str_radix(&s, 10))
    )
);

named!(
    pub nat32<i32>,
    alt_complete!(
        preceded!(
            tag!("0x"),
            map_res!(hexnum, |s: String| i32::from_str_radix(&s, 16))
        ) |
        map_res!(num, |s: String| i32::from_str_radix(&s, 10))
    )
);

/// int:    <nat> | +<nat> | -<nat>
named!(
    pub int<isize>,
    alt_complete!(
        preceded!(tag!("+"), nat) |
        map_res!(preceded!(tag!("-"), nat), |n: isize| {
            n.checked_neg().ok_or_else(|| WastError::OutOfRange(n))
        }) |
        nat
    )
);

named!(
    pub int32<i32>,
    map!(verify!(int, |n| i32::MIN as isize <= n && n <= i32::MAX as isize), |n| n as i32)
);

named!(
    pub int64<i64>,
    map!(verify!(int, |n| i64::MIN as isize <= n && n <= i64::MAX as isize), |n| n as i64)
);

/// float:  <num>.<num>?(e|E <num>)? | 0x<hexnum>.<hexnum>?(p|P <num>)?
named!(
    pub float<String>,
    map!(pair!(opt!(sign), alt_complete!(
        preceded!(
            tag!("0x"),
            tuple!(
                map_res!(hexnum, |s: String| isize::from_str_radix(&s, 16)),
                opt!(tag!(".")),
                opt!(complete!(map_res!(hexnum, |s: String| isize::from_str_radix(&s, 16)))),
                opt!(complete!(preceded!(tag_no_case!("p"), num)))
            )
        ) |
        tuple!(
            map_res!(num, |s: String| isize::from_str(&s)),
            opt!(tag!(".")),
            opt!(complete!(map_res!(num, |s: String| isize::from_str(&s)))),
            opt!(complete!(preceded!(tag_no_case!("e"), num)))
        )
    )), |(sign, (num, _, frac, exp)): (Option<&str>, (isize, _, Option<isize>, Option<String>))| {
        format!("{}{}.{}e{}", sign.unwrap_or_default(), num, frac.unwrap_or(0), exp.unwrap_or("0".to_owned()))
    })
);

named!(pub float32<f32>, map_res!(float, |s: String| {
    trace!("parsing f32: {}", s);

    f32::from_str(&s)
}));
named!(pub float64<f64>, map_res!(float, |s: String| {
    trace!("parsing f64: {}", s);

    f64::from_str(&s)
}));

named!(
    sign<&str>,
    map_res!(alt_complete!(tag!("+") | tag!("-")), str::from_utf8)
);

/// name:   $(<letter> | <digit> | _ | . | + | - | * | / | \ | ^ | ~ | = | < | > | ! | ? | @ | # | $ | % | & | | | : | ' | `)+
named!(
    pub name<&str>,
    map_res!(
        preceded!(
            tag!("$"),
            take_while1!(|b| nom::is_alphanumeric(b) || SYMBOL.contains(&b))
        ),
        str::from_utf8
    )
);

const SYMBOL: &[u8] = b"_.+-*/\\^~=<>!?@#$%&|:'`";

/// string: "(<char> | \n | \t | \\ | \' | \" | \<hex><hex> | \u{<hex>+})*"
named!(
    pub string<String>,
    delimited!(
        tag!("\""),
        map!(
            escaped_transform!(string_char, '\\',
                alt!(
                    tag!("r") => { |_| &b"\r"[..] } |
                    tag!("n") => { |_| &b"\n"[..] } |
                    tag!("t") => { |_| &b"\t"[..] } |
                    tag!("\\") => { |_| &b"\\"[..] } |
                    tag!("'") => { |_| &b"'"[..] } |
                    tag!("\"") => { |_| &b"\""[..] }
                )
            ),
            |s| String::from_utf8_lossy(&s).into_owned()
        ),
        tag!("\"")
    )
);

fn string_char(input: &[u8]) -> IResult<&[u8], &[u8], u32> {
    match input.iter().position(|&b| b == b'\\' || b == b'\"') {
        None => IResult::Done(&input[input.len()..], input),
        Some(index) if index > 0 => IResult::Done(&input[index..], &input[..index]),
        _ => IResult::Error(error_position!(ErrorKind::Tag, input)),
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Value {
    Int(isize),
    Float(f64),
}

impl Value {
    pub fn as_int(&self) -> isize {
        match *self {
            Value::Int(v) => v,
            Value::Float(v) => v as isize,
        }
    }

    pub fn as_float(&self) -> f64 {
        match *self {
            Value::Int(v) => v as f64,
            Value::Float(v) => v,
        }
    }
}

/// value:  <int> | <float>
named!(
    pub value<Value>,
    alt!(
        int => { |v| Value::Int(v) } |
        float64 => { |v| Value::Float(v) }
    )
);

#[derive(Clone, Debug, PartialEq)]
pub enum Var {
    Index(u32),
    Name(String),
}

impl Var {
    pub fn resolve_ref(&self, vars: &NameMap) -> Result<u32, Error> {
        match *self {
            Var::Index(index) => Ok(index),
            Var::Name(ref value) => vars.iter()
                .find(|&(_, name)| name == value)
                .map(|(idx, _)| idx)
                .ok_or_else(|| WastError::NotFound(value.to_owned()).into()),
        }
    }
}

/// var:    <nat> | <name>
named!(
    pub var<Var>,
        alt_complete!(
        name => { |v: &str| Var::Name(v.to_owned()) } |
        nat => { |v| Var::Index(v as u32) }
    )
);

named!(pub value_type_list<Vec<ValueType>>, ws!(many0!(value_type)));

/// value_type: i32 | i64 | f32 | f64
named!(pub value_type<ValueType>, alt!(int_type | float_type ));

named!(
    pub int_type<ValueType>,
    alt!(
        tag!("i32") => { |_| ValueType::I32 } |
        tag!("i64") => { |_| ValueType::I64 }
    )
);

named!(
    pub float_type<ValueType>,
    alt!(
        tag!("f32") => { |_| ValueType::F32 } |
        tag!("f64") => { |_| ValueType::F64 }
    )
);

named!(
    pub global_type<GlobalType>,
    alt!(
        value_type => { |ty| GlobalType::new(ty, false) } |
        mut_value_type => { |ty| GlobalType::new(ty, true) }
    )
);

named!(
    mut_value_type<ValueType>,
    ws!(delimited!(
        tag!("("),
        preceded!(tag!("mut"), value_type),
        tag!(")")
    ))
);

named!(
    inline_export<String>,
    ws!(delimited!(
        tag!("("),
        preceded!(tag!("export"), string),
        tag!(")")
    ))
);

named_args!(
    export_entry<'a>(types: &'a NameMap)<ExportEntry>,
    ws!(
        do_parse!(
            field: ws!(preceded!(tag!("export"), string)) >>
            res: ws!(delimited!(
                tag!("("),
                pair!(
                    alt!(tag!("func") | tag!("table") | tag!("memory") | tag!("global")),
                    map_res!(var, |var: Var| var.resolve_ref(types))
                ),
                tag!(")")
            )) >>
            entry: switch!(value!(res.0),
                b"func" => value!(ExportEntry::new(field, Internal::Function(res.1))) |
                b"table" => value!(ExportEntry::new(field, Internal::Table(res.1))) |
                b"memory" => value!(ExportEntry::new(field, Internal::Memory(res.1))) |
                b"global" => value!(ExportEntry::new(field, Internal::Global(res.1)))
            ) >>
            ( entry )
        )
    )
);

#[cfg(test)]
mod tests {
    use pretty_env_logger;

    use nom::Needed;

    use super::*;

    #[test]
    fn parse_number() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"123", IResult::Done(&[][..], "123".to_owned())),
            (
                b"123_456 abc",
                IResult::Done(&b" abc"[..], "123456".to_owned()),
            ),
            (
                b"123_456_abc",
                IResult::Done(&b"_abc"[..], "123456".to_owned()),
            ),
            (b"", IResult::Error(ErrorKind::Complete)),
        ];

        for (code, ref result) in tests {
            assert_eq!(num(code), *result, "parse num: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_hexnum() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"123", IResult::Done(&[][..], "123".to_owned())),
            (
                b"123_456 abc",
                IResult::Done(&b" abc"[..], "123456".to_owned()),
            ),
            (
                b"123_456_abc",
                IResult::Done(&b""[..], "123456abc".to_owned()),
            ),
            (b"", IResult::Error(ErrorKind::Complete)),
        ];

        for (code, ref result) in tests {
            assert_eq!(hexnum(code), *result, "parse hexnum: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_nat() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"123", IResult::Done(&[][..], 123)),
            (b"123_456 abc", IResult::Done(&b" abc"[..], 123_456)),
            (b"0x123_456_abc", IResult::Done(&b""[..], 0x123_456_abc)),
            (b"", IResult::Error(ErrorKind::Alt)),
        ];

        for (code, ref result) in tests {
            assert_eq!(nat(code), *result, "parse nat: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_int() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"+123", IResult::Done(&[][..], 123)),
            (b"-123_456 abc", IResult::Done(&b" abc"[..], -123_456)),
            (
                b"0x1234_5678_90ab_cdef",
                IResult::Done(&b""[..], 0x1234_5678_90ab_cdef),
            ),
            (b"", IResult::Error(ErrorKind::Alt)),
        ];

        for (code, ref result) in tests {
            assert_eq!(int(code), *result, "parse int: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_float() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"123.456e2", IResult::Done(&[][..], 123.456e2)),
            (b"123.e2", IResult::Done(&[][..], 123.0e2)),
            (b"123.456", IResult::Done(&[][..], 123.456)),
            (b"123.", IResult::Done(&[][..], 123.0)),
            (b"+123.456e2", IResult::Done(&[][..], 123.456e2)),
            (b"+123.e2", IResult::Done(&[][..], 123.0e2)),
            (b"+123.456", IResult::Done(&[][..], 123.456)),
            (b"+123.", IResult::Done(&[][..], 123.0)),
            (b"-123.456e2", IResult::Done(&[][..], -123.456e2)),
            (b"-123.e2", IResult::Done(&[][..], -123.0e2)),
            (b"-123.456", IResult::Done(&[][..], -123.456)),
            (b"-123.", IResult::Done(&[][..], -123.0)),
            (b"0x123.456p2", IResult::Done(&[][..], 29111.1)),
            (b"0x123.p2", IResult::Done(&[][..], 29100.0)),
            (b"0x123.456", IResult::Done(&[][..], 291.111)),
            (b"0x123.", IResult::Done(&[][..], 291.0)),
            (b"+0x123.456p2", IResult::Done(&[][..], 29111.1)),
            (b"+0x123.p2", IResult::Done(&[][..], 29100.0)),
            (b"+0x123.456", IResult::Done(&[][..], 291.111)),
            (b"+0x123.", IResult::Done(&[][..], 291.0)),
            (b"-0x123.456p2", IResult::Done(&[][..], -29111.1)),
            (b"-0x123.p2", IResult::Done(&[][..], -29100.0)),
            (b"-0x123.456", IResult::Done(&[][..], -291.111)),
            (b"-0x123.", IResult::Done(&[][..], -291.0)),
            (b"0x1p127", IResult::Done(&[][..], 1.0e127)),
            (b"", IResult::Error(ErrorKind::Alt)),
        ];

        for (code, ref result) in tests {
            assert_eq!(float64(code), *result, "parse float: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_name() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"$a", IResult::Done(&b""[..], "a")),
            (b"$foo bar", IResult::Done(&b" bar"[..], "foo")),
            (b"", IResult::Incomplete(Needed::Size(1))),
            (b"$", IResult::Incomplete(Needed::Size(2))),
            (b"+a", IResult::Error(ErrorKind::Tag)),
        ];

        for (code, ref result) in tests {
            assert_eq!(name(code), *result, "parse name: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_string() {
        let tests: Vec<(&[u8], _)> = vec![
            (
                b"\"hello world\"",
                IResult::Done(&b""[..], "hello world".to_owned()),
            ),
            (
                b"\"hello \\r\\n\\t\\\\\\'\\\"world\"",
                IResult::Done(&b""[..], "hello \r\n\t\\'\"world".to_owned()),
            ),
            (b"", IResult::Incomplete(Needed::Size(1))),
        ];

        for (code, ref result) in tests {
            assert_eq!(string(code), *result, "parse string: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_var() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"0", IResult::Done(&b""[..], Var::Index(0))),
            (
                b"$done",
                IResult::Done(&b""[..], Var::Name("done".to_owned())),
            ),
        ];

        for (code, ref result) in tests {
            assert_eq!(var(code), *result, "parse align: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_global_type() {
        let tests: Vec<(&[u8], _, _)> = vec![
            (b"i32", ValueType::I32, false),
            (b"(mut i64)", ValueType::I64, true),
        ];

        for (code, value_type, is_mutable) in tests {
            let (remaining, global_type) = global_type(code).unwrap();

            assert!(remaining.is_empty());
            assert_eq!(global_type.content_type(), value_type);
            assert_eq!(global_type.is_mutable(), is_mutable);
        }
    }

    #[test]
    fn parse_inline_export() {
        let tests: Vec<(&[u8], _)> =
            vec![(b"(export \"a\")", IResult::Done(&[][..], "a".to_owned()))];

        for (code, result) in tests {
            assert_eq!(inline_export(code), result);
        }
    }

    #[test]
    fn parse_export_entry() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"export \"a\" (func 123)", "a"),
            (b"export \"a\" (global 123)", "a"),
            (b"export \"a\" (table $a)", "a"),
            (b"export \"a\" (memory $a)", "a"),
        ];

        let mut types = NameMap::default();

        types.insert(123, "a".to_owned());

        for (code, field) in tests {
            let (remaining, export) = export_entry(code, &types).unwrap();

            assert!(remaining.is_empty());
            assert_eq!(export.field(), field);
        }
    }
}
