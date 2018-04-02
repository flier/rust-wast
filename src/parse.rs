use std::{i32, i64};
use std::str::{self, FromStr};

use failure::Error;
use nom::{self, IResult};
use nom::ErrorKind::*;
use parity_wasm::elements::{ExportEntry, External, FunctionNameSection, FunctionType, GlobalType, ImportEntry,
                            Internal, MemoryType, NameMap, TableType, Type, TypeSection, ValueType};

use func::func_type;
use errors::WastError;

pub trait FunctionTypeExt {
    fn is_empty(&self) -> bool;
}

impl FunctionTypeExt for FunctionType {
    fn is_empty(&self) -> bool {
        self.params().is_empty() && self.return_type().is_none()
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

/// num:    <digit> (_? <digit>)*
named!(
    pub num<String>,
    parsing!(Num,
        map!(
            map_res!(
                recognize!(
                    separated_nonempty_list_complete!(tag!("_"), nom::digit)
                ),
                str::from_utf8
            ),
            |s: &str| s.replace("_", "")
        )
    )
);

/// hexnum: <hexdigit> (_? <hexdigit>)*
named!(
    pub hexnum<String>,
    parsing!(HexNum,
        map!(
            map_res!(
                recognize!(
                    separated_nonempty_list_complete!(tag!("_"), nom::hex_digit)
                ),
                str::from_utf8
            ),
            |s: &str| s.replace("_", "")
        )
    )
);

/// nat:    <num> | 0x<hexnum>
named!(
    pub nat<isize>,
    parsing!(Nat,
        alt_complete!(
            preceded!(
                tag!("0x"),
                map_res!(hexnum, |s: String| isize::from_str_radix(&s, 16))
            ) |
            map_res!(num, |s: String| isize::from_str_radix(&s, 10))
        )
    )
);

named!(
    pub nat32<u32>,
    parsing!(Nat32,
        alt_complete!(
            preceded!(
                tag!("0x"),
                map_res!(hexnum, |s: String| u32::from_str_radix(&s, 16))
            ) |
            map_res!(num, |s: String| u32::from_str_radix(&s, 10))
        )
    )
);

/// int:    <nat> | +<nat> | -<nat>
named!(
    pub int<isize>,
    parsing!(Integer,
        alt_complete!(
            preceded!(tag!("+"), nat) |
            map_res!(preceded!(tag!("-"), nat), |n: isize| {
                n.checked_neg().ok_or_else(|| WastError::OutOfRange(n))
            }) |
            nat
        )
    )
);

named!(
    pub int32<i32>,
    parsing!(Int32,
        map!(verify!(int, |n| i32::MIN as isize <= n && n <= i32::MAX as isize), |n| n as i32)
    )
);

named!(
    pub int64<i64>,
    parsing!(Int64,
        map!(verify!(int, |n| i64::MIN as isize <= n && n <= i64::MAX as isize), |n| n as i64)
    )
);

/// float:  <num>.<num>?(e|E <num>)? | 0x<hexnum>.<hexnum>?(p|P <num>)?
named!(
    pub float<String>,
    parsing!(Floating,
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
    )
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
    parsing!(Name,
        map_res!(
            preceded!(
                tag!("$"),
                take_while1!(|b| nom::is_alphanumeric(b) || SYMBOL.contains(&b))
            ),
            str::from_utf8
        )
    )
);

#[macro_export]
macro_rules! first(
    ($input:expr, $submacro:ident!($($arguments:tt)*)) => (
        {
            preceded!(
                $input,
                call!($crate::parse::skip),
                $submacro!($($arguments)*)
            )
        }
    );

    ($input:expr, $f:expr) => (
        first!($input, call!($f));
    );
);

named!(pub skip, recognize!(many0!(alt!(comment | whitespace))));

const SYMBOL: &[u8] = b"_.+-*/\\^~=<>!?@#$%&|:'`";

/// string: "(<char> | \n | \t | \\ | \' | \" | \<hex><hex> | \u{<hex>+})*"
named!(
    pub string<String>,
    parsing!(Str,
        alt_complete!(
            tag!("\"\"") => { |_| String::default() } |
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
        )
    )
);

fn string_char(input: &[u8]) -> IResult<&[u8], &[u8], u32> {
    match input.iter().position(|&b| b == b'\\' || b == b'\"') {
        None => IResult::Done(&input[input.len()..], input),
        Some(index) if index > 0 => IResult::Done(&input[index..], &input[..index]),
        _ => IResult::Error(error_position!(Tag, input)),
    }
}

named!(pub string_list<Vec<String>>, many0!(first!(string)));

#[cfg_attr(rustfmt, rustfmt_skip)]
named!(
    comment,
    parsing!(Comment,
        alt!(
            preceded!(tag!("(;"), take_until_and_consume!(";)")) |
            preceded!(tag!(";;"), re_bytes_find_static!(r"^(?-u).*?(\r\n|\n|$)"))
        )
    )
);

named!(pub whitespace, is_a!(" \t\n\r"));

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
    parsing!(Value,
        alt_complete!(
            int => { |v| Value::Int(v) } |
            float64 => { |v| Value::Float(v) }
        )
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
    parsing!(Var,
        alt_complete!(
            name => { |v: &str| Var::Name(v.to_owned()) } |
            nat => { |v| Var::Index(v as u32) }
        )
    )
);

named!(pub var_list<Vec<Var>>, many0!(first!(var)));

named!(pub value_type_list<Vec<ValueType>>, many0!(first!(value_type)));

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
    ws!(delimited!(tag!("("), preceded!(tag!("mut"), value_type), tag!(")")))
);

named!(pub elem_type, tag!("anyfunc"));

named!(
    pub table_type<TableType>,
    map!(
        pair!(limits, first!(elem_type)),
        |((min, max), _)| TableType::new(min, max)
    )
);

named!(limits<(u32, Option<u32>)>, pair!(first!(nat32), opt!(first!(nat32))));

named!(
    memory_type<MemoryType>,
    map!(ws!(pair!(nat32, opt!(nat32))), |(min, max)| MemoryType::new(min, max))
);

named_args!(
    import<'a>(funcs: &'a mut TypeSection)<ImportEntry>,
    map!(
        dbg_dmp!(ws!(tuple!(tag!("import"), string, string, apply!(imkind, funcs)))),
        |(_, module, field, external)| ImportEntry::new(module, field, external)
    )
);

named_args!(
    imkind<'a>(funcs: &'a mut TypeSection)<External>,
    delimited!(
        tag!("("),
        alt!(
            ws!(tuple!(tag!("func"), opt!(name), func_type)) => {
                |(_, name, ty)| External::Function(0 /*funcs.get_or_insert(ty) as u32*/)
            } |
            ws!(tuple!(tag!("global"), opt!(name), global_type)) => {
                |(_, name, ty)| External::Global(ty)
            } |
            ws!(tuple!(tag!("table"), opt!(name), table_type)) => {
                |(_, name, ty)| External::Table(ty)
            } |
            ws!(tuple!(tag!("memory"), opt!(name), memory_type)) => {
                |(_, name, ty)| External::Memory(ty)
            }
        ),
        tag!(")")
    )
);

named_args!(
    export_entry<'a>(funcs: &'a FunctionNameSection)<ExportEntry>,
    ws!(
        do_parse!(
            field: ws!(preceded!(tag!("export"), string)) >>
            res: ws!(delimited!(
                tag!("("),
                pair!(
                    alt!(tag!("func") | tag!("table") | tag!("memory") | tag!("global")),
                    map_res!(var, |var: Var| var.resolve_ref(funcs.names()))
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

    use nom::Err::{NodePosition, Position};
    use nom::IResult::{Done, Error, Incomplete};
    use nom::Needed;
    use parity_wasm::elements::FunctionType;

    use super::*;
    use errors::Parsing::*;

    #[test]
    fn parse_number() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"123", Done(&[][..], "123".to_owned())),
            (b"123_456 abc", Done(&b" abc"[..], "123456".to_owned())),
            (b"123_456_abc", Done(&b"_abc"[..], "123456".to_owned())),
            (
                b"",
                Error(NodePosition(
                    Custom(Num as u32),
                    &[][..],
                    vec![Position(Complete, &[][..])],
                )),
            ),
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
            (b"123", Done(&[][..], "123".to_owned())),
            (b"123_456 abc", Done(&b" abc"[..], "123456".to_owned())),
            (b"123_456_abc", Done(&b""[..], "123456abc".to_owned())),
            (
                b"",
                Error(NodePosition(
                    Custom(HexNum as u32),
                    &[][..],
                    vec![Position(Complete, &[][..])],
                )),
            ),
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
            (b"123", Done(&[][..], 123)),
            (b"123_456 abc", Done(&b" abc"[..], 123_456)),
            (b"0x123_456_abc", Done(&b""[..], 0x123_456_abc)),
            (
                b"",
                Error(NodePosition(Custom(Nat as u32), &[][..], vec![Position(Alt, &[][..])])),
            ),
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
            (b"+123", Done(&[][..], 123)),
            (b"-123_456 abc", Done(&b" abc"[..], -123_456)),
            (b"0x1234_5678_90ab_cdef", Done(&b""[..], 0x1234_5678_90ab_cdef)),
            (
                b"",
                Error(NodePosition(
                    Custom(Integer as u32),
                    &[][..],
                    vec![Position(Alt, &[][..])],
                )),
            ),
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
            (b"123.456e2", Done(&[][..], 123.456e2)),
            (b"123.e2", Done(&[][..], 123.0e2)),
            (b"123.456", Done(&[][..], 123.456)),
            (b"123.", Done(&[][..], 123.0)),
            (b"+123.456e2", Done(&[][..], 123.456e2)),
            (b"+123.e2", Done(&[][..], 123.0e2)),
            (b"+123.456", Done(&[][..], 123.456)),
            (b"+123.", Done(&[][..], 123.0)),
            (b"-123.456e2", Done(&[][..], -123.456e2)),
            (b"-123.e2", Done(&[][..], -123.0e2)),
            (b"-123.456", Done(&[][..], -123.456)),
            (b"-123.", Done(&[][..], -123.0)),
            (b"0x123.456p2", Done(&[][..], 29111.1)),
            (b"0x123.p2", Done(&[][..], 29100.0)),
            (b"0x123.456", Done(&[][..], 291.111)),
            (b"0x123.", Done(&[][..], 291.0)),
            (b"+0x123.456p2", Done(&[][..], 29111.1)),
            (b"+0x123.p2", Done(&[][..], 29100.0)),
            (b"+0x123.456", Done(&[][..], 291.111)),
            (b"+0x123.", Done(&[][..], 291.0)),
            (b"-0x123.456p2", Done(&[][..], -29111.1)),
            (b"-0x123.p2", Done(&[][..], -29100.0)),
            (b"-0x123.456", Done(&[][..], -291.111)),
            (b"-0x123.", Done(&[][..], -291.0)),
            (b"0x1p127", Done(&[][..], 1.0e127)),
            (
                b"",
                Error(NodePosition(
                    Custom(Floating as u32),
                    &[][..],
                    vec![Position(Alt, &[][..])],
                )),
            ),
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
            (b"$a", Done(&b""[..], "a")),
            (b"$foo bar", Done(&b" bar"[..], "foo")),
            (b"", Incomplete(Needed::Size(1))),
            (b"$", Incomplete(Needed::Size(2))),
            (
                b"+a",
                Error(NodePosition(
                    Custom(Name as u32),
                    &b"+a"[..],
                    vec![Position(Tag, &b"+a"[..])],
                )),
            ),
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
            (b"\"\"", Done(&b""[..], "".to_owned())),
            (b"\"hello world\"", Done(&b""[..], "hello world".to_owned())),
            (
                b"\"hello \\r\\n\\t\\\\\\'\\\"world\"",
                Done(&b""[..], "hello \r\n\t\\'\"world".to_owned()),
            ),
            (
                b"",
                Error(NodePosition(Custom(Str as u32), &[][..], vec![Position(Alt, &[][..])])),
            ),
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
            (b"0", Done(&b""[..], super::Var::Index(0))),
            (b"$done", Done(&b""[..], super::Var::Name("done".to_owned()))),
        ];

        for (code, ref result) in tests {
            assert_eq!(var(code), *result, "parse align: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_global_type() {
        let tests: Vec<(&[u8], _, _)> = vec![(b"i32", ValueType::I32, false), (b"(mut i64)", ValueType::I64, true)];

        for (code, value_type, is_mutable) in tests {
            let (remaining, global_type) = global_type(code).unwrap();

            assert!(remaining.is_empty());
            assert_eq!(global_type.content_type(), value_type);
            assert_eq!(global_type.is_mutable(), is_mutable);
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

        let mut funcs = FunctionNameSection::default();

        funcs.names_mut().insert(123, "a".to_owned());

        for (code, field) in tests {
            let (remaining, export) = export_entry(code, &funcs).unwrap();

            assert!(remaining.is_empty());
            assert_eq!(export.field(), field);
        }
    }

    #[test]
    fn parse_comment() {
        let tests: Vec<(&[u8], _)> = vec![
            (b";; foobar", Done(&[][..], &b" foobar"[..])),
            (b";; foo\nbar", Done(&b"bar"[..], &b" foo\n"[..])),
            (b";; foo\r\nbar", Done(&b"bar"[..], &b" foo\r\n"[..])),
            (b";; foo;;bar", Done(&b""[..], &b" foo;;bar"[..])),
            (b"(;foobar;)", Done(&b""[..], &b"foobar"[..])),
            (b"(;foo(;foobar;)bar;)", Done(&b"bar;)"[..], &b"foo(;foobar"[..])),
        ];

        for (code, result) in tests {
            assert_eq!(comment(code), result, "parse comment: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_import() {
        let tests: Vec<(&[u8], _)> = vec![
            (
                br#"import "spectest" "print_i32" (func (param i64))"#,
                ("spectest", "print_i32", External::Function(0)),
            ),
            (
                br#"import "spectest" "print_f64_f64" (func $print_f64_f64 (param f64 f64))"#,
                ("spectest", "print_f64_f64", External::Function(0)),
            ),
        ];

        let mut funcs = TypeSection::with_types(vec![
            Type::Function(FunctionType::new(vec![ValueType::I32], None)),
            Type::Function(FunctionType::new(vec![ValueType::I64], None)),
        ]);

        for (code, (module, field, external)) in tests {
            let res = import(code, &mut funcs);

            trace_parse_error!(code, res);

            assert!(
                res.is_done(),
                "parse code: {}, err: {:?}",
                unsafe { str::from_utf8_unchecked(code) },
                res
            );

            let (remaining, import) = res.unwrap();

            assert_eq!(remaining, &b""[..], "parse code: {}", unsafe {
                str::from_utf8_unchecked(code)
            },);
            assert_eq!(import.module(), module);
            assert_eq!(import.field(), field);
            assert_eq!(format!("{:?}", import.external()), format!("{:?}", external));
        }
    }
}
