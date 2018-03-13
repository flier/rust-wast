use std::str::{self, FromStr};

use nom::{self, ErrorKind, IResult};

use errors::WastError;

pub trait Parse
where
    Self: Sized,
{
    fn parse(s: &[u8]) -> IResult<&[u8], Self>;
}

/// num:    <digit> (_? <digit>)*
named!(
    pub num<usize>,
    map_res!(map_res!(recognize!(separated_nonempty_list_complete!(
        tag!("_"),
        nom::digit
    )), str::from_utf8), |s: &str| usize::from_str_radix(&s.replace("_", ""), 10))
);

/// hexnum: <hexdigit> (_? <hexdigit>)*
named!(
    pub hexnum<usize>,
    map_res!(map_res!(recognize!(separated_nonempty_list_complete!(
        tag!("_"),
        nom::hex_digit
    )), str::from_utf8), |s: &str| usize::from_str_radix(&s.replace("_", ""), 16))
);

/// nat:    <num> | 0x<hexnum>
named!(
    pub nat<usize>,
    alt_complete!(
        preceded!(tag!("0x"), hexnum) |
        num
    )
);

/// int:    <nat> | +<nat> | -<nat>
named!(
    pub int<isize>,
    alt_complete!(
        map!(preceded!(tag!("+"), nat), |n| n as isize) |
        map_res!(preceded!(tag!("-"), nat), |n| {
            (n as isize).checked_neg().ok_or_else(|| WastError::OutOfRange(n as isize))
        }) |
        map!(nat, |n| n as isize)
    )
);

/// float:  <num>.<num>?(e|E <num>)? | 0x<hexnum>.<hexnum>?(p|P <num>)?
named!(
    pub float<String>,
    map!(pair!(opt!(sign), alt_complete!(
        preceded!(
            tag!("0x"),
            tuple!(
                hexnum,
                opt!(tag!(".")),
                opt!(complete!(hexnum)),
                opt!(complete!(preceded!(tag_no_case!("p"), num)))
            )
        ) |
        tuple!(
            num,
            opt!(tag!(".")),
            opt!(complete!(num)),
            opt!(complete!(preceded!(tag_no_case!("e"), num)))
        )
    )), |(sign, (num, _, frac, exp)): (Option<&str>, (usize, _, Option<usize>, Option<usize>))| {
        format!("{}{}.{}e{}", sign.unwrap_or_default(), num, frac.unwrap_or_default(), exp.unwrap_or_default())
    })
);

named!(pub float32<f32>, map_res!(float, |s: String| f32::from_str(&s)));
named!(pub float64<f64>, map_res!(float, |s: String| f64::from_str(&s)));

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
    Value(usize),
    Name(String),
}

impl Var {
    /// var:    <nat> | <name>
    named!(
        parse<Self>,
        alt!(
        nat => { |v| Var::Value(v) } |
        name => { |v: &str| Var::Name(v.to_owned()) }
    )
    );
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Offset(usize);

impl Offset {
    /// offset: offset=<nat>
    named!(
        parse<Self>,
        map!(preceded!(tag!("offset="), nat), |n| Offset(n))
    );
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Align(usize);

impl Align {
    /// align: align=(1|2|4|8|...)
    named!(
        parse<Self>,
        map!(preceded!(tag!("align="), num), |n| Align(n))
    );
}

/// val_type: i32 | i64 | f32 | f64
named!(
    val_type,
    alt!(tag!("i32") | tag!("i64") | tag!("f32") | tag!("f64"))
);

named!(int_type, alt!(tag!("i32") | tag!("i64")));

named!(float_type, alt!(tag!("f32") | tag!("f64")));

named!(
    mem_size<usize>,
    map_res!(
        map_res!(alt!(tag!("8") | tag!("16") | tag!("32")), str::from_utf8),
        usize::from_str
    )
);

named!(pub const_type, recognize!(pair!(val_type, tag!(".const"))));

#[cfg(test)]
mod tests {
    use pretty_env_logger;

    use nom::Needed;

    use super::*;

    #[test]
    fn parse_number() {
        assert_eq!(num(b"123"), IResult::Done(&[][..], 123));
        assert_eq!(num(b"123_456 abc"), IResult::Done(&b" abc"[..], 123_456));
        assert_eq!(num(b"123_456_abc"), IResult::Done(&b"_abc"[..], 123_456));

        assert_eq!(num(b""), IResult::Error(ErrorKind::Complete));
    }

    #[test]
    fn parse_hex_number() {
        assert_eq!(hexnum(b"123"), IResult::Done(&[][..], 0x123));
        assert_eq!(
            hexnum(b"123_456 abc"),
            IResult::Done(&b" abc"[..], 0x123_456)
        );
        assert_eq!(
            hexnum(b"123_456_abc"),
            IResult::Done(&b""[..], 0x123_456_abc)
        );

        assert_eq!(hexnum(b""), IResult::Error(ErrorKind::Complete));
    }

    #[test]
    fn parse_nat() {
        assert_eq!(nat(b"123"), IResult::Done(&[][..], 123));
        assert_eq!(nat(b"123_456 abc"), IResult::Done(&b" abc"[..], 123_456));
        assert_eq!(
            nat(b"0x123_456_abc"),
            IResult::Done(&b""[..], 0x123_456_abc)
        );

        assert_eq!(nat(b""), IResult::Error(ErrorKind::Alt));
    }

    #[test]
    fn parse_int() {
        assert_eq!(int(b"+123"), IResult::Done(&[][..], 123));
        assert_eq!(int(b"-123_456 abc"), IResult::Done(&b" abc"[..], -123_456));
        assert_eq!(
            int(b"0x1234_5678_90ab_cdef").unwrap(),
            (&b""[..], 0x1234_5678_90ab_cdef)
        );

        assert_eq!(int(b""), IResult::Error(ErrorKind::Alt));
    }

    #[test]
    fn parse_float() {
        assert_eq!(float64(b"123.456e2"), IResult::Done(&[][..], 123.456e2));
        assert_eq!(float64(b"123.e2"), IResult::Done(&[][..], 123.0e2));
        assert_eq!(float64(b"123.456"), IResult::Done(&[][..], 123.456));
        assert_eq!(float64(b"123."), IResult::Done(&[][..], 123.0));

        assert_eq!(float64(b"+123.456e2"), IResult::Done(&[][..], 123.456e2));
        assert_eq!(float64(b"+123.e2"), IResult::Done(&[][..], 123.0e2));
        assert_eq!(float64(b"+123.456"), IResult::Done(&[][..], 123.456));
        assert_eq!(float64(b"+123."), IResult::Done(&[][..], 123.0));

        assert_eq!(float64(b"-123.456e2"), IResult::Done(&[][..], -123.456e2));
        assert_eq!(float64(b"-123.e2"), IResult::Done(&[][..], -123.0e2));
        assert_eq!(float64(b"-123.456"), IResult::Done(&[][..], -123.456));
        assert_eq!(float64(b"-123."), IResult::Done(&[][..], -123.0));

        assert_eq!(float64(b"0x123.456p2"), IResult::Done(&[][..], 29111.1));
        assert_eq!(float64(b"0x123.p2"), IResult::Done(&[][..], 29100.0));
        assert_eq!(float64(b"0x123.456"), IResult::Done(&[][..], 291.111));
        assert_eq!(float64(b"0x123."), IResult::Done(&[][..], 291.0));

        assert_eq!(float64(b"+0x123.456p2"), IResult::Done(&[][..], 29111.1));
        assert_eq!(float64(b"+0x123.p2"), IResult::Done(&[][..], 29100.0));
        assert_eq!(float64(b"+0x123.456"), IResult::Done(&[][..], 291.111));
        assert_eq!(float64(b"+0x123."), IResult::Done(&[][..], 291.0));

        assert_eq!(float64(b"-0x123.456p2"), IResult::Done(&[][..], -29111.1));
        assert_eq!(float64(b"-0x123.p2"), IResult::Done(&[][..], -29100.0));
        assert_eq!(float64(b"-0x123.456"), IResult::Done(&[][..], -291.111));
        assert_eq!(float64(b"-0x123."), IResult::Done(&[][..], -291.0));

        assert_eq!(float64(b"0x1p127"), IResult::Done(&[][..], 1.0e127));
        assert_eq!(float64(b""), IResult::Error(ErrorKind::Alt));
    }

    #[test]
    fn parse_name() {
        assert_eq!(name(b"$a"), IResult::Done(&b""[..], "a"));
        assert_eq!(name(b"$foo bar"), IResult::Done(&b" bar"[..], "foo"));

        assert_eq!(name(b""), IResult::Incomplete(Needed::Size(1)));
        assert_eq!(name(b"$"), IResult::Incomplete(Needed::Size(2)));
        assert_eq!(name(b"+a"), IResult::Error(ErrorKind::Tag));
    }

    #[test]
    fn parse_string() {
        assert_eq!(
            string(b"\"hello world\""),
            IResult::Done(&b""[..], "hello world".to_owned())
        );
        assert_eq!(
            string(b"\"hello \\r\\n\\t\\\\\\'\\\"world\""),
            IResult::Done(&b""[..], "hello \r\n\t\\'\"world".to_owned())
        );

        assert_eq!(string(b""), IResult::Incomplete(Needed::Size(1)));
    }

    #[test]
    fn parse_offset() {
        assert_eq!(
            Offset::parse(b"offset=1234"),
            IResult::Done(&b""[..], Offset(1234))
        );
        assert_eq!(
            Offset::parse(b"offset=0xABCD"),
            IResult::Done(&b""[..], Offset(0xABCD))
        );
    }

    #[test]
    fn parse_align() {
        assert_eq!(Align::parse(b"align=8"), IResult::Done(&b""[..], Align(8)));
    }
}
