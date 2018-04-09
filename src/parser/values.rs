use std::{char, str, f32, f64, i32, i64};
use std::iter::{Extend, FromIterator};
use std::str::FromStr;

use nom;

use errors::WastError::OutOfRange;

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
                n.checked_neg().ok_or_else(||OutOfRange(n))
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
        alt!(
            map!(
                pair!(opt!(sign), alt_complete!(
                    tag!("inf") => { |_| "inf" } |
                    tuple!(tag!("nan"), opt!(complete!(pair!(tag!(":0x"), hexnum)))) => { |_| "NaN" }
                )),
                |(sign, num)| format!("{}{}", sign.unwrap_or_default(), num)
            ) |
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
    )
);

named!(pub float32<f32>, map_res!(float, |s: String| f32::from_str(&s)));
named!(pub float64<f64>, map_res!(float, |s: String| f64::from_str(&s)));

named!(
    sign<&str>,
    map_res!(alt_complete!(tag!("+") | tag!("-")), str::from_utf8)
);

/// id:   $(<letter> | <digit> | _ | . | + | - | * | / | \ | ^ | ~ | = | < | > | ! | ? | @ | # | $ | % | & | | | : | ' | `)+
named!(
    pub id<&str>,
    parsing!(Id,
        map_res!(
            preceded!(
                tag!("$"),
                take_while1!(|b| nom::is_alphanumeric(b) || SYMBOL.contains(&b))
            ),
            str::from_utf8
        )
    )
);

const SYMBOL: &[u8] = b"_.+-*/\\^~=<>!?@#$%&|:'`";

named!(pub string_list<Vec<String>>, many0!(first!(string)));

/// string: "(<char> | \n | \t | \\ | \' | \" | \<hex><hex> | \u{<hex>+})*"
named!(
    pub string<String>,
    parsing!(Str,
        dbg_dmp!(alt_complete!(
            tag!("\"\"") => { |_| Default::default() } |
            delimited!(
                tag!("\""),
                map!(
                    escaped_transform!(string_char, '\\',
                        alt_complete!(
                            tag!("r") => { |_| vec!['\r'] } |
                            tag!("n") => { |_| vec!['\n'] } |
                            tag!("t") => { |_| vec!['\t'] } |
                            tag!("\\") => { |_| vec!['\\'] } |
                            tag!("'") => { |_| vec!['\''] } |
                            tag!("\"") => { |_| vec!['"'] } |
                            hex_char => { |b| vec![char::from(b)] } |
                            unicode_char => { |c| vec![c] }
                        )
                    ),
                    String::from_iter
                ),
                tag!("\"")
            )
        ))
    )
);

named!(
    pub name<String>, call!(string)
);

named!(
    hex_char<u8>,
    map_res!(
        map_res!(
            verify!(take!(2), |v: &[u8]| v.iter().all(|c| c.is_ascii_hexdigit())),
            str::from_utf8
        ),
        |s| u8::from_str_radix(s, 16)
    )
);

named!(
    unicode_char<char>,
    map!(
        map_res!(delimited!(tag!("u{"), hexnum, tag!("}")), |s: String| {
            u32::from_str_radix(&s, 16)
        }),
        |n| unsafe { char::from_u32_unchecked(n) }
    )
);

fn string_char(input: &[u8]) -> nom::IResult<&[u8], Vec<char>, u32> {
    match input.iter().position(|&b| b == b'\\' || b == b'\"') {
        None => nom::IResult::Done(&input[input.len()..], input),
        Some(index) if index > 0 => nom::IResult::Done(&input[index..], &input[..index]),
        _ => nom::IResult::Error(error_position!(nom::ErrorKind::Tag, input)),
    }.map(|bytes| unsafe { str::from_utf8_unchecked(bytes) })
        .map(|s| s.chars().collect())
}

#[cfg(test)]
mod tests {
    use nom::Err::{NodePosition, Position};
    use nom::ErrorKind::*;
    use nom::IResult::{Done, Error, Incomplete};
    use nom::Needed;

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
            (b"inf", Done(&[][..], f64::INFINITY)),
            (b"-inf", Done(&[][..], -f64::INFINITY)),
            (b"", Error(NodePosition(Custom(Floating as u32), &[][..], vec![Position(Alt, &[][..])]))),
        ];

        for (code, ref result) in tests {
            assert_eq!(float64(code), *result, "parse float: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }

        assert!(float32(b"nan").unwrap().1.is_nan());
        assert!(float32(b"-nan").unwrap().1.is_nan());
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
                    Custom(Id as u32),
                    &b"+a"[..],
                    vec![Position(Tag, &b"+a"[..])],
                )),
            ),
        ];

        for (code, ref result) in tests {
            assert_eq!(id(code), *result, "parse id: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_string() {
        let tests: Vec<(&[u8], _)> = vec![
            (br#""""#, Done(&b""[..], "".to_owned())),
            (br#""hello world""#, Done(&b""[..], "hello world".to_owned())),
            (
                br#""hello \r \n \t \\ \' \" world""#,
                Done(&b""[..], "hello \r \n \t \\ \' \" world".to_owned()),
            ),
            (br#""\61""#, Done(&b""[..], "a".to_owned())),
            (br#""\u{6d4b}\u{8bd5}""#, Done(&b""[..], "测试".to_owned())),
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
}
