named!(
    pub comment,
    parsing!(
        Comment,
        alt!(
            preceded!(tag!("(;"), take_until_and_consume!(";)"))
                | preceded!(tag!(";;"), re_bytes_find_static!(r"^(?-u).*?(\r\n|\n|$)"))
        )
    )
);

#[cfg(test)]
mod tests {
    use std::str;

    use nom::IResult::Done;

    use super::*;

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
}
