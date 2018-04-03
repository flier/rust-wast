use super::string;

named!(
    pub inline_export<String>,
    delimited!(tag!("("), preceded!(first!(tag!("export")), first!(string)), tag!(")"))
);

#[cfg(test)]
mod tests {
    use std::str;

    use nom::IResult::Done;

    use super::*;

    #[test]
    fn parse_inline_export() {
        let tests: Vec<(&[u8], _)> = vec![(b"(export \"a\")", "a")];

        for (code, result) in tests {
            assert_eq!(
                inline_export(code),
                Done(&[][..], result.to_owned()),
                "parse `{}` failed",
                unsafe { str::from_utf8_unchecked(code) }
            );
        }
    }
}
