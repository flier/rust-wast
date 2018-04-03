use super::string;

named!(
    pub inline_import<(String, String)>,
    delimited!(
        tag!("("),
        preceded!(first!(tag!("import")), pair!(first!(string), first!(string))),
        tag!(")")
    )
);

#[cfg(test)]
mod tests {
    use std::str;

    use nom::IResult::Done;

    use super::*;

    #[test]
    fn parse_inline_import() {
        let tests: Vec<(&[u8], _)> = vec![(b"(import \"m\" \"a\")", ("m", "a"))];

        for (code, (module, field)) in tests {
            assert_eq!(
                inline_import(code),
                Done(&[][..], (module.to_owned(), field.to_owned())),
                "parse inline_import: {}",
                unsafe { str::from_utf8_unchecked(code) }
            );
        }
    }
}
