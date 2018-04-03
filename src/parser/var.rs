use super::{name, nat32};
use ast::Var;

named!(pub var_list<Vec<Var>>, many0!(first!(var)));

/// var:    <nat> | <name>
named!(
    pub var<Var>,
    parsing!(Var,
        alt_complete!(
            name => { |v: &str| Var::Name(v.to_owned()) } |
            nat32 => { |v| Var::Index(v) }
        )
    )
);

named!(pub bind_var<Var>, map!(name, |s| Var::Name(s.to_owned())));

#[cfg(test)]
mod tests {
    use std::str;

    use nom::IResult::Done;

    use super::*;

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
}
