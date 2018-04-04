use super::{id, nat32};
use ast::Var;

named!(pub var_list<Vec<Var>>, many0!(first!(var)));

/// var:    <nat> | <id>
named!(
    pub var<Var>,
    parsing!(Var,
        alt_complete!(
            id => { |v: &str| Var::Id(v.to_owned()) } |
            nat32 => { |v| Var::Index(v) }
        )
    )
);

named!(pub opt_bind_var<Option<Var>>, opt!(complete!(first!(bind_var))));

named!(bind_var<Var>, map!(id, |s| Var::Id(s.to_owned())));

#[cfg(test)]
mod tests {
    use std::str;

    use nom::IResult::Done;

    use super::*;

    #[test]
    fn parse_var() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"0", Done(&b""[..], super::Var::Index(0))),
            (b"$done", Done(&b""[..], super::Var::Id("done".to_owned()))),
        ];

        for (code, ref result) in tests {
            assert_eq!(var(code), *result, "parse align: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }
}
