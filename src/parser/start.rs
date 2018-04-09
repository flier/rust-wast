use super::{funcidx, LPAR, RPAR, START};
use ast::Var;

named!(pub start<Var>, delimited!(LPAR, preceded!(START, funcidx), RPAR));

#[cfg(test)]
mod tests {
    use std::str;

    use nom::IResult;

    use super::*;

    #[test]
    fn parse_start() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"(start $main)", Var::Id("main".to_owned())),
            (b"(start 2)", Var::Index(2)),
        ];

        for (code, ref value) in tests {
            let res = start(code);

            trace_parse_error!(code, res);

            assert_eq!(res, IResult::Done(&[][..], value.clone()), "parse start: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }
}
