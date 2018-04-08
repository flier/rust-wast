use parity_wasm::elements::FunctionType;

use super::{func_type, typeidx, LPAR, RPAR, TYPE};
use ast::Var;

named!(
    pub typeuse<(Option<Var>, Option<FunctionType>)>,
    parsing!(TypeUse,
        pair!(
            opt!(complete!(use_type)),
            func_type
        )
    )
);

named!(use_type<Var>, delimited!(LPAR, preceded!(TYPE, typeidx), RPAR));

#[cfg(test)]
mod tests {
    use std::str;

    use nom::IResult::Done;
    use parity_wasm::elements::ValueType::*;
    use pretty_env_logger;

    use super::*;

    #[test]
    fn parse_typedef() {
        let _ = pretty_env_logger::try_init();

        let tests: Vec<(&[u8], _)> = vec![
            (b"(type $sig)", (Some(Var::Id("sig".to_owned())), None)),
            (b"(type 0)", (Some(Var::Index(0)), None)),
            (b"(param i64)", (None, Some(FunctionType::new(vec![I64], None)))),
            (b"(result i64)", (None, Some(FunctionType::new(vec![], Some(I64))))),
            (
                b"(param i64) (result i64)",
                (None, Some(FunctionType::new(vec![I64], Some(I64)))),
            ),
            (
                b"(type $over-i64) (param i64) (result i64)",
                (
                    Some(Var::Id("over-i64".to_owned())),
                    Some(FunctionType::new(vec![I64], Some(I64))),
                ),
            ),
        ];

        for (code, ref func_type) in tests {
            assert_eq!(
                typeuse(code),
                Done(&[][..], func_type.clone()),
                "parse typeuse: {}",
                unsafe { str::from_utf8_unchecked(code) },
            );
        }
    }
}
