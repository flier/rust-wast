use super::{const_list, name, opt_bind_var, string, string_list, ASSERT_EXHAUSTION, ASSERT_INVALID, ASSERT_MALFORMED,
            ASSERT_RETURN, ASSERT_RETURN_ARITHMETIC_NAN, ASSERT_RETURN_CANONICAL_NAN, ASSERT_TRAP, ASSERT_UNLINKABLE,
            BIN, GET, INVOKE, LPAR, MODULE, QUOTE, RPAR};
use ast::{Action, Assertion, ScriptModule};

named!(
    script_module<ScriptModule>,
    delimited!(
        LPAR,
        preceded!(
            MODULE,
            alt!(
                tuple!(opt_bind_var, BIN, string_list) => { |(id, _, strs): (_, _, Vec<String>)|
                    ScriptModule::Encoded(id, strs.into_iter().fold(vec![], |mut v, s| {
                        v.append(&mut s.into_bytes());
                        v
                    }))
                } |
                tuple!(opt_bind_var, QUOTE, string_list) => { |(id, _, strs): (_, _, Vec<String>)|
                    ScriptModule::Quoted(id, strs.into_iter().fold(String::new(), |mut o, s| {
                        o.push_str(&s);
                        o
                    }))
                }
            )
        ),
        RPAR
    )
);

named!(
    action<Action>,
    parsing!(
        Action,
        delimited!(
            LPAR,
            alt!(
                preceded!(INVOKE, tuple!(opt_bind_var, first!(name), const_list)) => {
                    |(id, name, constants)| Action::Invoke(id, name, constants)
                } |
                preceded!(GET, tuple!(opt_bind_var, first!(name))) => {
                    |(id, name)| Action::Get(id, name)
                }
            ),
            RPAR
        )
    )
);

named!(
    pub assertion<Assertion>,
    parsing!(Assertion,
        delimited!(
            LPAR,
            alt!(
                preceded!(ASSERT_MALFORMED, pair!(script_module, first!(string))) => { |(module, reason)|
                    Assertion::Malformed(module, reason)
                } |
                preceded!(ASSERT_INVALID, pair!(script_module, first!(string))) => { |(module, reason)|
                    Assertion::Invalid(module, reason)
                } |
                preceded!(ASSERT_UNLINKABLE, pair!(script_module, first!(string))) => { |(module, reason)|
                    Assertion::Unlinkable(module, reason)
                } |
                preceded!(ASSERT_RETURN, tuple!(action, const_list)) => { | (action, constants) |
                    Assertion::Return(action, constants)
                } |
                preceded!(ASSERT_RETURN_CANONICAL_NAN, action) => { | action |
                    Assertion::ReturnCanonicalNan(action)
                } |
                preceded!(ASSERT_RETURN_ARITHMETIC_NAN, action) => { | action |
                    Assertion::ReturnArithmeticNan(action)
                } |
                preceded!(ASSERT_TRAP, pair!(action, first!(string))) => { | (action, reason) |
                    Assertion::Trap(action, reason)
                } |
                preceded!(ASSERT_EXHAUSTION, pair!(action, first!(string))) => { | (action, reason) |
                    Assertion::Exhaustion(action, reason)
                }
            ),
            RPAR
        )
    )
);

#[cfg(test)]
mod tests {
    use std::str;

    use nom::IResult;

    use super::*;
    use ast::{Constant, Var};

    #[test]
    fn parse_action() {
        let tests: Vec<(&[u8], _)> = vec![
            (
                b"(get \"e\")",
                IResult::Done(&[][..], Action::Get(None, "e".to_owned())),
            ),
            (
                b"(get $Global \"e\")",
                IResult::Done(&[][..], Action::Get(Some(Var::id("Global")), "e".to_owned())),
            ),
            (
                b"(invoke \"e\" (i32.const 42))",
                IResult::Done(&[][..], Action::Invoke(None, "e".to_owned(), vec![Constant::I32(42)])),
            ),
            (
                b"(invoke $Func \"e\" (i32.const 42))",
                IResult::Done(
                    &[][..],
                    Action::Invoke(Some(Var::id("Func")), "e".to_owned(), vec![Constant::I32(42)]),
                ),
            ),
        ];

        for (code, result) in tests {
            let res = action(code);

            trace_parse_error!(code, res);

            assert_eq!(res, result, "parse assertion: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_assertion() {
        let tests: Vec<(&[u8], _)> = vec![
            (
                br#"(assert_malformed
  (module quote
    "(memory 1)"
    "(func (drop (i32.load offset=4294967296 (i32.const 0))))"
  )
  "i32 constant"
)"#,
                IResult::Done(
                    &[][..],
                    Assertion::Malformed(
                        ScriptModule::Quoted(
                            None,
                            "(memory 1)(func (drop (i32.load offset=4294967296 (i32.const 0))))".to_owned(),
                        ),
                        "i32 constant".to_owned(),
                    ),
                ),
            ),
            (
                br#"(assert_malformed
  (module binary
    "\00asm" "\01\00\00\00"
    "\00"
  )
  "unexpected end"
)"#,
                IResult::Done(
                    &[][..],
                    Assertion::Malformed(
                        ScriptModule::Encoded(None, b"\x00asm\x01\x00\x00\x00\x00".to_vec()),
                        "unexpected end".to_owned(),
                    ),
                ),
            ),
            (
                b"(assert_return_canonical_nan (invoke \"f64.promote_f32\" (f32.const 3.14)))",
                IResult::Done(
                    &[][..],
                    Assertion::ReturnCanonicalNan(Action::Invoke(
                        None,
                        "f64.promote_f32".to_owned(),
                        vec![Constant::F32(3.14)],
                    )),
                ),
            ),
            (
                b"(assert_return (invoke \"e\" (i32.const 42)) (i32.const 43))",
                IResult::Done(
                    &[][..],
                    Assertion::Return(
                        Action::Invoke(None, "e".to_owned(), vec![Constant::I32(42)]),
                        vec![Constant::I32(43)],
                    ),
                ),
            ),
        ];

        for (code, result) in tests {
            let res = assertion(code);

            trace_parse_error!(code, res);

            assert_eq!(res, result, "parse assertion: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }
}
