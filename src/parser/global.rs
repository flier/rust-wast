use parity_wasm::elements::{GlobalType, InitExpr, ValueType};

use super::{init_expr, inline_export, inline_import, opt_bind_var, value_type, GLOBAL, LPAR, MUT, RPAR};
use ast::{Global, Var};

named!(
    pub global<(Option<Var>, Global)>,
    parsing!(
        Global,
        delimited!(
            LPAR,
            preceded!(
                GLOBAL,
                pair!(opt_bind_var, first!(global_fields))
            ),
            RPAR
        )
    )
);

named!(
    global_fields<Global>,
    alt!(
        pair!(first!(global_type), first!(init_expr)) => {
            |(global_type, init_expr)| Global { global_type, init_expr: init_expr }
        } |
        pair!(first!(inline_import), first!(global_type)) => {
            |((module_name, item_name), global_type)| Global { global_type, init_expr: InitExpr::new(vec![]) }
        } |
        pair!(first!(inline_export), first!(global_fields)) => {
            |(export_name, global)| global
        }
    )
);

named!(
    global_type<GlobalType>,
    alt!(
        value_type => { |ty| GlobalType::new(ty, false) } |
        mut_value_type => { |ty| GlobalType::new(ty, true) }
    )
);

named!(
    mut_value_type<ValueType>,
    delimited!(LPAR, preceded!(MUT, first!(value_type)), RPAR)
);

#[cfg(test)]
mod tests {
    use std::mem;
    use std::str;

    use nom::IResult::Done;
    use parity_wasm::elements::{Opcode::*, ValueType::*};
    use pretty_env_logger;

    use super::*;

    #[test]
    fn parse_global() {
        let _ = pretty_env_logger::try_init();

        let tests: Vec<(&[u8], _)> = vec![
            (
                b"(global $a i32 (i32.const -2))",
                (
                    Some(Var::Id("a".to_owned())),
                    Global::new(GlobalType::new(I32, false), InitExpr::new(vec![I32Const(-2)])),
                ),
            ),
            (
                b"(global (;1;) f32 (f32.const -3))",
                (
                    None,
                    Global::new(
                        GlobalType::new(F32, false),
                        InitExpr::new(vec![F32Const(unsafe { mem::transmute(-3.0f32) })]),
                    ),
                ),
            ),
            (
                b"(global (;2;) f64 (f64.const -4))",
                (
                    None,
                    Global::new(
                        GlobalType::new(F64, false),
                        InitExpr::new(vec![F64Const(unsafe { mem::transmute(-4.0f64) })]),
                    ),
                ),
            ),
            (
                b"(global $b i64 (i64.const -5))",
                (
                    Some(Var::Id("b".to_owned())),
                    Global::new(GlobalType::new(I64, false), InitExpr::new(vec![I64Const(-5)])),
                ),
            ),
            (
                b"(global $x (mut i32) (i32.const -12))",
                (
                    Some(Var::Id("x".to_owned())),
                    Global::new(GlobalType::new(I32, true), InitExpr::new(vec![I32Const(-12)])),
                ),
            ),
            (
                b"(global (;5;) (mut f32) (f32.const -13))",
                (
                    None,
                    Global::new(
                        GlobalType::new(F32, true),
                        InitExpr::new(vec![F32Const(unsafe { mem::transmute(-13.0f32) })]),
                    ),
                ),
            ),
            (
                b"(global (;6;) (mut f64) (f64.const -14))",
                (
                    None,
                    Global::new(
                        GlobalType::new(F64, true),
                        InitExpr::new(vec![F64Const(unsafe { mem::transmute(-14.0f64) })]),
                    ),
                ),
            ),
            (
                b"(global $y (mut i64) (i64.const -15))",
                (
                    Some(Var::Id("y".to_owned())),
                    Global::new(GlobalType::new(I64, true), InitExpr::new(vec![I64Const(-15)])),
                ),
            ),
            (
                br#"(global (import "spectest" "global_i32") i32)"#,
                (None, Global::new(GlobalType::new(I32, false), InitExpr::new(vec![]))),
            ),
            (
                br#"(global (export "global-i32") i32 (i32.const 55))"#,
                (
                    None,
                    Global::new(GlobalType::new(I32, false), InitExpr::new(vec![I32Const(55)])),
                ),
            ),
        ];

        for (code, result) in tests {
            assert_eq!(global(code), Done(&[][..], result), "parse global: {}", unsafe {
                str::from_utf8_unchecked(code)
            },);
        }
    }
}
