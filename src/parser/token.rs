#![allow(non_snake_case)]

named!(pub LPAR, first!(tag!("(")));
named!(pub RPAR, first!(tag!(")")));

named!(pub I32, first!(tag!("i32")));
named!(pub I64, first!(tag!("i64")));
named!(pub F32, first!(tag!("f32")));
named!(pub F64, first!(tag!("f64")));

named!(pub ANYFUNC, first!(tag!("anyfunc")));
named!(pub BLOCK, first!(tag!("block")));
named!(pub BR_IF, first!(tag!("br_if")));
named!(pub BR_TABLE, first!(tag!("br_table")));
named!(pub BR, first!(tag!("br")));
named!(pub CALL_INDIRECT, first!(tag!("call_indirect")));
named!(pub CALL, first!(tag!("call")));
named!(pub CURRENT_MEMORY, first!(tag!("current_memory")));
named!(pub DATA, first!(tag!("data")));
named!(pub DROP, first!(tag!("drop")));
named!(pub ELEM, first!(tag!("elem")));
named!(pub ELSE, first!(tag!("else")));
named!(pub END, first!(tag!("end")));
named!(pub EXPORT, first!(tag!("export")));
named!(pub FUNC, first!(tag!("func")));
named!(pub GET_GLOBAL, first!(tag!("get_global")));
named!(pub GET_LOCAL, first!(tag!("get_local")));
named!(pub GLOBAL, first!(tag!("global")));
named!(pub GROW_MEMORY, first!(tag!("grow_memory")));
named!(pub IF, first!(tag!("if")));
named!(pub IMPORT, first!(tag!("import")));
named!(pub LOOP, first!(tag!("loop")));
named!(pub MEMORY, first!(tag!("memory")));
named!(pub MODULE, first!(tag!("module")));
named!(pub MUT, first!(tag!("mut")));
named!(pub NOP, first!(tag!("nop")));
named!(pub OFFSET, first!(tag!("offset")));
named!(pub PARAM, first!(tag!("param")));
named!(pub RESULT, first!(tag!("result")));
named!(pub RETURN, first!(tag!("return")));
named!(pub SELECT, first!(tag!("select")));
named!(pub SET_GLOBAL, first!(tag!("set_global")));
named!(pub SET_LOCAL, first!(tag!("set_local")));
named!(pub START, first!(tag!("start")));
named!(pub TABLE, first!(tag!("table")));
named!(pub TEE_LOCAL, first!(tag!("tee_local")));
named!(pub THEN, first!(tag!("then")));
named!(pub TYPE, first!(tag!("type")));
named!(pub UNREACHABLE, first!(tag!("unreachable")));
