use itertools;
use parity_wasm::elements::{BlockType, ValueType};

use super::{align, binary, compare, convert, expr, func_type, mem_size, sign, test, unary, value_type, var, float32,
            float64, int32, int64, nat32};
use ast::{Constant, Instr, Load, Store, Var};

named!(
    pub instr_list<Vec<Instr>>,
    map!(ws!(many0!(first!(instr))), |instrs| itertools::flatten(instrs).collect())
);

named!(
    pub instr<Vec<Instr>>,
    parsing!(
        Instr,
        ws!(alt!(
            plain_instr => { |instr| vec![instr] } |
            call_instr => { |instr| vec![instr] } |
            block_instr => { |instr| vec![instr] } |
            expr
        ))
    )
);

named!(
    pub plain_instr<Instr>,
    parsing!(
        PlainInstr,
        alt_complete!(
            tag!("unreachable") => { |_| Instr::Unreachable } |
            tag!("nop") => { |_| Instr::Nop } |
            ws!(preceded!(tag!("br"), var)) => { |var| Instr::Br(var) } |
            ws!(preceded!(tag!("br_if"), var)) => { |var| Instr::BrIf(var) } |
            ws!(preceded!(tag!("br_table"), pair!(var, many0!(var)))) => {
                |(default, targets)| Instr::BrTable(targets, default)
            } |
            tag!("return") => { |_| Instr::Return } |
            ws!(preceded!(tag!("call"), var)) => { |var| Instr::Call(var) } |
            tag!("drop") => { |_| Instr::Drop } |
            tag!("select") => { |_| Instr::Select } |
            ws!(preceded!(tag!("get_local"), var)) => { |var| Instr::GetLocal(var) } |
            ws!(preceded!(tag!("set_local"), var)) => { |var| Instr::SetLocal(var) } |
            ws!(preceded!(tag!("tee_local"), var)) => { |var| Instr::TeeLocal(var) } |
            ws!(preceded!(tag!("get_global"), var)) => { |var| Instr::GetGlobal(var) } |
            ws!(preceded!(tag!("set_global"), var)) => { |var| Instr::SetGlobal(var) } |
            load => { |load| Instr::Load(load) } |
            store => { |store| Instr::Store(store) } |
            tag!("current_memory") => { |_| Instr::CurrentMemory } |
            tag!("grow_memory") => { |_| Instr::GrowMemory } |
            constant => { |constant| Instr::Const(constant) } |
            test => { |test| Instr::Test(test) } |
            compare => { |compare| Instr::Compare(compare) } |
            unary => { |unary| Instr::Unary(unary) } |
            binary => { |binary| Instr::Binary(binary) } |
            convert => { |convert| Instr::Convert(convert) }
        )
    )
);

named!(
    pub call_instr<Instr>,
    preceded!(tag!("call_indirect"), first!(call_instr_type))
);

named!(
    call_instr_type<Instr>,
    alt!(
        var => { |var| Instr::CallIndirect(Some(var),  None) } |
        pair!(opt!(first!(type_use)), first!(func_type)) => {
            |(type_use, func_type)| Instr::CallIndirect(type_use, func_type)
        }
    )
);

named!(
    pub type_use<Var>,
    ws!(delimited!(tag!("("), preceded!(tag!("type"), var), tag!(")")))
);

// named!(call_instr_instr<Instr>, tag!(""));

named!(
    block_instr<Instr>,
    parsing!(
        BlockInstr,
        ws!(alt_complete!(
            tuple!(tag!("block"), opt!(first!(var)), first!(block), tag!("end"), opt!(first!(var))) => {
                |(_, _, (block_type, instrs), _, _)| Instr::Block(block_type, instrs)
            } |
            tuple!(tag!("loop"), opt!(first!(var)), first!(block), tag!("end"), opt!(first!(var))) => {
                |(_, _, (block_type, instrs), _, _)| Instr::Loop(block_type, instrs)
            } |
            tuple!(tag!("if"), opt!(first!(var)), first!(block), tag!("end"), opt!(first!(var))) => {
                |(_, _, (block_type, then), _, _)| Instr::If(block_type, then, vec![])
            } |
            tuple!(
                tag!("if"), opt!(first!(var)), first!(block),
                tag!("else"), opt!(first!(var)), instr_list,
                tag!("end"), opt!(first!(var))
            ) => {
                |(_, _, (block_type, then), _, _, else_, _, _)| Instr::If(block_type, then, else_)
            }
        ))
    )
);

named!(
    pub block<(BlockType, Vec<Instr>)>,
    parsing!(
        Block,
        map!(
            pair!(opt!(first!(block_type)), first!(instr_list)),
            |(block_type, instrs)| (block_type.map_or(BlockType::NoResult, BlockType::Value), instrs)
        )
    )
);

named!(
    pub block_type<ValueType>,
    ws!(delimited!(tag!("("), preceded!(tag!("result"), value_type), tag!(")")))
);

/// <val_type>.const <value>
named!(
    constant<Constant>,
    ws!(switch!(recognize!(pair!(value_type, tag!(".const"))),
            b"i32.const" => map!(int32, |n| Constant::I32(n)) |
            b"i64.const" => map!(int64, |n| Constant::I64(n)) |
            b"f32.const" => map!(float32, |v| Constant::F32(v)) |
            b"f64.const" => map!(float64, |v| Constant::F64(v))
        ))
);

/// <val_type>.load((8|16|32)_<sign>)? <offset>? <align>?
#[cfg_attr(rustfmt, rustfmt_skip)]
named!(
    load<Load>,
    ws!(do_parse!(
        op: recognize!(tuple!(value_type, tag!(".load"), opt!(complete!(tuple!(mem_size, tag!("_"), sign))))) >>
        offset: map!(opt!(complete!(offset)), |n| n.unwrap_or_default()) >>
        align: verify!(
            map!(opt!(complete!(align)), |n| n.unwrap_or_default()),
            |n: u32| n == 0 || n.is_power_of_two()
        ) >>
        opcode: switch!(value!(op),
            b"i32.load" => value!(Load::I32(align, offset)) |
            b"i64.load" => value!(Load::I64(align, offset)) |
            b"f32.load" => value!(Load::F32(align, offset)) |
            b"f64.load" => value!(Load::F64(align, offset)) |
            b"i32.load8_s" => value!(Load::I8AsI32(align, offset)) |
            b"i32.load8_u" => value!(Load::U8AsI32(align, offset)) |
            b"i32.load16_s" => value!(Load::I16AsI32(align, offset)) |
            b"i32.load16_u" => value!(Load::U16AsI32(align, offset)) |
            b"i64.load8_s" => value!(Load::I8AsI64(align, offset)) |
            b"i64.load8_u" => value!(Load::U8AsI64(align, offset)) |
            b"i64.load16_s" => value!(Load::I16AsI64(align, offset)) |
            b"i64.load16_u" => value!(Load::U16AsI64(align, offset)) |
            b"i64.load32_s" => value!(Load::I32AsI64(align, offset)) |
            b"i64.load32_u" => value!(Load::U32AsI64(align, offset))
        ) >>
        ( opcode )
    ))
);

/// <val_type>.store(8|16|32)? <offset>? <align>?
#[cfg_attr(rustfmt, rustfmt_skip)]
named!(
    pub store<Store>,
    ws!(do_parse!(
        op: recognize!(tuple!(
                value_type,
                tag!(".store"),
                opt!(complete!(mem_size))
            )) >>
        offset: map!(opt!(complete!(offset)), |n| n.unwrap_or_default()) >>
        align: verify!(
            map!(opt!(complete!(align)), |n| n.unwrap_or_default()),
            |n: u32| n == 0 || n.is_power_of_two()
        ) >>
        opcode: switch!(value!(op),
            b"i32.store" => value!(Store::I32(align, offset)) |
            b"i64.store" => value!(Store::I64(align, offset)) |
            b"f32.store" => value!(Store::F32(align, offset)) |
            b"f64.store" => value!(Store::F64(align, offset)) |
            b"i32.store8" => value!(Store::I32AsI8(align, offset)) |
            b"i32.store16" => value!(Store::I32AsI16(align, offset)) |
            b"i64.store8" => value!(Store::I64AsI8(align, offset)) |
            b"i64.store16" => value!(Store::I64AsI16(align, offset)) |
            b"i64.store32" => value!(Store::I64AsI32(align, offset))
        ) >>
        ( opcode )
    ))
);

/// offset: offset=<nat>
named!(offset<u32>, map!(preceded!(tag!("offset="), nat32), |n| n as u32));

#[cfg(test)]
mod tests {
    use std::str;

    use nom::IResult::*;
    use parity_wasm::elements::{FunctionType, ValueType::*};

    use super::*;
    use ast::Instr::*;
    use ast::Var;

    #[test]
    fn parse_plain_instr() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"unreachable", Done(&[][..], Unreachable)),
            (b"nop", Done(&[][..], Nop)),
            (b"br 123", Done(&[][..], Br(Var::Index(123)))),
            (b"br $end", Done(&[][..], Br(Var::Id("end".to_owned())))),
            (b"br_if 123", Done(&[][..], BrIf(Var::Index(123)))),
            (b"br_if $end", Done(&[][..], BrIf(Var::Id("end".to_owned())))),
            (b"br_table 0", Done(&[][..], BrTable(vec![], Var::Index(0)))),
            (
                b"br_table 0 1 2 3",
                Done(
                    &[][..],
                    BrTable(vec![Var::Index(1), Var::Index(2), Var::Index(3)], Var::Index(0)),
                ),
            ),
            (b"return", Done(&[][..], Return)),
            (b"call 123", Done(&[][..], Call(Var::Index(123)))),
            (b"call $name", Done(&[][..], Call(Var::Id("name".to_owned())))),
            (b"get_local 123", Done(&[][..], GetLocal(Var::Index(123)))),
            (b"get_local $name", Done(&[][..], GetLocal(Var::Id("name".to_owned())))),
            (b"set_local 123", Done(&[][..], SetLocal(Var::Index(123)))),
            (b"set_local $name", Done(&[][..], SetLocal(Var::Id("name".to_owned())))),
            (b"tee_local 123", Done(&[][..], TeeLocal(Var::Index(123)))),
            (b"tee_local $name", Done(&[][..], TeeLocal(Var::Id("name".to_owned())))),
            (b"get_global 123", Done(&[][..], GetGlobal(Var::Index(123)))),
            (
                b"get_global $name",
                Done(&[][..], GetGlobal(Var::Id("name".to_owned()))),
            ),
            (b"set_global 123", Done(&[][..], SetGlobal(Var::Index(123)))),
            (
                b"set_global $name",
                Done(&[][..], SetGlobal(Var::Id("name".to_owned()))),
            ),
            (b"current_memory", Done(&[][..], CurrentMemory)),
            (b"grow_memory", Done(&[][..], GrowMemory)),
        ];

        for &(code, ref result) in tests.iter() {
            assert_eq!(plain_instr(code), *result, "parse instr: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_call_instr() {
        let tests: Vec<(&[u8], _)> = vec![
            (
                b"call_indirect 123",
                Done(&[][..], CallIndirect(Some(Var::Index(123)), None)),
            ),
            (
                b"call_indirect $hello",
                Done(&[][..], CallIndirect(Some(Var::Id("hello".to_owned())), None)),
            ),
            (
                b"call_indirect (type $hello)",
                Done(&[][..], CallIndirect(Some(Var::Id("hello".to_owned())), None)),
            ),
            (
                b"call_indirect (param i32)",
                Done(&[][..], CallIndirect(None, Some(FunctionType::new(vec![I32], None)))),
            ),
            (
                b"call_indirect (param i64)",
                Done(&[][..], CallIndirect(None, Some(FunctionType::new(vec![I64], None)))),
            ),
            (
                b"call_indirect (param i32) (param i64) (result f32)",
                Done(
                    &[][..],
                    CallIndirect(None, Some(FunctionType::new(vec![I32, I64], Some(F32)))),
                ),
            ),
        ];

        for &(code, ref result) in tests.iter() {
            assert_eq!(call_instr(code), *result, "parse instr: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }

    #[test]
    fn parse_block_instr() {
        let tests: Vec<(&[u8], _)> = vec![
            (b"block\nend", Done(&[][..], Block(BlockType::NoResult, vec![]))),
            (
                b"block (result i32)\nend",
                Done(&[][..], Block(BlockType::Value(I32), vec![])),
            ),
            (
                b"block $done\nend $done",
                Done(&[][..], Block(BlockType::NoResult, vec![])),
            ),
            (
                b"block $done (result i32)\nend $done",
                Done(&[][..], Block(BlockType::Value(I32), vec![])),
            ),
            (
                b"block $done (result i32) nop\nend $done",
                Done(&[][..], Block(BlockType::Value(I32), vec![Nop])),
            ),
            (
                b"loop $done (result i32) unreachable\nend $done",
                Done(&[][..], Loop(BlockType::Value(I32), vec![Unreachable])),
            ),
            (
                b"if $done (result i32) nop unreachable\nend $done",
                Done(&[][..], If(BlockType::Value(I32), vec![Nop, Unreachable], vec![])),
            ),
            (
                b"if $done (result i32) nop\nelse unreachable\nend $done",
                Done(&[][..], If(BlockType::Value(I32), vec![Nop], vec![Unreachable])),
            ),
        ];

        for &(code, ref result) in tests.iter() {
            assert_eq!(block_instr(code), *result, "parse instr: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }
}
