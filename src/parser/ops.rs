use std::str::{self, FromStr};
use std::usize;

use parity_wasm::elements::Opcode;

use super::{int_type, nat32};

/// align: align=(1|2|4|8|...)
named!(
    pub align<u32>,
    map!(preceded!(tag!("align="), nat32), |n| n as u32)
);

named!(
    pub mem_size<usize>,
    map_res!(
        map_res!(alt!(tag!("8") | tag!("16") | tag!("32")), str::from_utf8),
        usize::from_str
    )
);

/// sign:  s|u
named!(pub sign, alt!(tag!("s") | tag!("u")));

// <val_type>.<testop>
named!(
    pub test<Opcode>,
    switch!(recognize!(tuple!(int_type, tag!("."), testop)),
        b"i32.eqz" => value!(Opcode::I32Eqz) |
        b"i64.eqz" => value!(Opcode::I64Eqz)
    )
);

named!(testop, tag!("eqz"));

/// <val_type>.<unop>
named!(
    pub unary<Opcode>,
    switch!(recognize!(tuple!(int_type, tag!("."), unop)),
        b"i32.clz" => value!(Opcode::I32Clz) |
        b"i32.ctz" => value!(Opcode::I32Ctz) |
        b"i32.popcnt" => value!(Opcode::I32Popcnt) |

        b"i64.clz" => value!(Opcode::I64Clz) |
        b"i64.ctz" => value!(Opcode::I64Ctz) |
        b"i64.popcnt" => value!(Opcode::I64Popcnt) |

        b"f32.abs" => value!(Opcode::F32Abs) |
		b"f32.neg" => value!(Opcode::F32Neg) |
        b"f32.sqrt" => value!(Opcode::F32Sqrt ) |
		b"f32.ceil" => value!(Opcode::F32Ceil ) |
		b"f32.floor" => value!(Opcode::F32Floor ) |
		b"f32.trunc" => value!(Opcode::F32Trunc ) |
		b"f32.nearest" => value!(Opcode::F32Nearest ) |

        b"f64.abs" => value!(Opcode::F64Abs) |
		b"f64.neg" => value!(Opcode::F64Neg) |
		b"f64.sqrt" => value!(Opcode::F64Sqrt ) |
		b"f64.ceil" => value!(Opcode::F64Ceil ) |
		b"f64.floor" => value!(Opcode::F64Floor ) |
		b"f64.trunc" => value!(Opcode::F64Trunc ) |
		b"f64.nearest" => value!(Opcode::F64Nearest )
    )
);

named!(
    unop,
    alt!(
        tag!("clz") | tag!("ctz") | tag!("popcnt") | tag!("abs") | tag!("neg") | tag!("sqrt") | tag!("ceil")
            | tag!("floor") | tag!("trunc") | tag!("nearest")
    )
);

/// <val_type>.<binop>
named!(
    pub binary<Opcode>,
    alt!(
        tag!("i32.add") => { |_| Opcode::I32Add } |
		tag!("i32.sub") => { |_| Opcode::I32Sub } |
		tag!("i32.mul") => { |_| Opcode::I32Mul } |
		tag!("i32.div_s") => { |_| Opcode::I32DivS } |
		tag!("i32.div_u") => { |_| Opcode::I32DivU } |
		tag!("i32.rem_s") => { |_| Opcode::I32RemS } |
		tag!("i32.rem_u") => { |_| Opcode::I32RemU } |
		tag!("i32.and") => { |_| Opcode::I32And } |
		tag!("i32.or") => { |_| Opcode::I32Or } |
		tag!("i32.xor") => { |_| Opcode::I32Xor } |
		tag!("i32.shl") => { |_| Opcode::I32Shl } |
		tag!("i32.shr_s") => { |_| Opcode::I32ShrS } |
		tag!("i32.shr_u") => { |_| Opcode::I32ShrU } |
		tag!("i32.rotl") => { |_| Opcode::I32Rotl } |
		tag!("i32.rotr") => { |_| Opcode::I32Rotr } |

        tag!("i64.add") => { |_| Opcode::I64Add } |
		tag!("i64.sub") => { |_| Opcode::I64Sub } |
		tag!("i64.mul") => { |_| Opcode::I64Mul } |
		tag!("i64.div_s") => { |_| Opcode::I64DivS } |
		tag!("i64.div_u") => { |_| Opcode::I64DivU } |
		tag!("i64.rem_s") => { |_| Opcode::I64RemS } |
		tag!("i64.rem_u") => { |_| Opcode::I64RemU } |
		tag!("i64.and") => { |_| Opcode::I64And } |
		tag!("i64.or") => { |_| Opcode::I64Or } |
		tag!("i64.xor") => { |_| Opcode::I64Xor } |
		tag!("i64.shl") => { |_| Opcode::I64Shl } |
		tag!("i64.shr_s") => { |_| Opcode::I64ShrS } |
		tag!("i64.shr_u") => { |_| Opcode::I64ShrU } |
		tag!("i64.rotl") => { |_| Opcode::I64Rotl } |
		tag!("i64.rotr") => { |_| Opcode::I64Rotr } |

		tag!("f32.add") => { |_| Opcode::F32Add } |
		tag!("f32.sub") => { |_| Opcode::F32Sub } |
		tag!("f32.mul") => { |_| Opcode::F32Mul } |
		tag!("f32.div") => { |_| Opcode::F32Div } |
		tag!("f32.min") => { |_| Opcode::F32Min } |
		tag!("f32.max") => { |_| Opcode::F32Max } |
		tag!("f32.copysign") => { |_| Opcode::F32Copysign } |

		tag!("f64.add") => { |_| Opcode::F64Add } |
		tag!("f64.sub") => { |_| Opcode::F64Sub } |
		tag!("f64.mul") => { |_| Opcode::F64Mul } |
		tag!("f64.div") => { |_| Opcode::F64Div } |
		tag!("f64.min") => { |_| Opcode::F64Min } |
		tag!("f64.max") => { |_| Opcode::F64Max } |
		tag!("f64.copysign") => { |_| Opcode::F64Copysign }
    )
);

///<val_type>.<relop>
named!(
    pub compare<Opcode>,
    alt!(
        tag!("i32.eq") => { |_| Opcode::I32Eq } |
		tag!("i32.ne") => { |_| Opcode::I32Ne } |
		tag!("i32.lt_s") => { |_| Opcode::I32LtS } |
		tag!("i32.lt_u") => { |_| Opcode::I32LtU } |
		tag!("i32.gt_s") => { |_| Opcode::I32GtS } |
		tag!("i32.gt_u") => { |_| Opcode::I32GtU } |
		tag!("i32.le_s") => { |_| Opcode::I32LeS } |
		tag!("i32.le_u") => { |_| Opcode::I32LeU } |
		tag!("i32.ge_s") => { |_| Opcode::I32GeS } |
		tag!("i32.ge_u") => { |_| Opcode::I32GeU } |

        tag!("i64.eq") => { |_| Opcode::I64Eq } |
		tag!("i64.ne") => { |_| Opcode::I64Ne } |
		tag!("i64.lt_s") => { |_| Opcode::I64LtS } |
		tag!("i64.lt_u") => { |_| Opcode::I64LtU } |
		tag!("i64.gt_s") => { |_| Opcode::I64GtS } |
		tag!("i64.gt_u") => { |_| Opcode::I64GtU } |
		tag!("i64.le_s") => { |_| Opcode::I64LeS } |
		tag!("i64.le_u") => { |_| Opcode::I64LeU } |
		tag!("i64.ge_s") => { |_| Opcode::I64GeS } |
		tag!("i64.ge_u") => { |_| Opcode::I64GeU } |

        tag!("f32.eq") => { |_| Opcode::F32Eq } |
		tag!("f32.ne") => { |_| Opcode::F32Ne } |
		tag!("f32.lt") => { |_| Opcode::F32Lt } |
        tag!("f32.gt") => { |_| Opcode::F32Gt } |
        tag!("f32.le") => { |_| Opcode::F32Le } |
        tag!("f32.ge") => { |_| Opcode::F32Ge } |

        tag!("f64.eq") => { |_| Opcode::F64Eq } |
		tag!("f64.ne") => { |_| Opcode::F64Ne } |
		tag!("f64.lt") => { |_| Opcode::F64Lt } |
        tag!("f64.gt") => { |_| Opcode::F64Gt } |
        tag!("f64.le") => { |_| Opcode::F64Le } |
        tag!("f64.ge") => { |_| Opcode::F64Ge }
    )
);

/// <val_type>.<cvtop>/<val_type>
named!(
    pub convert<Opcode>,
    alt!(
		tag!("i32.wrap/i64") => { |_| Opcode::I32WrapI64 } |
		tag!("i32.trunc_s/f32") => { |_| Opcode::I32TruncSF32 } |
		tag!("i32.trunc_u/f32") => { |_| Opcode::I32TruncUF32 } |
		tag!("i32.trunc_s/f64") => { |_| Opcode::I32TruncSF64 } |
		tag!("i32.trunc_u/f64") => { |_| Opcode::I32TruncUF64 } |
		tag!("i64.extend_s/i32") => { |_| Opcode::I64ExtendSI32 } |
		tag!("i64.extend_u/i32") => { |_| Opcode::I64ExtendUI32 } |
		tag!("i64.trunc_s/f32") => { |_| Opcode::I64TruncSF32 } |
		tag!("i64.trunc_u/f32") => { |_| Opcode::I64TruncSF64 } |
		tag!("i64.trunc_s/f64") => { |_| Opcode::I64TruncSF64 } |
		tag!("i64.trunc_u/f64") => { |_| Opcode::I64TruncUF64 } |
		tag!("f32.convert_s/i32") => { |_| Opcode::F32ConvertSI32 } |
		tag!("f32.convert_u/i32") => { |_| Opcode::F32ConvertUI32 } |
		tag!("f32.convert_s/i64") => { |_| Opcode::F32ConvertSI64 } |
		tag!("f32.convert_u/i64") => { |_| Opcode::F32ConvertUI64 } |
		tag!("f32.demote/f64") => { |_| Opcode::F32DemoteF64 } |
		tag!("f64.convert_s/i32") => { |_| Opcode::F64ConvertSI32 } |
		tag!("f64.convert_u/i32") => { |_| Opcode::F64ConvertUI32 } |
		tag!("f64.convert_s/i64") => { |_| Opcode::F64ConvertSI64 } |
		tag!("f64.convert_u/i64") => { |_| Opcode::F64ConvertUI64 } |
		tag!("f64.promote/f32") => { |_| Opcode::F64PromoteF32 } |
		tag!("i32.reinterpret/f32") => { |_| Opcode::I32ReinterpretF32 } |
		tag!("i64.reinterpret/f64") => { |_| Opcode::I64ReinterpretF64 } |
		tag!("f32.reinterpret/i32") => { |_| Opcode::F32ReinterpretI32 } |
		tag!("f64.reinterpret/i64") => { |_| Opcode::F64ReinterpretI64 }
    )
);
