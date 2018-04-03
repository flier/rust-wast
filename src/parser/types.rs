use parity_wasm::elements::{GlobalType, MemoryType, TableType, ValueType};

use super::nat32;

named!(pub value_type_list<Vec<ValueType>>, many0!(first!(value_type)));

/// value_type: i32 | i64 | f32 | f64
named!(pub value_type<ValueType>, alt!(int_type | float_type ));

named!(
    pub int_type<ValueType>,
    alt!(
        tag!("i32") => { |_| ValueType::I32 } |
        tag!("i64") => { |_| ValueType::I64 }
    )
);

named!(
    pub float_type<ValueType>,
    alt!(
        tag!("f32") => { |_| ValueType::F32 } |
        tag!("f64") => { |_| ValueType::F64 }
    )
);

named!(pub limits<(u32, Option<u32>)>, pair!(first!(nat32), opt!(first!(nat32))));

named!(
    pub memory_type<MemoryType>,
    map!(ws!(pair!(nat32, opt!(nat32))), |(min, max)| MemoryType::new(min, max))
);

named!(
    pub table_type<TableType>,
    map!(pair!(limits, first!(elem_type)), |((min, max), _)| TableType::new(
        min,
        max
    ))
);

named!(pub elem_type, tag!("anyfunc"));

named!(
    pub global_type<GlobalType>,
    alt!(
        value_type => { |ty| GlobalType::new(ty, false) } |
        mut_value_type => { |ty| GlobalType::new(ty, true) }
    )
);

named!(
    mut_value_type<ValueType>,
    ws!(delimited!(tag!("("), preceded!(tag!("mut"), value_type), tag!(")")))
);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_global_type() {
        let tests: Vec<(&[u8], _, _)> = vec![(b"i32", ValueType::I32, false), (b"(mut i64)", ValueType::I64, true)];

        for (code, value_type, is_mutable) in tests {
            let (remaining, global_type) = global_type(code).unwrap();

            assert!(remaining.is_empty());
            assert_eq!(global_type.content_type(), value_type);
            assert_eq!(global_type.is_mutable(), is_mutable);
        }
    }
}
