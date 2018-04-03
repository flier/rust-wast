use parity_wasm::elements::MemoryType;

use super::{bind_var, inline_export, inline_import, memory_type, string_list};
use ast::{Memory, Var};

named!(
    pub memory<(Option<Var>, Memory)>,
    parsing!(
        Memory,
        dbg_dmp!(delimited!(
            tag!("("),
            preceded!(
                first!(tag!("memory")),
                pair!(opt!(first!(bind_var)), first!(memory_fields))
            ),
            tag!(")")
        ))
    )
);

named!(
    memory_fields<Memory>,
    alt!(
        first!(memory_type) => { |memory_type| Memory { memory_type, elements: vec![] } } |
        pair!(first!(inline_import), first!(memory_type)) => {
            |((module_name, item_name), memory_type)| Memory { memory_type, elements: vec![] }
        } |
        pair!(first!(inline_export), first!(memory_fields)) => {
            |(export_name, memory)| memory
        } |
        delimited!(
            tag!("("),
            preceded!(first!(tag!("data")), string_list),
            tag!(")")
        ) => { |strs: Vec<String>|
        {
            let elements = strs.into_iter().fold(vec![], |mut value, s| {
                value.extend_from_slice(s.as_bytes());
                value
            });

            Memory{
                memory_type: MemoryType::new(elements.len() as u32, Some(elements.len() as u32)),
                elements,
            }
        }}
    )
);

#[cfg(test)]
mod tests {
    use std::str;

    use nom::IResult::Done;
    use pretty_env_logger;

    use super::*;

    #[test]
    fn parse_memory() {
        let _ = pretty_env_logger::try_init();

        let tests: Vec<(&[u8], _)> = vec![
            (
                b"(memory 0 0)",
                (
                    None,
                    Memory {
                        memory_type: MemoryType::new(0, Some(0)),
                        elements: vec![],
                    },
                ),
            ),
            (
                b"(memory 0 1)",
                (
                    None,
                    Memory {
                        memory_type: MemoryType::new(0, Some(1)),
                        elements: vec![],
                    },
                ),
            ),
            (
                b"(memory 1 256)",
                (
                    None,
                    Memory {
                        memory_type: MemoryType::new(1, Some(256)),
                        elements: vec![],
                    },
                ),
            ),
            (
                b"(memory 0 65536)",
                (
                    None,
                    Memory {
                        memory_type: MemoryType::new(0, Some(65536)),
                        elements: vec![],
                    },
                ),
            ),
            (
                b"(memory (data))",
                (
                    None,
                    Memory {
                        memory_type: MemoryType::new(0, Some(0)),
                        elements: vec![],
                    },
                ),
            ),
            (
                br#"(memory (data ""))"#,
                (
                    None,
                    Memory {
                        memory_type: MemoryType::new(0, Some(0)),
                        elements: vec![],
                    },
                ),
            ),
            (
                br#"(memory (data "x"))"#,
                (
                    None,
                    Memory {
                        memory_type: MemoryType::new(1, Some(1)),
                        elements: b"x".to_vec(),
                    },
                ),
            ),
            (
                br#"(memory (import "spectest" "memory") 1 2)"#,
                (
                    None,
                    Memory {
                        memory_type: MemoryType::new(1, Some(2)),
                        elements: vec![],
                    },
                ),
            ),
            (
                br#"(memory (export "memory-2-inf") 2)"#,
                (
                    None,
                    Memory {
                        memory_type: MemoryType::new(2, None),
                        elements: vec![],
                    },
                ),
            ),
        ];

        for (code, ref result) in tests {
            let res = memory(code);

            trace_parse_error!(code, res);

            assert_eq!(res, Done(&[][..], result.clone()), "parse memory: {}", unsafe {
                str::from_utf8_unchecked(code)
            });
        }
    }
}
