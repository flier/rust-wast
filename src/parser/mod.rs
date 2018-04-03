#[macro_use]
pub mod macros;
mod values;
mod var;
mod types;
mod comment;
mod skip;
mod ops;
mod instr;
mod expr;
mod type_def;
mod global;
mod memory;
mod data;
mod table;
mod elem;
mod import;
mod export;
mod module;

pub use self::comment::comment;
pub use self::data::data;
pub use self::elem::elem;
pub use self::export::inline_export;
pub use self::expr::{expr, expr_list, init_expr, offset};
pub use self::global::global;
pub use self::import::inline_import;
pub use self::instr::{block, block_type, call_instr, instr_list, plain_instr, type_use};
pub use self::memory::memory;
pub use self::module::module;
pub use self::ops::{align, binary, compare, convert, mem_size, sign, test, unary};
pub use self::skip::skip;
pub use self::table::table;
pub use self::type_def::type_def;
pub use self::types::{elem_type, float_type, func_type, global_type, int_type, limits, memory_type, table_type,
                      value_type, value_type_list};
pub use self::values::{float, int, name, nat, string, string_list, float32, float64, int32, int64, nat32};
pub use self::var::{bind_var, var, var_list};