use parity_wasm::elements::FunctionType;

use super::{func_type, typeidx, LPAR, RPAR, TYPE};
use ast::Var;

named!(
    pub typeuse<(Option<Var>, Option<FunctionType>)>,
    parsing!(
        TypeUse,
        pair!(
            opt!(use_type),
            func_type
        )
    )
);

named!(use_type<Var>, delimited!(LPAR, preceded!(TYPE, typeidx), RPAR));
