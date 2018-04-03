#[macro_export]
macro_rules! first(
    ($input:expr, $submacro:ident!($($arguments:tt)*)) => (
        {
            preceded!(
                $input,
                call!($crate::parser::skip),
                $submacro!($($arguments)*)
            )
        }
    );

    ($input:expr, $f:expr) => (
        first!($input, call!($f));
    );
);
