#![cfg(test)]

use crate::Config;

macro_rules! tests {
	() => {};
	($name:ident with $config:expr; $($rest:tt)*) => {
		#[test]
		fn $name() {
			let input = include_str!(concat!(stringify!($name), ".in"));
			let expected = include_str!(concat!(stringify!($name), ".out"));
			let output = crate::format(&input, &$config).unwrap();
			assert_eq!(output, expected);
		}
		tests!($($rest)*);
	};
	($name:ident; $($rest:tt)*) => {
		tests!($name with Config::default(); $($rest)*);
	};
}

tests! {
	empty;
	comprehensive;
	expr;
}
