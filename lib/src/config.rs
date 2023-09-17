use std::num::NonZeroU8;

#[derive(Debug)]
pub struct Config {
	pub indentation: Spacing,
	pub after_mnemonic: Spacing,
	pub before_inline_comment: Spacing,
	pub max_empty_lines: u8,
}

impl Default for Config {
	fn default() -> Self {
		Self {
			indentation: Spacing::Tab,
			after_mnemonic: Spacing::Spaces(NonZeroU8::new(1).unwrap()),
			before_inline_comment: Spacing::Spaces(NonZeroU8::new(2).unwrap()),
			max_empty_lines: 1,
		}
	}
}

#[derive(Debug, Clone, Copy)]
pub enum Spacing {
	Tab,
	/// The number of spaces should not be more than eight.
	Spaces(NonZeroU8),
	None,
}

impl Spacing {
	#[must_use]
	pub(crate) fn as_str(self) -> &'static str {
		match self {
			Self::Tab => "\t",
			Self::Spaces(num) => {
				let spaces = "        ";
				let len = usize::from(num.get());
				spaces.get(..len).unwrap_or(spaces)
			}
			Self::None => "",
		}
	}
}
