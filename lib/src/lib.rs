#![deny(
	absolute_paths_not_starting_with_crate,
	keyword_idents,
	macro_use_extern_crate,
	meta_variable_misuse,
	missing_abi,
	non_ascii_idents,
	nonstandard_style,
	noop_method_call,
	pointer_structural_match,
	private_in_public,
	rust_2018_idioms,
	unused_qualifications
)]
#![warn(clippy::pedantic)]
#![forbid(unsafe_code)]

use std::num::NonZeroU8;
use std::ops::Range;

use logos::Logos;

pub use crate::config::Config;
use crate::config::Spacing;

pub mod config;
mod tests;

#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq)]
#[logos(skip r"\s+")]
enum Token {
	/// This is only separate from `LeftOperator` to allow postprocessing to find `LabelDefinition`s.
	#[token(":")]
	Colon,
	/// In a later stage, operands are retagged as this if they are followed by a colon.
	/// Except for initial indentation, this should be treated the same as `Operand`.
	LabelDefinition,
	#[token(",")]
	#[token(";")] // Statement separator.
	LeftOperator,
	#[regex(r"[\])}]")] // Closing delimiters.
	ClosingLeftOperator,
	#[token("<<")] // Binary operators.
	#[token(">>")] // Binary operators.
	#[regex("[><=]=?")] // Binary operators.
	#[regex(r"[+*/%&|]")] // Binary operators.
	MiddleOperator,
	#[regex(r"[!~]")] // Unary operators.
	#[regex(r"%[a-zA-Z_]+")] // e.g., `%hi(symbol)`.
	#[regex(r"[\[({]")] // Opening delimiters.
	RightOperator,
	#[token("-")]
	MiddleOrRightOperator,
	#[regex(r"[a-zA-Z_][a-zA-Z0-9_.]*")] // Identifier.
	#[regex(r"0[bB][01]+")] // Binary number.
	#[regex(r"0[xX][0-9a-fA-F]+")] // Hex number.
	#[regex(r"[0-9]+[bf]?")] // Decimal number, 0-prefixed octal, and numeric label references.
	#[regex(r"[0-9]+\.[0-9]*([eE][-+]?[0-9]+)?")] // Float: 1. or 1.0 or 1.e6 or 1.0e6.
	#[regex(r"\.[0-9]+([eE][-+]?[0-9]+)?")] // Float: .1 or .1e6.
	#[regex(r"[0-9]+[eE][-+]?[0-9]+")] // Float: 1e6.
	#[regex(r#""((\\([\\"]|\n[ \t]*))|[^"])*""#)] // String literal.
	#[regex(r#"'((\\([\\']|\n[ \t]*))|[^'])*'"#)] // Character literal.
	Operand,
	#[regex(r"\.[a-zA-Z_][a-zA-Z0-9_.]*")]
	Directive,
	#[regex(r"(#|//)[^\n]*")]
	LineComment,
	#[regex(r"/\*([^*]|\*[^/])*\*/")]
	BlockComment,
}

impl Token {
	fn is_comment(self) -> bool {
		matches!(self, Self::LineComment | Self::BlockComment)
	}
}

#[derive(Debug)]
pub enum Error {
	/// The lexer did not understand the token around `span`.
	InvalidToken { span: Range<usize> },
}

pub type Result<T, E = Error> = std::result::Result<T, E>;

const ONE_NZU8: NonZeroU8 = match NonZeroU8::new(1) {
	Some(v) => v,
	None => panic!(),
};

/// # Errors
///
/// For any reason present in [`Error`].
pub fn format(input: &str, config: &Config) -> Result<String> {
	let mut tokens = Token::lexer(input)
		.spanned()
		.map(|(token, span)| {
			Ok((
				span.clone(),
				token.map_err(|()| Error::InvalidToken { span })?,
			))
		})
		.collect::<Result<Vec<_>>>()?;

	post_process(&mut tokens);

	let mut output = String::with_capacity(input.len() / 2);

	let mut last: Option<(Range<usize>, Token)> = None;
	let mut nth_non_comment_on_line = 0;

	for (span, token) in tokens.iter().cloned() {
		// Whitespace between the previous token (or the start of the file) and this token.
		{
			let whitespace_span = last
				.clone()
				.map_or(0, |(last_span, _last_token)| last_span.end)..span.start;
			let old_text = &input[whitespace_span];
			let old_newlines = old_text.bytes().filter(|&ch| ch == b'\n').count();

			let last_on_same_line = last.clone().filter(|_| old_newlines == 0);
			let new_whitespace = if let Some((_last_span, last_token)) = last_on_same_line {
				match within_line_rule(last_token, token) {
					FormattedWhitespace::Empty => Spacing::None,
					FormattedWhitespace::Spaced => {
						if nth_non_comment_on_line == 1
							&& last.clone().map(|(_span, token)| token) == Some(Token::Operand)
						{
							config.after_mnemonic
						} else {
							Spacing::Spaces(ONE_NZU8)
						}
					}
					FormattedWhitespace::BeforeInlineComment => config.before_inline_comment,
				}
			} else {
				indentation_at_start_of_line(token).resolve(config, old_text)
			}
			.as_str();

			let num_newlines = old_newlines.min(usize::from(config.max_empty_lines) + 1);

			output.reserve(num_newlines + new_whitespace.len());
			for _ in 0..num_newlines {
				output += "\n";
			}
			if num_newlines > 0 {
				nth_non_comment_on_line = 0;
			}
			output += new_whitespace;
		}

		// The token itself.
		{
			let text = &input[span.clone()];
			output += text;
		}

		if !token.is_comment() {
			nth_non_comment_on_line += 1;
		}
		last = Some((span, token));
	}

	if last.is_some() {
		output += "\n";
	}

	Ok(output)
}

fn post_process(tokens: &mut [(Range<usize>, Token)]) {
	post_process_pairs(tokens);

	if let Some(last) = tokens
		.iter_mut()
		.rev()
		.map(|(_span, token)| token)
		.find(|&&mut token| token != Token::BlockComment)
	{
		if *last == Token::MiddleOrRightOperator {
			*last = Token::RightOperator;
		}
	}
}

fn post_process_pairs(tokens: &mut [(Range<usize>, Token)]) {
	let mut iter = tokens
		.iter_mut()
		.map(|(_span, token)| token)
		.filter(|&&mut token| token != Token::BlockComment);
	let Some(mut prev) = iter.next() else {
		return;
	};
	for current in iter {
		post_process_pair(prev, current);
		prev = current;
	}
}

#[allow(clippy::match_same_arms)] // Case-by-case clarity.
fn post_process_pair(first: &mut Token, second: &mut Token) {
	match (*first, *second) {
		(Token::Operand, Token::Colon) => {
			*first = Token::LabelDefinition;
		}
		(before, Token::MiddleOrRightOperator) => {
			// The general rule here is that if the operator is preceded by an operand, it's a MiddleOperator;
			// otherwise, it's a RightOperator.
			*second = match before {
				// e.g., `) - 3`.
				Token::ClosingLeftOperator => Token::MiddleOperator,
				// e.g. `2 - 3`.
				Token::Operand => Token::MiddleOperator,
				// e.g. `hello: -3`
				Token::Colon => Token::RightOperator,
				// e.g. `2, -3`
				Token::LeftOperator => Token::RightOperator,
				// e.g., `+ -3`.
				Token::MiddleOperator => Token::RightOperator,
				// e.g., `(-3`. Will also correctly handle e.g., `- -----3`.
				Token::RightOperator => Token::RightOperator,
				// e.g., `- -3`.
				Token::MiddleOrRightOperator => Token::RightOperator,
				// e.g. `.word -2`
				Token::Directive => Token::RightOperator,
				// The operator would be at the start of the next line, thus, there cannot be an operand preceding it.
				Token::LineComment => Token::RightOperator,
				// Excluded by `post_process`.
				Token::BlockComment => unreachable!(),
				// Can only be followed by a colon.
				Token::LabelDefinition => unreachable!(),
			}
		}
		_ => (),
	}
}

enum FormattedWhitespace {
	Empty,
	Spaced,
	BeforeInlineComment,
}

#[allow(clippy::match_same_arms)]
fn within_line_rule(first: Token, second: Token) -> FormattedWhitespace {
	use FormattedWhitespace as A;

	match (first, second) {
		// `MiddleOrRightOperator` will always be replaced with `MiddleOperator` or `RightOperator`.
		(_, Token::MiddleOrRightOperator) | (Token::MiddleOrRightOperator, _) => unreachable!(),
		(_, Token::LineComment) => A::BeforeInlineComment,
		(_, Token::Colon | Token::LeftOperator | Token::ClosingLeftOperator) => A::Empty,
		// It will only be followed by `Colon`, which was already handled in the previous case.
		(Token::LabelDefinition, _) => unreachable!(),
		(Token::RightOperator, _) => A::Empty,
		// This special case handles e.g., `8(sp)`.
		(Token::Operand, Token::RightOperator) => A::Empty,
		(_, Token::MiddleOperator | Token::RightOperator)
		| (Token::MiddleOperator | Token::LeftOperator | Token::ClosingLeftOperator, _) => A::Spaced,
		(Token::Colon, _) => A::Spaced,
		// Nothing can come after it within a line.
		(Token::LineComment, _) => unreachable!(),
		(
			Token::BlockComment | Token::Operand | Token::Directive,
			Token::Operand | Token::Directive | Token::LabelDefinition | Token::BlockComment,
		) => A::Spaced,
	}
}

enum Indented {
	Yes,
	No,
	Keep,
}

impl Indented {
	fn resolve(self, config: &Config, old_text: &str) -> Spacing {
		let indented = match self {
			Indented::Yes => true,
			Indented::No => false,
			Indented::Keep => !old_text.split('\n').last().unwrap().is_empty(),
		};
		if indented {
			config.indentation
		} else {
			Spacing::None
		}
	}
}

fn indentation_at_start_of_line(token: Token) -> Indented {
	match token {
		Token::LabelDefinition => Indented::No,
		Token::LineComment | Token::BlockComment | Token::Directive => Indented::Keep,
		_ => Indented::Yes,
	}
}
