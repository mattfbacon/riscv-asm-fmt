#![deny(
	absolute_paths_not_starting_with_crate,
	keyword_idents,
	macro_use_extern_crate,
	meta_variable_misuse,
	missing_abi,
	missing_copy_implementations,
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

use std::io::Write;
use std::num::NonZeroU8;
use std::ops::Range;
use std::path::PathBuf;

use argh::FromArgs;
use rvfmt_lib::config::Spacing;
use rvfmt_lib::{Config, Error};

fn spacing_from_str(raw: &str) -> Result<Spacing, String> {
	match raw {
		"tab" => return Ok(Spacing::Tab),
		"none" => return Ok(Spacing::None),
		_ => {}
	}

	if let Some(num_spaces) = raw.strip_suffix("spaces").or(raw.strip_suffix("space")) {
		let num_spaces = num_spaces
			.parse()
			.map_err(|_| format!("invalid number {num_spaces:?}"))?;
		return Ok(NonZeroU8::new(num_spaces).map_or(Spacing::None, Spacing::Spaces));
	}

	Err("expected `tab`, `<n> space(s)`, or `none`".into())
}

/// Format RISC-V Assembly from stdin to stdout.
#[derive(FromArgs)]
struct Args {
	/// the character(s) to use for indentation
	///
	/// Defaults to a tab.
	#[argh(option, from_str_fn(spacing_from_str))]
	indentation: Option<Spacing>,
	/// the character(s) to use for spacing immediately after instruction mnemonics
	///
	/// Defaults to one space.
	#[argh(option, from_str_fn(spacing_from_str))]
	after_mnemonic: Option<Spacing>,
	/// the character(s) to use for spacing before inline line comments
	///
	/// Defaults to two spaces.
	#[argh(option, from_str_fn(spacing_from_str))]
	before_inline_comment: Option<Spacing>,
	/// more than this number of empty lines will be trimmed down
	#[argh(option)]
	max_empty_lines: Option<u8>,

	#[argh(positional)]
	files: Vec<PathBuf>,
}

fn main() {
	let args: Args = argh::from_env();

	let mut config = Config::default();

	if let Some(indentation) = args.indentation {
		config.indentation = indentation;
	}

	if let Some(after_mnemonic) = args.after_mnemonic {
		config.after_mnemonic = after_mnemonic;
	}

	if let Some(before_inline_comment) = args.before_inline_comment {
		config.before_inline_comment = before_inline_comment;
	}

	if let Some(max_empty_lines) = args.max_empty_lines {
		config.max_empty_lines = max_empty_lines;
	}

	if args.files.is_empty() {
		let input = std::io::read_to_string(std::io::stdin()).expect("reading from stdin");

		match rvfmt_lib::format(&input, &config) {
			Err(error) => report_error(&input, &config, &error),
			Ok(output) => {
				std::io::stdout()
					.write_all(output.as_bytes())
					.expect("writing to stdout");
			}
		}
	} else {
		for file in &args.files {
			let input = std::fs::read_to_string(file)
				.unwrap_or_else(|error| panic!("reading {file:?} to string: {error}"));

			match rvfmt_lib::format(&input, &config) {
				Err(error) => report_error(&input, &config, &error),
				Ok(output) => {
					std::fs::write(file, output.as_bytes())
						.unwrap_or_else(|error| panic!("writing output to {file:?}: {error}"));
				}
			}
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct CharIndex {
	first_byte: usize,
	char_index: usize,
}

impl std::ops::Add for CharIndex {
	type Output = CharIndex;

	fn add(self, other: Self) -> Self {
		Self {
			first_byte: self.first_byte + other.first_byte,
			char_index: self.char_index + other.char_index,
		}
	}
}

fn byte_index_to_char_index(source: &str, byte_index: usize) -> CharIndex {
	let mut ret = CharIndex {
		first_byte: 0,
		char_index: 0,
	};

	for ch in source.chars() {
		if byte_index < ret.first_byte + ch.len_utf8() {
			break;
		}
		ret.char_index += 1;
		ret.first_byte += ch.len_utf8();
	}

	ret
}

fn byte_span_to_char_span(source: &str, span: Range<usize>) -> Option<Range<usize>> {
	if span.start > span.end {
		return None;
	}

	let start = byte_index_to_char_index(source, span.start);
	let end = byte_index_to_char_index(&source[start.first_byte..], span.end - span.start) + start;
	Some(start.char_index..end.char_index)
}

fn report_error(source: &str, config: &Config, error: &Error) -> ! {
	let tab_width = match config.indentation {
		Spacing::Spaces(n) => n.get().into(),
		_ => 2,
	};

	let span_bytes = match error {
		Error::InvalidToken { span } => span.clone(),
	};
	let (message, label, note) = match error {
		Error::InvalidToken { .. } => (
			"Invalid token.",
			"The lexer does not understand the token around this point.",
			"If your code is valid, this is a bug in the formatter.",
		),
	};

	let span_chars = byte_span_to_char_span(source, span_bytes).unwrap();
	let report = ariadne::Report::build(ariadne::ReportKind::Error, (), span_chars.start)
		.with_config(ariadne::Config::default().with_tab_width(tab_width))
		.with_message(message)
		.with_label(ariadne::Label::new(span_chars).with_message(label))
		.with_note(note)
		.finish();
	report.eprint(ariadne::Source::from(source)).unwrap();

	std::process::exit(1);
}
