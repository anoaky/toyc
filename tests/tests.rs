use std::{
    fs::File,
    io::{BufRead, BufReader},
    path::{Path, PathBuf},
};

use anyhow::Result;

use insta::{assert_debug_snapshot, assert_snapshot, assert_yaml_snapshot};
use rstest::rstest;
use toyc::{
    ast::{format_program, program::Program},
    lexer::{Category, Tokeniser},
    parser::Parser,
    util::CompilerPass,
};

const LEXER_FAIL: u32 = 250;
const PARSER_FAIL: u32 = 245;
const PASS: u32 = 0;

macro_rules! set_snapshot_suffix {
    ($($expr:expr),*) => {
        let mut settings = insta::Settings::clone_current();
        settings.set_snapshot_suffix(format!($($expr,)*));
        let _guard = settings.bind_to_scope();
    }
}

fn test_lexer(path: &Path) -> u32 {
    let mut tokeniser = Tokeniser::from_path(path).unwrap();
    while tokeniser.next_token().unwrap().category() != Category::Eof {}
    if tokeniser.has_error() {
        LEXER_FAIL
    } else {
        PASS
    }
}

fn test_parser(path: &Path) -> Result<(u32, Program)> {
    let tokeniser = Tokeniser::from_path(path).unwrap();
    let mut parser = Parser::with_tokeniser(tokeniser)?;
    let program = parser.parse()?;
    let exit_code = if parser.has_error() {
        PARSER_FAIL
    } else {
        PASS
    };
    let ast_string = format_program(&program)?;
    Ok((exit_code, program))
}

#[rstest]
fn test(
    #[base_dir = "tests/resources/source/"]
    #[files("*.tc")]
    path: PathBuf,
) -> Result<()> {
    let mut reader = BufReader::new(File::open(&path)?);
    let mut line = String::new();
    reader.read_line(&mut line)?;
    let expected_exit_code = if line.starts_with("/*") {
        // extract exit code
        line.clear();
        reader.read_line(&mut line)?;
        line.trim().parse::<u32>().unwrap_or(1)
    } else if line.starts_with("//") {
        line.split_off(2).trim().parse::<u32>().unwrap_or(1)
    } else {
        0
    };
    let lexer_exit_code = if expected_exit_code == LEXER_FAIL {
        LEXER_FAIL
    } else {
        PASS
    };
    let parser_exit_code = if expected_exit_code == PARSER_FAIL {
        PARSER_FAIL
    } else {
        PASS
    };

    assert_eq!(lexer_exit_code, test_lexer(path.as_path()));
    if expected_exit_code < LEXER_FAIL {
        let (parser_exit, ast) = test_parser(path.as_path())?;
        assert_eq!(parser_exit_code, parser_exit);
        if parser_exit_code == PASS {
            set_snapshot_suffix!("parser");
            assert_yaml_snapshot!(path.file_stem().unwrap().to_str().unwrap(), ast);
        }
    }
    Ok(())
}
