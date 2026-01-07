use std::{
    fs::File,
    io::{BufRead, BufReader, Result},
    path::{Path, PathBuf},
};

use insta::{assert_snapshot, assert_yaml_snapshot, glob};
use rstest::rstest;
use toyc::{
    lexer::{Category, Tokeniser},
    util::CompilerPass,
};

const LEXER_FAIL: u32 = 250;
const PASS: u32 = 0;

fn test_lexer(path: &Path) -> u32 {
    let mut tokeniser = Tokeniser::from_path(path).unwrap();
    while tokeniser.next_token().unwrap().category() != Category::Eof {}
    if tokeniser.has_error() {
        LEXER_FAIL
    } else {
        PASS
    }
}

#[rstest]
fn lexer(
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
        0
    };

    assert_eq!(lexer_exit_code, test_lexer(path.as_path()));
    Ok(())
}
