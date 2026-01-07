use std::{
    io::Result,
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
) {
    assert_snapshot!(test_lexer(path.as_path()));
}
