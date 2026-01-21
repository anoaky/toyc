use std::{
    fs::File,
    io::{BufRead, BufReader},
    path::{Path, PathBuf},
};

use anyhow::Result;
use insta::assert_ron_snapshot;
use rstest::rstest;
use toyc::{
    ast::DeclKind,
    hir::{HirPool, SemanticAnalysis, TypedHir},
    lexer::{Category, Tokeniser},
    parser::Parser,
    util::CompilerPass,
};

const LEXER_FAIL: u32 = 250;
const PARSER_FAIL: u32 = 245;
const SEM_FAIL: u32 = 240;
const PASS: u32 = 0;

type Ast = Vec<DeclKind>;

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

fn test_parser(path: &Path) -> Result<(u32, Ast)> {
    let tokeniser = Tokeniser::from_path(path).unwrap();
    let mut parser = Parser::with_tokeniser(tokeniser)?;
    let program = parser.parse()?;
    let exit_code = if parser.has_error() {
        PARSER_FAIL
    } else {
        PASS
    };
    Ok((exit_code, program))
}

fn test_sem(path: &Path) -> Result<(u32, HirPool<TypedHir>)> {
    let mut sem_analysis = SemanticAnalysis::from_path(path)?;
    let typed_hir = sem_analysis.analyse();
    match typed_hir {
        Ok(typed_hir) => Ok((PASS, typed_hir)),
        Err(_) => Ok((SEM_FAIL, HirPool::new())),
    }
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
    let lexer_expected = if expected_exit_code == LEXER_FAIL {
        LEXER_FAIL
    } else {
        PASS
    };
    let parser_expected = if expected_exit_code == PARSER_FAIL {
        PARSER_FAIL
    } else {
        PASS
    };
    let sem_expected = if expected_exit_code == SEM_FAIL {
        SEM_FAIL
    } else {
        PASS
    };

    assert_eq!(lexer_expected, test_lexer(path.as_path()));
    if expected_exit_code < LEXER_FAIL {
        let (parser_actual, ast) = test_parser(path.as_path())?;
        assert_eq!(parser_expected, parser_actual);
        if parser_expected == PASS {
            set_snapshot_suffix!("parser");
            assert_ron_snapshot!(path.file_stem().unwrap().to_str().unwrap(), ast);
        }
    }
    if expected_exit_code < PARSER_FAIL {
        let (sem_actual, typed_hir) = test_sem(path.as_path())?;
        assert_eq!(sem_expected, sem_actual);
        if sem_expected == PASS {
            set_snapshot_suffix!("sem");
            assert_ron_snapshot!(path.file_stem().unwrap().to_str().unwrap(), typed_hir);
        }
    }
    Ok(())
}
