use std::{
    fs::File,
    io::{BufRead, BufReader},
    path::PathBuf,
};

use akynoc::{
    ast::Item,
    lexer::{lex, SourceFile, Token},
    parser,
};
use anyhow::Result;
use ariadne::FileCache;
use chumsky::Parser;
use insta::assert_ron_snapshot;
use rstest::rstest;
const LEXER_FAIL: u32 = 250;
const PARSER_FAIL: u32 = 245;
const SEM_FAIL: u32 = 240;
const PASS: u32 = 0;

macro_rules! set_snapshot_suffix {
    ($($expr:expr),*) => {
        let mut settings = insta::Settings::clone_current();
        settings.set_snapshot_suffix(format!($($expr,)*));
        let _guard = settings.bind_to_scope();
    }
}

fn test_lexer<'a>(src: &'a SourceFile) -> (u32, Vec<Token<'a>>) {
    let tokens = lex(src).map(|(t, _)| t).collect::<Vec<_>>();
    let rv = tokens.clone();
    for tok in tokens {
        match tok {
            Token::Invalid => return (LEXER_FAIL, rv),
            _ => (),
        }
    }
    return (PASS, rv);
}

fn test_parser<'a>(src: &'a SourceFile) -> (u32, Vec<Item>) {
    let token_stream = parser::token_stream(&src);
    let parser = parser::parser();
    match parser.parse(token_stream).into_result() {
        Ok(ast) => (PASS, ast),
        Err(errs) => (PARSER_FAIL, vec![]),
    }
}

// fn test_sem(path: &Path) -> Result<(u32, HirPool<TypedHir>)> {
//     let mut sem_analysis = SemanticAnalysis::from_path(path)?;
//     let typed_hir = sem_analysis.analyse();
//     match typed_hir {
//         Ok(typed_hir) => Ok((PASS, typed_hir)),
//         Err(_) => Ok((SEM_FAIL, HirPool::new())),
//     }
// }

#[rstest]
fn test(
    #[base_dir = "tests/resources/source/"]
    #[files("*.akn")]
    path: PathBuf,
) -> Result<()> {
    let mut reader = BufReader::new(File::open(&path)?);
    let mut line = String::new();
    let cache = FileCache::default();
    let src = SourceFile::from_path(path.as_path(), cache);
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

    let (lexer_actual, tokens) = test_lexer(&src);
    assert_eq!(lexer_expected, lexer_actual);
    if lexer_expected == PASS {
        set_snapshot_suffix!("lexer");
        assert_ron_snapshot!(path.file_stem().unwrap().to_str().unwrap(), tokens);
    }

    if expected_exit_code < LEXER_FAIL {
        let (parser_actual, ast) = test_parser(&src);
        assert_eq!(parser_expected, parser_actual);
        if parser_expected == PASS {
            set_snapshot_suffix!("parser");
            assert_ron_snapshot!(path.file_stem().unwrap().to_str().unwrap(), ast);
        }
    }
    Ok(())
}
