#![doc = include_str!("../README.md")]
use std::path::PathBuf;

use akynoc::{
    lexer::{self, lex, SourceFile},
    parser::{self, parser},
};
use anyhow::Result;
use ariadne::FileCache;
use clap::{builder::PossibleValue, Parser, ValueEnum};

#[derive(Debug, Clone, Copy)]
enum Pass {
    All,
    Lexer,
    Parser,
}

#[derive(Parser, Debug)]
#[command(version, about)]
struct Args {
    /// The source .nky file to process
    file: PathBuf,

    /// Select a specific compiler pass to test
    #[arg(value_enum, short, long, default_value_t = Pass::All)]
    pass: Pass,
}

pub fn main() -> Result<()> {
    let args = Args::parse();
    let cache = FileCache::default();
    let src = SourceFile::from_path(args.file.as_path(), cache);

    match args.pass {
        Pass::All => unimplemented!(),
        Pass::Lexer => {
            let token_iter = lex(&src);
            lexer::print_errors(&src, token_iter);
        }
        Pass::Parser => {
            use chumsky::Parser;
            let token_stream = parser::token_stream(&src);
            let parser = parser();
            match parser.parse(token_stream).into_result() {
                Ok(_) => (),
                Err(errs) => {
                    parser::print_errors(&src.source, errs);
                    panic!("Parsing failed");
                }
            }
        }
    }
    Ok(())
}

impl ValueEnum for Pass {
    fn value_variants<'a>() -> &'a [Self] {
        &[Pass::Lexer, Pass::Parser, Pass::All]
    }
    fn to_possible_value(&self) -> Option<clap::builder::PossibleValue> {
        let s = match self {
            Self::All => "all",
            Self::Lexer => "lexer",
            Self::Parser => "parser",
        };
        Some(PossibleValue::new(s))
    }
}
