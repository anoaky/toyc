#![doc = include_str!("../README.md")]
use std::path::Path;

use anyhow::Result;
use ariadne::FileCache;
use nockyc::lexer::{self, lex, SourceFile};

pub fn main() -> Result<()> {
    let args: Vec<String> = std::env::args().collect();
    let cache = FileCache::default();
    let src = SourceFile::from_path(Path::new(&args[2]), cache);

    if args[1] == "-lexer" {
        let token_iter = lex(&src);
        lexer::print_errors(&src, token_iter);
        // } else if args[1] == "-parser" {
        //     let program = parse(tokeniser)?;
        //     let mut out = BufWriter::new(stdout());
        //     let mut writer = Writer::new(&mut out);
        //     for decl in program {
        //         decl.write(&mut writer, true)?;
        //     }
        // } else if args[1] == "-sem" {
        //     let program = parse(tokeniser)?;
        //     let mut binder = Binder::new();
        //     let untyped_hir = binder.bind(program)?;
        //     println!("Name analysis successful");
        //     let mut typer = Typer::new(untyped_hir);
        //     typer.type_all()?;
        //     println!("Type analysis successful");
    } else {
        usage();
    }
    Ok(())
}

fn usage() -> ! {
    println!("Usage: nockyc <pass> <file>");
    println!("Available passes: -lexer, -parser");
    std::process::exit(-1);
}
