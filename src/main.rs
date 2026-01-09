use std::path::Path;

use anyhow::Result;
use toyc::{
    lexer::{Category, Tokeniser},
    parser::Parser,
    util::{CompilerPass, Writable, Writer},
};

const PARSER_FAIL: i32 = 245;

pub fn main() -> Result<()> {
    let args: Vec<String> = std::env::args().collect();
    let mut tokeniser = Tokeniser::from_path(Path::new(&args[2]))?;

    if args[1] == "-lexer" {
        let mut t = tokeniser.next_token()?;
        while t.category() != Category::Eof {
            println!("{}", t);
            t = tokeniser.next_token()?;
        }
    } else if args[1] == "-parser" {
        let mut parser = Parser::with_tokeniser(tokeniser)?;
        let program = parser.parse()?;
        if parser.has_error() {
            std::process::exit(PARSER_FAIL);
        }
        let mut out = std::io::stdout();
        let mut writer = Writer::new(&mut out);
        program.write(&mut writer)?;
    } else {
        usage();
    }
    Ok(())
}

fn usage() -> ! {
    println!("Usage: toycc <pass> <file>");
    println!("Available passes: -lexer, -parser");
    std::process::exit(-1);
}
