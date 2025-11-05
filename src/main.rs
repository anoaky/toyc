use std::io::Result;

use toyc::lexer::{Category, Token, Tokeniser};

pub fn main() -> Result<()> {
    let args: Vec<String> = std::env::args().collect();
    let mut tokeniser = Tokeniser::from_path(&args[2])?;

    if args[1] == "-lexer" {
        let mut t = tokeniser.next_token()?;
        while t.category() != Category::Eof {
            print!("{}", t);
            t = tokeniser.next_token()?;
        }
    }
    Ok(())
}

fn usage() -> ! {
    println!("Usage: toycc <pass> <file>");
    println!("Available passes: -lexer, -parser");
    std::process::exit(-1);
}
