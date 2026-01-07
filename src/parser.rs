use std::collections::VecDeque;
use std::result::{
    Result,
    Result::{Err, Ok},
};

use crate::{
    lexer::{Token, Tokeniser},
    util::CompilerPass,
};

pub struct Parser {
    errors: u32,
    buffer: VecDeque<Token>,
    tokeniser: Tokeniser,
    token: Option<Token>,
    last_error_token: Option<Token>,
}

impl CompilerPass for Parser {
    fn inc_error(&mut self) {
        self.errors += 1;
    }

    fn num_errors(&self) -> u32 {
        self.errors
    }
}

impl Parser {
    pub fn with_tokeniser(tokeniser: Tokeniser) -> Self {
        Self {
            errors: 0,
            buffer: VecDeque::new(),
            tokeniser,
            token: None,
            last_error_token: None,
        }
    }

    fn next_token(&mut self) -> Result<(), &'static str> {
        if !self.buffer.is_empty() {
            self.token = self.buffer.pop_front();
            Ok(())
        } else {
            match self.tokeniser.next_token() {
                Ok(t) => {
                    self.token = Some(t);
                    Ok(())
                }
                Err(_) => Err("Error reading next token"),
            }
        }
    }

    fn load_buffer(&mut self) -> Result<(), &'static str> {
        match self.tokeniser.next_token() {
            Ok(t) => {
                self.buffer.push_back(t);
                Ok(())
            }
            Err(_) => Err("Error reading next token"),
        }
    }

    fn look_ahead(&mut self, i: usize) -> Result<Token, &'static str> {
        while self.buffer.len() < i {
            self.load_buffer()?;
        }

        match self.buffer.get(i) {
            None => Err("Failed look ahead"),
            Some(t) => Ok(t.clone()),
        }
    }
}
