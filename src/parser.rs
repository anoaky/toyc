use anyhow::{Ok, Result, bail};
use std::cell::RefCell;
use std::collections::VecDeque;
use std::rc::Rc;

use crate::ast::decl::{Decl, StructTypeDecl, VarDecl};
use crate::ast::program::Program;
use crate::ast::types::EType;
use crate::lexer::Category;
use crate::rc_ref;
use crate::{
    lexer::{Token, Tokeniser},
    util::CompilerPass,
};

pub struct Parser {
    errors: u32,
    buffer: VecDeque<Token>,
    tokeniser: Tokeniser,
    token: Token,
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
    pub fn with_tokeniser(mut tokeniser: Tokeniser) -> Result<Self> {
        let first_token = tokeniser.next_token()?;
        Ok(Self {
            errors: 0,
            buffer: VecDeque::new(),
            tokeniser,
            token: first_token,
            last_error_token: None,
        })
    }

    pub fn parse(&mut self) -> Result<Program> {
        self.parse_program()
    }

    fn accept(&self, expected: Vec<Category>) -> bool {
        expected.contains(&self.token.category())
    }

    fn convert_token(&self, t: Token) -> EType {
        use EType::*;
        match t.category() {
            Category::Int => Int,
            Category::Char => Char,
            Category::Void => Void,
            _ => Unknown,
        }
    }

    fn error(&mut self, expected: Vec<Category>) {
        if self
            .last_error_token
            .clone()
            .is_some_and(|t| t == self.token)
        {
            return;
        }
        println!(
            "Parsing error: expected {:?} found {:?} at {:?}",
            expected,
            self.token.category(),
            self.token.position
        );
        self.inc_error();
        self.last_error_token = Some(self.token.clone());
    }

    fn expect(&mut self, expected: Vec<Category>) -> Result<Token> {
        let token = self.token.clone();
        for e in expected.clone() {
            if e == token.category() {
                self.next_token()?;
                return Ok(token);
            }
        }
        self.error(expected);
        Ok(token)
    }

    fn is_fun(&mut self) -> Result<bool> {
        use Category::*;
        if !self.accept(vec![Struct, Int, Char, Void]) {
            return Ok(false);
        }

        let mut k = if self.accept(vec![Struct]) { 2 } else { 1 };

        while self.look_ahead(k)?.category() == Asterisk {
            k += 1;
        }

        Ok(self.look_ahead(k + 1)?.category() == LPar)
    }

    fn next_token(&mut self) -> Result<()> {
        if !self.buffer.is_empty() {
            self.token = self.buffer.pop_front().unwrap();
            Ok(())
        } else {
            match self.tokeniser.next_token() {
                std::result::Result::Ok(t) => {
                    self.token = t;
                    Ok(())
                }
                Err(_) => bail!("Error reading next token"),
            }
        }
    }

    fn load_buffer(&mut self) -> Result<()> {
        match self.tokeniser.next_token() {
            std::result::Result::Ok(t) => {
                self.buffer.push_back(t);
                Ok(())
            }
            Err(_) => bail!("Error reading next token"),
        }
    }

    fn look_ahead(&mut self, i: usize) -> Result<Token> {
        while self.buffer.len() < i {
            self.load_buffer()?;
        }

        match self.buffer.get(i - 1) {
            None => bail!("Failed look ahead"),
            Some(t) => Ok(t.clone()),
        }
    }

    fn parse_includes(&mut self) -> Result<()> {
        use Category::*;
        if self.accept(vec![Include]) {
            self.next_token()?;
            self.expect(vec![StrLiteral])?;
            self.parse_includes()
        } else {
            Ok(())
        }
    }

    fn parse_program(&mut self) -> Result<Program> {
        use Category::*;
        self.parse_includes()?;
        let mut decls: Vec<Rc<RefCell<dyn Decl>>> = vec![];

        while self.accept(vec![Struct, Int, Char, Void]) {
            if self.token.category() == Struct
                && self.look_ahead(1)?.category() == Identifier
                && self.look_ahead(2)?.category() == LBrace
            {
                decls.push(rc_ref!(self.parse_struct_decl()?));
            } else if self.is_fun()? {
                // parse fun decl / defn
                panic!("No functions!!");
            } else {
                decls.push(rc_ref!(self.parse_var_decl()?));
                self.expect(vec![Semi])?;
            }
        }

        self.expect(vec![Eof])?;
        Ok(Program::new(decls))
    }

    fn parse_struct_decl(&mut self) -> Result<StructTypeDecl> {
        use Category::*;
        let ty = self.parse_struct_type()?;
        let mut decl = StructTypeDecl::new(ty);
        self.expect(vec![LBrace])?;
        loop {
            decl.add_var_decl(self.parse_var_decl()?);
            self.expect(vec![Semi])?;
            if !self.accept(vec![Int, Char, Void, Struct]) {
                break;
            }
        }
        self.expect(vec![RBrace])?;
        self.expect(vec![Semi])?;
        Ok(decl)
    }

    fn parse_struct_type(&mut self) -> Result<EType> {
        use Category::*;
        self.expect(vec![Struct])?;
        let id = self.expect(vec![Identifier])?;
        Ok(EType::Struct(RefCell::new(0), id.data))
    }

    fn parse_types(&mut self) -> Result<EType> {
        use Category::*;
        let mut ty: EType = if self.token.category() == Struct {
            self.parse_struct_type()?
        } else {
            let t = self.expect(vec![Int, Char, Void])?;
            self.convert_token(t)
        };
        while self.accept(vec![Asterisk]) {
            ty = EType::Pointer(rc_ref!(ty));
            self.next_token()?;
        }
        Ok(ty)
    }

    fn parse_var_decl(&mut self) -> Result<VarDecl> {
        use Category::*;
        let mut ty = self.parse_types()?;
        let id = self.expect(vec![Identifier])?;
        let mut lens: VecDeque<usize> = VecDeque::new();
        while self.accept(vec![LBrack]) {
            self.expect(vec![LBrack])?;
            let i = self.expect(vec![IntLiteral])?;
            self.expect(vec![RBrack])?;
            if i.category() == IntLiteral {
                lens.push_front(i.data.parse::<usize>()?);
            }
        }
        while !lens.is_empty() {
            ty = EType::Array(
                lens.pop_front()
                    .expect("Failed to pop non-empty VecDeque ??"),
                rc_ref!(ty),
            );
        }

        Ok(VarDecl::new(ty, id.data))
    }
}
