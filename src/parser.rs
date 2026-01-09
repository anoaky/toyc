use std::collections::VecDeque;

use anyhow::{bail, Ok, Result};

use crate::{
    ast::{Ast, DeclKind, ExprKind, Literal, OpKind, StmtKind, Type},
    lexer::{Category, Token, Tokeniser},
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

/*
    precedence:
    =                                   (2, 1)
    ||                                  (3, 4)
    &&                                  (5, 6)
    ==  !=                              (7, 8)
    <   <=  >   >=                      (9, 10)
    +   -                               (11, 12)
    *   /   %                           (13, 14)
    &val    *ptr    (type)    -x    +x  ((), 15)
    ()      []      .                   (17, 18)
*/

fn infix_precedence(category: Category) -> (u8, u8) {
    use Category::*;
    match category {
        Assign => (2, 1),
        Lt | Le | Gt | Ge => (9, 10),
        Plus | Minus => (11, 12),
        Asterisk | Div | Rem => (13, 14),
        _ => panic!("No precedence for category {:?}", category),
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

    pub fn parse(&mut self) -> Result<Ast> {
        self.parse_program()
    }

    fn accept(&self, expected: Category) -> bool {
        self.token.category() == expected
    }

    fn accept_any(&self, expected: Vec<Category>) -> bool {
        expected.contains(&self.token.category())
    }

    fn convert_token(&self, t: Token) -> Type {
        match t.category() {
            Category::Int => Type::Int,
            Category::Char => Type::Char,
            Category::Void => Type::Void,
            _ => Type::Unknown,
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

    fn expect(&mut self, expected: Category) -> Result<Token> {
        self.expect_any(vec![expected])
    }

    fn expect_any(&mut self, expected: Vec<Category>) -> Result<Token> {
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
        if !self.accept_any(vec![Struct, Int, Char, Void]) {
            return Ok(false);
        }

        let mut k = if self.accept_any(vec![Struct]) { 2 } else { 1 };

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

    fn parse_atom(&mut self) -> Result<ExprKind> {
        use Category::*;
        let expr = match self.token.category() {
            LPar => unimplemented!(),
            IntLiteral => {
                let l = self.expect(IntLiteral)?;
                ExprKind::Literal(Literal::Int(l.data.parse()?))
            }
            Identifier => {
                let id = self.expect(Identifier)?;
                ExprKind::VarExpr(id.data)
            }
            _ => {
                self.expect_any(vec![
                    LPar,
                    IntLiteral,
                    CharLiteral,
                    StrLiteral,
                    Identifier,
                    Sizeof,
                ])?;
                ExprKind::InvalidExpr
            }
        };
        Ok(expr)
    }

    fn parse_block(&mut self) -> Result<Ast> {
        let mut vds = vec![];
        let mut stmts = vec![];
        self.expect(Category::LBrace)?;
        while self.accept_any(vec![
            Category::Int,
            Category::Char,
            Category::Void,
            Category::Struct,
        ]) {
            vds.push(self.parse_var_decl()?);
            self.expect(Category::Semi)?;
        }
        while !self.accept_any(vec![Category::RBrace, Category::Eof]) {
            stmts.push(self.parse_stmt()?);
        }
        self.expect(Category::RBrace)?;
        let block = StmtKind::Block { decls: vds, stmts };
        Ok(Ast::Stmt(block))
    }

    fn parse_expr(&mut self, min_precedence: u8) -> Result<ExprKind> {
        let mut lhs = self.parse_atom()?;
        loop {
            use Category::*;
            if !self.accept_any(vec![
                Plus, Minus, Assign, Asterisk, Div, Rem, Lt, Le, Gt, Ge,
            ]) {
                break;
            }
            let op = self.token.category();

            let (lprec, rprec) = infix_precedence(op);
            if lprec < min_precedence {
                break;
            }
            self.next_token()?;
            let rhs = self.parse_expr(rprec)?;
            lhs = if op == Assign {
                ExprKind::Assign(Box::new(lhs), Box::new(rhs))
            } else {
                let op = match op {
                    Plus => OpKind::Add,
                    Minus => OpKind::Sub,
                    Asterisk => OpKind::Mul,
                    Div => OpKind::Div,
                    Rem => OpKind::Mod,
                    Lt => OpKind::Lt,
                    Le => OpKind::Le,
                    Gt => OpKind::Gt,
                    Ge => OpKind::Ge,
                    _ => unreachable!(),
                };
                ExprKind::BinOp(Box::new(lhs), op, Box::new(rhs))
            }
        }
        Ok(lhs)
    }

    fn parse_fun_decl(&mut self) -> Result<Ast> {
        let return_type = self.parse_types()?;
        let id = self.expect_any(vec![Category::Identifier])?;
        self.expect_any(vec![Category::LPar])?;
        let params = if !self.accept_any(vec![Category::RPar]) {
            self.parse_params()?
        } else {
            vec![]
        };
        self.expect(Category::RPar)?;
        Ok(Ast::Decl(DeclKind::FunDecl(return_type, id.data, params)))
    }

    fn parse_includes(&mut self) -> Result<()> {
        use Category::*;
        if self.accept_any(vec![Include]) {
            self.next_token()?;
            self.expect_any(vec![StrLiteral])?;
            self.parse_includes()
        } else {
            Ok(())
        }
    }

    fn parse_params(&mut self) -> Result<Vec<Ast>> {
        let mut params = vec![];
        params.push(self.parse_var_decl()?);
        while self.accept_any(vec![Category::Comma]) {
            self.next_token()?;
            params.push(self.parse_var_decl()?);
        }
        Ok(params)
    }

    fn parse_program(&mut self) -> Result<Ast> {
        use Category::*;
        self.parse_includes()?;
        let mut prog = vec![];

        while self.accept_any(vec![Struct, Int, Char, Void]) {
            if self.token.category() == Struct
                && self.look_ahead(1)?.category() == Identifier
                && self.look_ahead(2)?.category() == LBrace
            {
                prog.push(self.parse_struct_decl()?);
            } else if self.is_fun()? {
                let decl = {
                    let decl = self.parse_fun_decl()?;
                    if self.accept(LBrace) {
                        let block = Box::new(self.parse_block()?);
                        Ast::Decl(DeclKind::FunDefn {
                            decl: Box::new(decl),
                            block,
                        })
                    } else if self.accept(Semi) {
                        self.next_token()?;
                        decl
                    } else {
                        self.expect_any(vec![Semi, LBrace])?;
                        decl
                    }
                };
                prog.push(decl);
            } else {
                prog.push(self.parse_var_decl()?);
                self.expect_any(vec![Semi])?;
            }
        }

        self.expect_any(vec![Eof])?;
        Ok(Ast::Program(prog))
    }

    fn parse_stmt(&mut self) -> Result<Ast> {
        use Category::*;
        match self.token.category() {
            LBrace => self.parse_block(),
            While => {
                self.next_token()?;
                self.expect(LPar)?;
                let e = Ast::Expr(self.parse_expr(0)?);
                self.expect(RPar)?;
                let s = self.parse_stmt()?;
                let whl = StmtKind::While {
                    expr: Box::new(e),
                    stmt: Box::new(s),
                };
                Ok(Ast::Stmt(whl))
            }
            If => {
                self.next_token()?;
                self.expect(LPar)?;
                let expr = Box::new(Ast::Expr(self.parse_expr(0)?));
                self.expect(RPar)?;
                let then = Box::new(self.parse_stmt()?);
                let els = if self.accept(Else) {
                    self.next_token()?;
                    Some(Box::new(self.parse_stmt()?))
                } else {
                    None
                };
                let stmt = StmtKind::If { expr, then, els };
                Ok(Ast::Stmt(stmt))
            }
            Return => {
                self.next_token()?;
                let rv = if !self.accept(Semi) {
                    Some(Box::new(Ast::Expr(self.parse_expr(0)?)))
                } else {
                    None
                };
                self.expect(Semi)?;
                let ret = StmtKind::Return(rv);
                Ok(Ast::Stmt(ret))
            }
            Continue => {
                self.next_token()?;
                self.expect(Semi)?;
                Ok(Ast::Stmt(StmtKind::Continue))
            }
            Break => {
                self.next_token()?;
                self.expect(Semi)?;
                Ok(Ast::Stmt(StmtKind::Break))
            }
            _ => {
                let expr = Box::new(Ast::Expr(self.parse_expr(0)?));
                while self.token.category() != Semi && self.token.category() != Eof {
                    self.expect(Semi)?;
                    self.next_token()?;
                }
                self.expect(Semi)?;
                Ok(Ast::Stmt(StmtKind::ExprStmt(expr)))
            }
        }
    }

    fn parse_struct_decl(&mut self) -> Result<Ast> {
        use Category::*;
        let ty = self.parse_struct_type()?;
        let name = match &ty {
            Type::Struct(s) => s.clone(),
            _ => panic!("Expected struct type for struct"),
        };
        let mut fields: Vec<Ast> = vec![];
        self.expect_any(vec![LBrace])?;
        loop {
            fields.push(self.parse_var_decl()?);
            self.expect_any(vec![Semi])?;
            if !self.accept_any(vec![Int, Char, Void, Struct]) {
                break;
            }
        }
        self.expect_any(vec![RBrace])?;
        self.expect_any(vec![Semi])?;
        let decl = DeclKind::StructTypeDecl(ty, name, fields);
        Ok(Ast::Decl(decl))
    }

    fn parse_struct_type(&mut self) -> Result<Type> {
        use Category::*;
        self.expect_any(vec![Struct])?;
        let id = self.expect_any(vec![Identifier])?;
        let ty = Type::Struct(id.data.clone());
        Ok(ty)
    }

    fn parse_types(&mut self) -> Result<Type> {
        use Category::*;
        let mut ty = if self.token.category() == Struct {
            self.parse_struct_type()?
        } else {
            let t = self.expect_any(vec![Int, Char, Void])?;
            self.convert_token(t)
        };
        while self.accept_any(vec![Asterisk]) {
            ty = Type::Pointer(Box::new(ty));
            self.next_token()?;
        }
        Ok(ty)
    }

    fn parse_var_decl(&mut self) -> Result<Ast> {
        use Category::*;
        let mut ty = self.parse_types()?;
        let id = self.expect_any(vec![Identifier])?;
        let mut lens: VecDeque<usize> = VecDeque::new();
        while self.accept_any(vec![LBrack]) {
            self.expect_any(vec![LBrack])?;
            let i = self.expect_any(vec![IntLiteral])?;
            self.expect_any(vec![RBrack])?;
            if i.category() == IntLiteral {
                lens.push_front(i.data.parse::<usize>()?);
            }
        }
        while !lens.is_empty() {
            ty = Type::Array(
                lens.pop_front()
                    .expect("Failed to pop non-empty VecDeque ??"),
                Box::new(ty),
            );
        }

        Ok(Ast::Decl(DeclKind::VarDecl(ty, id.data)))
    }
}
