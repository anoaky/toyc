use core::fmt;
use std::{
    fs::File,
    io::{BufReader, ErrorKind, Read, Result},
};

pub struct Tokeniser {
    reader: Reader,
}

impl Tokeniser {
    pub fn from_path(fp: &str) -> Result<Self> {
        let f = File::open(fp)?;
        let reader = Reader::from_file(f)?;
        Ok(Self { reader })
    }

    fn invalid(&self, c: char, line: u32, col: u32) -> Result<Token> {
        println!(
            "Lexing error: unrecognised character {} at {}:{}",
            c, line, col
        );
        Ok(Token::new(Category::Invalid, None, line, col))
    }

    fn try_read_int(&mut self, data: String) -> Result<Token> {
        if !self.reader.peek().is_ascii_digit() {
            Ok(Token::new(
                Category::IntLiteral,
                Some(data),
                self.reader.line,
                self.reader.col,
            ))
        } else {
            let mut data = data.clone();
            data.push(self.reader.next()?);
            self.try_read_int(data)
        }
    }

    fn try_read_keyword(&mut self, data: String, keywords: Vec<String>) -> Result<Token> {
        use Category::*;
        let filtered: Vec<String> = keywords
            .iter()
            .filter(|&s| s.len() >= data.len() && s.starts_with(&data))
            .cloned()
            .collect();
        if !self.reader.has_next() || !is_valid_ident(self.reader.peek()) {
            let category = match data.as_str() {
                "int" => Int,
                "void" => Void,
                "char" => Char,
                "if" => If,
                "else" => Else,
                "while" => While,
                "return" => Return,
                "struct" => Struct,
                "sizeof" => Sizeof,
                "continue" => Continue,
                "break" => Break,
                "include" => Include,
                _ => Identifier,
            };
            if category == Identifier {
                Ok(Token::new(
                    category,
                    Some(data),
                    self.reader.line,
                    self.reader.col,
                ))
            } else {
                Ok(Token::blank(category, self.reader.line, self.reader.col))
            }
        } else {
            let mut new_data = data.clone();
            new_data.push(self.reader.next()?);
            self.try_read_keyword(new_data, filtered)
        }
    }

    pub fn next_token(&mut self) -> Result<Token> {
        use Category::*;
        let line = self.reader.line;
        let col = self.reader.col;
        if self.reader.has_next() {
            let c = self.reader.next()?;

            let tok = match c {
                '{' => Token::blank(LBrace, line, col),
                '}' => Token::blank(RBrace, line, col),
                '(' => Token::blank(LPar, line, col),
                ')' => Token::blank(RPar, line, col),
                '[' => Token::blank(LBrack, line, col),
                ']' => Token::blank(RBrack, line, col),
                ';' => Token::blank(Semi, line, col),
                ',' => Token::blank(Comma, line, col),
                '+' => Token::blank(Plus, line, col),
                '-' => Token::blank(Minus, line, col),
                '%' => Token::blank(Rem, line, col),
                '*' => Token::blank(Asterisk, line, col),
                '.' => Token::blank(Dot, line, col),
                '=' => {
                    if self.reader.peek() == '=' {
                        self.reader.next()?;
                        Token::blank(Eq, line, col)
                    } else {
                        Token::blank(Assign, line, col)
                    }
                }
                '&' => {
                    if self.reader.peek() == '&' {
                        self.reader.next()?;
                        Token::blank(LogAnd, line, col)
                    } else {
                        Token::blank(And, line, col)
                    }
                }
                '|' => {
                    if self.reader.peek() == '|' {
                        self.reader.next()?;
                        Token::blank(LogOr, line, col)
                    } else {
                        self.invalid(c, line, col)?
                    }
                }
                '!' => {
                    if self.reader.peek() == '=' {
                        self.reader.next()?;
                        Token::blank(Ne, line, col)
                    } else {
                        self.invalid(c, line, col)?
                    }
                }
                '<' => {
                    if self.reader.peek() == '=' {
                        self.reader.next()?;
                        Token::blank(Le, line, col)
                    } else {
                        Token::blank(Lt, line, col)
                    }
                }
                '>' => {
                    if self.reader.peek() == '=' {
                        self.reader.next()?;
                        Token::blank(Ge, line, col)
                    } else {
                        Token::blank(Gt, line, col)
                    }
                }
                _ if c.is_ascii_whitespace() => self.next_token()?,
                _ if is_valid_ident_start(c) => self.try_read_keyword(
                    c.to_string(),
                    vec![
                        "if".to_string(),
                        "int".to_string(),
                        "void".to_string(),
                        "char".to_string(),
                        "else".to_string(),
                        "while".to_string(),
                        "return".to_string(),
                        "struct".to_string(),
                        "sizeof".to_string(),
                        "continue".to_string(),
                        "break".to_string(),
                    ],
                )?,
                _ if c.is_ascii_digit() => self.try_read_int(c.to_string())?,
                _ => self.invalid(c, line, col)?,
            };
            Ok(tok)
        } else {
            Ok(Token::new(Category::Eof, None, line, col))
        }
    }
}

fn is_valid_ident_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_valid_ident(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Category {
    Identifier,
    Assign,
    LBrace,
    RBrace,
    LPar,
    RPar,
    LBrack,
    RBrack,
    Semi,
    Comma,
    Int,
    Void,
    Char,
    If,
    Else,
    While,
    Return,
    Struct,
    Sizeof,
    Continue,
    Break,
    Include,
    CharLiteral,
    StrLiteral,
    IntLiteral,
    LogAnd,
    LogOr,
    Eq,
    Ne,
    Gt,
    Lt,
    Le,
    Ge,
    Plus,
    Minus,
    Asterisk,
    Div,
    Rem,
    And,
    Dot,
    Eof,
    Invalid,
}

#[derive(Clone)]
pub struct Token {
    category: Category,
    data: String,
    position: (u32, u32), // line, col
}

impl Token {
    pub fn new(category: Category, data: Option<String>, line: u32, col: u32) -> Self {
        match data {
            Some(s) => Self {
                category,
                data: s,
                position: (line, col),
            },
            None => Self {
                category,
                data: "".to_string(),
                position: (line, col),
            },
        }
    }

    pub fn blank(category: Category, line: u32, col: u32) -> Self {
        Self::new(category, None, line, col)
    }

    pub fn category(&self) -> Category {
        self.category
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.data.is_empty() {
            write!(f, "{:?}", self.category)
        } else {
            write!(f, "{:?}({})", self.category, self.data)
        }
    }
}

struct Reader {
    input: BufReader<File>,
    buf: [u8; 2],
    pub line: u32,
    pub col: u32,
}

impl Reader {
    pub fn from_file(f: File) -> Result<Self> {
        let mut input = BufReader::new(f);
        let mut buf = [0, 0]; // TODO: rework eventually for slight memory efficiency gains; two `u8`s are unnecessary here
        input.read_exact(&mut buf[1..])?;
        Ok(Self {
            input,
            buf,
            line: 1,
            col: 1,
        })
    }

    pub fn next(&mut self) -> Result<char> {
        self.buf[0] = self.buf[1];
        match self.input.read_exact(&mut self.buf[1..]) {
            Ok(()) => (),
            Err(err) if err.kind() == ErrorKind::UnexpectedEof => self.buf[1] = 0, // end of file
            Err(err) => return Err(err),
        }
        if self.buf[0] as char == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        Ok(self.buf[0] as char)
    }

    pub fn peek(&self) -> char {
        self.buf[1] as char
    }

    pub fn has_next(&self) -> bool {
        self.buf[1] != 0
    }

    pub fn pos(&self) -> (u32, u32) {
        (self.line, self.col)
    }
}

#[cfg(test)]
mod test {
    use crate::lexer::Reader;
    use std::{fs::File, io::Result};

    #[test]
    fn check_next() -> Result<()> {
        let mut reader = Reader::from_file(File::open("tests/src/decls.tc")?)?;
        assert_eq!(reader.next()?, 'i');
        Ok(())
    }

    #[test]
    fn check_peek() -> Result<()> {
        let mut reader = Reader::from_file(File::open("tests/src/decls.tc")?)?;
        assert_eq!(reader.peek(), 'i');
        assert_eq!(reader.next()?, 'i');
        assert_eq!(reader.peek(), 'n');
        Ok(())
    }

    #[test]
    fn check_pos() -> Result<()> {
        let mut reader = Reader::from_file(File::open("tests/src/decls.tc")?)?;
        assert_eq!(reader.pos(), (1, 1));
        reader.next()?;
        assert_eq!(reader.pos(), (1, 2));
        while reader.next()? != ';' {}
        assert_eq!(reader.pos(), (1, 7));
        reader.next()?;
        assert_eq!(reader.pos(), (2, 1));
        Ok(())
    }
}
