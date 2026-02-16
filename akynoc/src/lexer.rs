use std::{fmt::Display, path::Path};

use ariadne::{Cache, FileCache, Report, Source};
use chumsky::span::SimpleSpan;
use logos::Logos;
use serde::Serialize;

#[derive(Logos, Clone, Debug, PartialEq, Serialize)]
#[logos(skip r#"\s+"#)]
#[logos(skip r#"//([^\n]+)?\n?"#)]
#[logos(skip r#"/\*[^*]*\*+([^/*][^*]*\*+)*/"#)]
#[logos(subpattern alpha = r#"[a-zA-Z]"#)]
#[logos(subpattern num = r#"[0-9]"#)]
#[logos(subpattern alphanum = r#"(?&alpha)|(?&num)"#)]
#[logos(utf8 = true)]
#[logos(subpattern special = r#"[\x20\x21\x23-\x26\x28-\x2F\x3A-\x40\x5B\x5D-\x60\x7B-\x7E]"#)]
pub enum Token<'a> {
    #[regex(r#"([a-zA-Z][_a-zA-Z0-9]*|[_][_a-zA-Z0-9]+)"#)]
    Identifier(&'a str),
    #[token("=")]
    Assign,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("(")]
    LPar,
    #[token(")")]
    RPar,
    #[token("[")]
    LBrack,
    #[token("]")]
    RBrack,
    #[token(";")]
    Semi,
    #[token(":")]
    Colon,
    #[token("_")]
    Underscore,
    #[token(",")]
    Comma,
    #[token("int")]
    Int,
    #[token("void")]
    Void,
    #[token("char")]
    Char,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("while")]
    While,
    #[token("for")]
    For,
    #[token("return")]
    Return,
    #[token("struct")]
    Struct,
    #[token("continue")]
    Continue,
    #[token("break")]
    Break,
    #[token("static")]
    Static,
    #[regex(r#"'((?&alphanum)|(?&special)|"| |\\(["'ntr]|x[0-9]{2}))'"#)]
    CharLiteral(&'a str),
    #[regex(r#""((?&alphanum)|(?&special)|'| |\\(["'ntr]|x[0-9]{2}))*""#)]
    StrLiteral(&'a str),
    #[regex(r#"[0-9]+"#)]
    IntLiteral(&'a str),
    #[token("&&")]
    LogAnd,
    #[token("||")]
    LogOr,
    #[token("==")]
    Eq,
    #[token("!=")]
    Ne,
    #[token(">")]
    Gt,
    #[token("<")]
    Lt,
    #[token("<=")]
    Le,
    #[token(">=")]
    Ge,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token(r"*")]
    Asterisk,
    #[token(r"/")]
    Div,
    #[token("%")]
    Rem,
    #[token("&")]
    And,
    #[token(".")]
    Dot,
    #[token("=>")]
    FatArrow,
    #[token("let")]
    Let,
    #[token("fn")]
    Fn,
    #[token(":=")]
    Define,
    Invalid,
}

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Token::*;
        match self {
            Identifier(s) => write!(f, "{s}"),
            Assign => write!(f, "="),
            LBrace => write!(f, "{{"),
            RBrace => write!(f, "}}"),
            LPar => write!(f, "("),
            RPar => write!(f, ")"),
            RBrack => write!(f, "["),
            LBrack => write!(f, "]"),
            Semi => write!(f, ";"),
            Colon => write!(f, ":"),
            Underscore => write!(f, "_"),
            Comma => write!(f, ","),
            Int => write!(f, "int"),
            Void => write!(f, "void"),
            Char => write!(f, "char"),
            If => write!(f, "if"),
            Else => write!(f, "else"),
            While => write!(f, "while"),
            For => write!(f, "for"),
            Return => write!(f, "return"),
            Struct => write!(f, "struct"),
            Continue => write!(f, "continue"),
            Break => write!(f, "break"),
            Static => write!(f, "static"),
            CharLiteral(c) => write!(f, "{c}"),
            StrLiteral(s) => write!(f, "{s}"),
            IntLiteral(i) => write!(f, "{i}"),
            LogAnd => write!(f, "&&"),
            LogOr => write!(f, "||"),
            Eq => write!(f, "=="),
            Ne => write!(f, "!="),
            Gt => write!(f, ">"),
            Lt => write!(f, "<"),
            Le => write!(f, "<="),
            Ge => write!(f, ">="),
            Plus => write!(f, "+"),
            Minus => write!(f, "-"),
            Asterisk => write!(f, "*"),
            Div => write!(f, "/"),
            Rem => write!(f, "%"),
            And => write!(f, "&"),
            Dot => write!(f, "."),
            FatArrow => write!(f, "=>"),
            Let => write!(f, "let"),
            Fn => write!(f, "fn"),
            Define => write!(f, ":="),
            Invalid => write!(f, "INVALID"),
        }
    }
}

pub struct SourceFile {
    pub name: String,
    pub source: Source,
}

impl SourceFile {
    pub fn from_path(id: &Path, mut cache: FileCache) -> Self {
        let source = cache.fetch(id).unwrap().clone();
        Self {
            name: id.to_str().unwrap().to_string(),
            source,
        }
    }
}

pub fn lex<'src>(src: &'src SourceFile) -> impl std::iter::Iterator<Item = (Token<'src>, SimpleSpan)> {
    Token::lexer(src.source.text()).spanned().map(|(tok, span)| -> (Token<'src>, SimpleSpan) {
        match tok {
            Ok(t) => match t {
                Token::CharLiteral(c) => (Token::CharLiteral(c.strip_prefix("'").unwrap().strip_suffix("'").unwrap()), span.into()),
                Token::StrLiteral(s) => (Token::StrLiteral(s.strip_prefix("\"").unwrap().strip_suffix("\"").unwrap()), span.into()),
                t => (t, span.into()),
            },
            Err(()) => {
                let span = Into::<SimpleSpan>::into(span);
                (Token::Invalid, span)
            }
        }
    })
}

pub fn print_errors<'src, I>(src: &'src SourceFile, token_iter: I)
where
    I: std::iter::Iterator<Item = (Token<'src>, SimpleSpan)>,
{
    token_iter.for_each(|(tok, span)| {
        if tok == Token::Invalid {
            Report::build(ariadne::ReportKind::Error, (&src.name, span.into_range()))
                .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
                .with_code(250)
                .with_message("Lexing error")
                .with_label(
                    ariadne::Label::new((&src.name, span.into_range()))
                        .with_message("Unrecognized character")
                        .with_color(ariadne::Color::Red),
                )
                .finish()
                .eprint((&src.name, &src.source))
                .unwrap();
        }
    });
}

#[cfg(test)]
mod tests {
    use std::io::Write;

    use ariadne::FileCache;
    use rstest::{fixture, rstest};
    use tempfile::NamedTempFile;

    use crate::lexer::SourceFile;

    #[fixture]
    fn cache() -> FileCache {
        FileCache::default()
    }

    fn src(s: String, cache: FileCache) -> SourceFile {
        let mut f = NamedTempFile::new().unwrap();
        write!(f, "{}", s).unwrap();
        SourceFile::from_path(f.path(), cache)
    }

    #[rstest]
    #[case::plus("+", "+")]
    #[case::minus("-", "-")]
    #[case::a("'a'", "a")]
    #[case::newline(r"'\n'", r"\n")]
    #[should_panic]
    #[case::question_mark("int x?;", "")]
    #[case::main_defn("void main() {\n\treturn;\n}", "voidmain(){return;}")]
    #[case::hello_world("\"Hello, world!\"", "Hello, world!")]
    #[case::single_line_comment("// comment\nvoid main() {}", "voidmain(){}")]
    fn test_lexer(#[case] input: String, #[case] expected: String, cache: FileCache) {
        let src_file = src(input, cache);
        let token_iter = super::lex(&src_file);
        let recovered = token_iter
            .map(|(t, _)| {
                if t == super::Token::Invalid {
                    panic!("Invalid token");
                } else {
                    t.to_string()
                }
            })
            .collect::<String>();
        assert_eq!(recovered, expected);
    }
}
