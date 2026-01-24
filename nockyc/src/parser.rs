//! Parses [`Tokens`](Token) into [`Items`](Item).
//!
#![doc = include_str!("../../grammars/parser_grammar.md")]
use chumsky::{
    extra::Err,
    input::{MapExtra, Stream, ValueInput},
    pratt::{self, *},
    prelude::*,
};

use crate::{
    ast::{
        exprs::{Expr, ExprKind, Literal, Operator},
        statements::{Stmt, StmtKind},
        types::{Ident, Primitive, Ty, TyKind},
        Item, ItemKind,
    },
    lexer::{lex, SourceFile, Token},
};

type Extras<'tok, 'src> = Err<Rich<'tok, Token<'src>>>;

pub fn print_errors<'tok, 'src: 'tok>(
    source: &ariadne::Source,
    errs: Vec<Rich<'tok, Token<'src>>>,
) {
    for err in errs {
        let reason = err.reason().clone().map_token(|t| t.to_string());
        ariadne::Report::build(ariadne::ReportKind::Error, ((), err.span().into_range()))
            .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
            .with_code(245)
            .with_message(err.to_string())
            .with_label(
                ariadne::Label::new(((), err.span().into_range()))
                    .with_message(reason.to_string())
                    .with_color(ariadne::Color::Red),
            )
            .finish()
            .eprint(source)
            .unwrap();
    }
}

fn ident<'tok, 'src: 'tok, I>() -> impl Parser<'tok, I, Ident, Extras<'tok, 'src>>
where
    I: ValueInput<'tok, Span = SimpleSpan, Token = Token<'src>>,
{
    select! {
        Token::Identifier(s) => Into::<Ident>::into(s)
    }
}

fn literal<'tok, 'src: 'tok, I>() -> impl Parser<'tok, I, Literal, Extras<'tok, 'src>>
where
    I: ValueInput<'tok, Span = SimpleSpan, Token = Token<'src>>,
{
    select! {
        Token::IntLiteral(i) => i.parse::<u32>().unwrap().into(),Token::CharLiteral(c) => any::<&str,Err<chumsky::error::EmptyErr>>().parse(c).into_result().unwrap().into(),Token::StrLiteral(s) => s.into()
    }
}

fn expr<'tok, 'src: 'tok, I>() -> impl Parser<'tok, I, Expr, Extras<'tok, 'src>>
where
    I: ValueInput<'tok, Span = SimpleSpan, Token = Token<'src>>,
{
    recursive(|expr| {
        let atom = choice((
            literal().boxed().map(|l| l.into()),
            ident().boxed().map(|id| id.into()),
        ));
        let expr_pratt = atom.clone().pratt((
            infix(right(1), just(Token::Assign), |lhs, _, rhs, _| {
                ExprKind::Assign(Box::new(lhs), Box::new(rhs)).into()
            }),
            infix(left(3), just(Token::LogOr), |lhs, _, rhs, _| {
                ExprKind::BinOp(Box::new(lhs), Operator::Or, Box::new(rhs)).into()
            }),
            infix(left(5), just(Token::LogAnd), |lhs, _, rhs, _| {
                ExprKind::BinOp(Box::new(lhs), Operator::And, Box::new(rhs)).into()
            }),
        ));
        choice((
            expr.delimited_by(just(Token::LPar), just(Token::RPar)),
            expr_pratt,
            atom,
        ))
    })
}

fn static_var<'tok, 'src: 'tok, I>() -> impl Parser<'tok, I, Item, Extras<'tok, 'src>>
where
    I: ValueInput<'tok, Span = SimpleSpan, Token = Token<'src>>,
{
    just(Token::Static)
        .ignore_then(ident())
        .then_ignore(just(Token::Colon))
        .then(
            parse_type_strict()
                .or_not()
                .map(|ty| ty.unwrap_or(TyKind::Infer.into())),
        )
        .then(just(Token::Assign).ignore_then(literal()).or_not())
        .then_ignore(just(Token::Semi))
        .map(|((ident, ty), value)| ItemKind::Static((ident, ty, value).into()).into())
}

fn item<'tok, 'src: 'tok, I>() -> impl Parser<'tok, I, Item, Extras<'tok, 'src>>
where
    I: ValueInput<'tok, Span = SimpleSpan, Token = Token<'src>>,
{
    choice((static_var(), todo()))
}

fn array_type<'tok, 'src: 'tok, I>() -> impl Parser<'tok, I, Vec<usize>, Extras<'tok, 'src>>
where
    I: ValueInput<'tok, Span = SimpleSpan, Token = Token<'src>>,
{
    select! {
        Token::IntLiteral(i) => i.parse::<usize>().unwrap(),
    }
    .delimited_by(just(Token::LBrack), just(Token::RBrack))
    .repeated()
    .collect()
}

fn base_type<'tok, 'src: 'tok, I>() -> impl Parser<'tok, I, Ty, Extras<'tok, 'src>>
where
    I: ValueInput<'tok, Span = SimpleSpan, Token = Token<'src>>,
{
    choice((
        select! {
            Token::Int => TyKind::Primitive(Primitive::Int).into(),Token::Char => TyKind::Primitive(Primitive::Char).into(),Token::Void => TyKind::Void.into(),Token::Underscore => TyKind::Infer.into()
        },
        struct_type(),
    ))
}

fn strict_base_type<'tok, 'src: 'tok, I>() -> impl Parser<'tok, I, Ty, Extras<'tok, 'src>>
where
    I: ValueInput<'tok, Span = SimpleSpan, Token = Token<'src>>,
{
    choice((
        select! {
            Token::Int => TyKind::Primitive(Primitive::Int).into(),Token::Char => TyKind::Primitive(Primitive::Char).into(),Token::Void => TyKind::Void.into()
        },
        struct_type(),
    ))
}

fn pointer_type<'tok, 'src: 'tok, I>() -> impl Parser<'tok, I, usize, Extras<'tok, 'src>>
where
    I: ValueInput<'tok, Span = SimpleSpan, Token = Token<'src>>,
{
    just(Token::And).repeated().count()
}

fn struct_type<'tok, 'src: 'tok, I>() -> impl Parser<'tok, I, Ty, Extras<'tok, 'src>>
where
    I: ValueInput<'tok, Span = SimpleSpan, Token = Token<'src>>,
{
    just(Token::Struct).ignore_then(select! {
        Token::Identifier(s) => TyKind::Struct(s.into()).into()
    })
}

/// See [grammars.md](../grammars/parser_grammar.md)
fn type_parser<'tok, 'src: 'tok, I>() -> impl Parser<'tok, I, Ty, Extras<'tok, 'src>>
where
    I: ValueInput<'tok, Span = SimpleSpan, Token = Token<'src>>,
{
    let base_type = base_type().boxed();
    let parse_type_1 = choice((
        select! {
            Token::IntLiteral(i) => i.parse::<usize>().unwrap(),
        }
        .delimited_by(just(Token::LBrack), just(Token::RBrack))
        .repeated()
        .collect(),
        empty().to(vec![]),
    ));
    recursive(|parse_type| {
        choice((
            base_type.then(parse_type_1.clone()).map(make_array_type),
            just(Token::And)
                .then(parse_type.clone())
                .map(|(_, ty)| TyKind::Pointer(ty).into()),
            parse_type
                .delimited_by(just(Token::LPar), just(Token::RPar))
                .then(parse_type_1.clone())
                .map(make_array_type),
        ))
    })
}

fn parse_type_strict<'tok, 'src: 'tok, I>() -> impl Parser<'tok, I, Ty, Extras<'tok, 'src>>
where
    I: ValueInput<'tok, Span = SimpleSpan, Token = Token<'src>>,
{
    let base_type = strict_base_type().boxed();
    let parse_type_1 = choice((
        select! {
            Token::IntLiteral(i) => i.parse::<usize>().unwrap(),
        }
        .delimited_by(just(Token::LBrack), just(Token::RBrack))
        .repeated()
        .collect(),
        empty().to(vec![]),
    ));
    recursive(|parse_type| {
        choice((
            base_type.then(parse_type_1.clone()).map(make_array_type),
            just(Token::And)
                .then(parse_type.clone())
                .map(|(_, ty)| TyKind::Pointer(ty).into()),
            parse_type
                .delimited_by(just(Token::LPar), just(Token::RPar))
                .then(parse_type_1.clone())
                .map(make_array_type),
        ))
    })
}

fn make_array_type((ty, sizes): (Ty, Vec<usize>)) -> Ty {
    let mut ty = ty;
    let mut sizes = sizes.clone();
    while !sizes.is_empty() {
        let size = sizes.pop().unwrap();
        ty = TyKind::Array(size, ty).into();
    }
    ty
}

fn make_ptr_type(ty: Ty, indir: usize) -> Ty {
    let mut ty = ty;
    for _ in 0..indir {
        ty = TyKind::Pointer(ty).into();
    }
    ty
}

/// Helper method for parsing, also useful for testing
fn get_inputs<'tok, 'src: 'tok>(
    source: &'src SourceFile,
) -> impl ValueInput<'tok, Span = SimpleSpan, Token = Token<'src>> {
    let token_iter = lex(source);
    Stream::from_iter(token_iter).map((0..source.source.len()).into(), |(t, s)| (t, s))
}

#[cfg(test)]
mod tests {
    use std::{fmt::Display, io::Write};

    use anyhow::{bail, Result};
    use ariadne::FileCache;
    use chumsky::Parser;
    use internment::Intern;
    use rstest::{fixture, rstest};
    use serde::Serialize;
    use tempfile::NamedTempFile;

    use crate::{
        ast::types::{Primitive, Ty, TyKind},
        lexer::SourceFile,
        parser::base_type,
    };

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Hash)]
    enum TyTestKind {
        Int,
        Char,
        Void,
        Struct,
        Pointer,
        Array,
        Infer,
    }

    impl Display for TyTestKind {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let s = match self {
                Self::Int => "int",
                Self::Char => "char",
                Self::Void => "void",
                Self::Struct => "struct",
                Self::Pointer => "pointer",
                Self::Array => "array",
                Self::Infer => "infer",
            };
            write!(f, "{}", s)
        }
    }

    impl TyTestKind {
        pub fn equals(&self, other: &TyKind) -> bool {
            match (self, other) {
                (Self::Int, TyKind::Primitive(Primitive::Int))
                | (Self::Char, TyKind::Primitive(Primitive::Char))
                | (Self::Void, TyKind::Void)
                | (Self::Struct, TyKind::Struct(_))
                | (Self::Pointer, TyKind::Pointer(_))
                | (Self::Array, TyKind::Array(_, _))
                | (Self::Infer, TyKind::Infer) => true,
                (_, _) => false,
            }
        }
    }

    #[fixture]
    fn cache() -> FileCache {
        FileCache::default()
    }

    fn match_kinds<const N: usize>(kinds: [TyTestKind; N], entry_ty: Ty) {
        let mut cur_kind = *entry_ty.kind;
        for i in 0..N {
            if !kinds[i].equals(&cur_kind) {
                panic!("Mismatching kinds[{i}]: {0} != {cur_kind}", kinds[i])
            }
            match cur_kind {
                TyKind::Primitive(_) | TyKind::Void | TyKind::Struct(_) | TyKind::Infer => {
                    if i == N - 1 {
                        return;
                    } else {
                        panic!("Unexpected end of chain with {cur_kind}");
                    }
                }
                TyKind::Pointer(next_ty) | TyKind::Array(_, next_ty) => {
                    if i == N - 1 {
                        panic!(
                            "Unexpected chain continues from {cur_kind} with {0}",
                            *next_ty.kind
                        );
                    } else {
                        cur_kind = *next_ty.kind;
                    }
                }
            }
        }
    }

    fn src(s: String, cache: FileCache) -> SourceFile {
        let mut f = NamedTempFile::new().unwrap();
        write!(f, "{}", s).unwrap();
        SourceFile::from_path(f.path(), cache)
    }

    #[rstest]
    #[case::int("23", "23")]
    #[case::hello_world("\"Hello, world!\"", "\"Hello, world!\"")]
    #[case::char_a("'a'", "'a'")]
    fn test_literal(
        #[case] input: String,
        #[case] expected: String,
        cache: FileCache,
    ) -> Result<()> {
        let src_file = src(input, cache);
        let inputs = super::get_inputs(&src_file);
        match super::literal().parse(inputs).into_result() {
            Ok(recovered) => assert_eq!(recovered.to_string(), expected),
            Err(errs) => {
                super::print_errors(&src_file.source, errs);
                bail!("Parsing error");
            }
        };
        Ok(())
    }

    #[rstest]
    #[case::infer("_", [TyTestKind::Infer])]
    #[case::int("int", [TyTestKind::Int])]
    #[case::char("char", [TyTestKind::Char])]
    #[case::int_ptr("&char", [TyTestKind::Pointer, TyTestKind::Char])]
    #[case::int_arr("char[4]", [TyTestKind::Array, TyTestKind::Char])]
    #[case::ptr_arr("(&int)[10]", [TyTestKind::Array, TyTestKind::Pointer, TyTestKind::Int])]
    #[case::arr_ptr("&int[10]", [TyTestKind::Pointer, TyTestKind::Array, TyTestKind::Int])]
    fn test_type<const N: usize>(
        #[case] input: String,
        #[case] expected: [TyTestKind; N],
        cache: FileCache,
    ) {
        let src_file = src(input, cache);
        let inputs = super::get_inputs(&src_file);
        match super::type_parser().parse(inputs).into_result() {
            Ok(recovered) => match_kinds(expected, recovered),
            Err(errs) => {
                super::print_errors(&src_file.source, errs);
                panic!("Parsing error");
            }
        }
    }

    #[rstest]
    #[case::int_literal("24", "24")]
    #[case::ident("foo", "foo")]
    #[case::lit_assign("foo = 42", "(foo = 42)")]
    #[case::double_assign("foo = bar = 42", "(foo = (bar = 42))")]
    #[case::log_or("foo = 42 || 24", "(foo = (42 || 24))")]
    #[case::log_ops("foo = 42 || 24 && 24 || 42", "(foo = ((42 || (24 && 24)) || 42))")]
    fn test_expr(#[case] input: String, #[case] expected: String, cache: FileCache) {
        let src_file = src(input, cache);
        let inputs = super::get_inputs(&src_file);
        match super::expr().parse(inputs).into_result() {
            Ok(recovered) => assert_eq!(recovered.to_string(), expected),
            Err(errs) => {
                super::print_errors(&src_file.source, errs);
                panic!("Parsing error");
            }
        }
    }
}
