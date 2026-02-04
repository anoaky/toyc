//! Parses [`Tokens`](Token) into [`Items`](Item).
//!
#![doc = include_str!("../../grammars/parser_grammar.md")]
use chumsky::{
    extra::Err,
    input::{Stream, ValueInput},
    pratt::*,
    prelude::*,
};

use crate::{
    ast::{
        exprs::{Expr, ExprKind, Literal, Operator},
        functions::{FnDecl, FnDefn, FnSig, Param},
        statements::{Stmt, StmtKind},
        structs::{Field, StructDecl},
        types::{Ident, Primitive, Ty, TyKind},
        Item, ItemKind,
    },
    lexer::{lex, SourceFile, Token},
};

type Extras<'tok, 'src> = Err<Rich<'tok, Token<'src>>>;

/// Parses a [`SourceFile`] into a [`Vec`] of [`Items`](`Item`)
pub fn parse<'tok, 'src: 'tok>(src: &'src SourceFile) -> Vec<Item> {
    let inputs = token_stream(src);
    match item()
        .boxed()
        .repeated()
        .collect::<Vec<Item>>()
        .parse(inputs)
        .into_result()
    {
        Ok(ast) => ast,
        Err(errs) => {
            print_errors(&src.source, errs);
            panic!("Parsing failed")
        }
    }
}

/// Returns a parser that parses a [`SourceFile`] into a [`Vec`] of [`Items`](`Item`)
pub fn parser<'tok, 'src: 'tok, I>() -> impl Parser<'tok, I, Vec<Item>, Extras<'tok, 'src>>
where
    I: ValueInput<'tok, Span = SimpleSpan, Token = Token<'src>>,
{
    item().boxed().repeated().collect::<Vec<_>>()
}

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

pub fn token_stream<'tok, 'src: 'tok>(
    source: &'src SourceFile,
) -> impl ValueInput<'tok, Span = SimpleSpan, Token = Token<'src>> {
    let token_iter = lex(source);
    Stream::from_iter(token_iter).map((0..source.source.len()).into(), |(t, s)| (t, s))
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
        let atom = choice((
            atom,
            expr.clone()
                .delimited_by(just(Token::LPar), just(Token::RPar)),
        ));
        atom.pratt((
            infix(right(1), just(Token::Assign), |lhs, _, rhs, _| {
                ExprKind::Assign(Box::new(lhs), Box::new(rhs)).into()
            }),
            infix(left(3), just(Token::LogOr), |lhs, _, rhs, _| {
                ExprKind::BinOp(Box::new(lhs), Operator::Or, Box::new(rhs)).into()
            }),
            infix(left(5), just(Token::LogAnd), |lhs, _, rhs, _| {
                ExprKind::BinOp(Box::new(lhs), Operator::And, Box::new(rhs)).into()
            }),
            infix(
                left(7),
                just(Token::Eq).or(just(Token::Ne)),
                |lhs, op_token: Token<'_>, rhs, _| {
                    ExprKind::BinOp(Box::new(lhs), op_token.into(), Box::new(rhs)).into()
                },
            ),
            infix(
                left(9),
                choice((
                    just(Token::Lt),
                    just(Token::Gt),
                    just(Token::Le),
                    just(Token::Ge),
                )),
                |lhs, op: Token<'_>, rhs, _| {
                    ExprKind::BinOp(Box::new(lhs), op.into(), Box::new(rhs)).into()
                },
            ),
            infix(
                left(11),
                choice((just(Token::Plus), just(Token::Minus))),
                |lhs, op: Token<'_>, rhs, _| {
                    ExprKind::BinOp(Box::new(lhs), op.into(), Box::new(rhs)).into()
                },
            ),
            infix(
                left(13),
                choice((just(Token::Asterisk), just(Token::Div), just(Token::Rem))),
                |lhs, op: Token<'_>, rhs, _| {
                    ExprKind::BinOp(Box::new(lhs), op.into(), Box::new(rhs)).into()
                },
            ),
            prefix(
                15,
                choice((just(Token::Plus), just(Token::Minus))),
                |op: Token<'_>, rhs, _| {
                    ExprKind::BinOp(
                        Box::new(ExprKind::Literal(0.into()).into()),
                        op.into(),
                        Box::new(rhs),
                    )
                    .into()
                },
            ),
            prefix(15, just(Token::And), |_, rhs, _| {
                ExprKind::Ref(Box::new(rhs)).into()
            }),
            prefix(15, just(Token::Asterisk), |_, rhs, _| {
                ExprKind::Deref(Box::new(rhs)).into()
            }),
            prefix(
                15,
                typ()
                    .boxed()
                    .delimited_by(just(Token::LPar), just(Token::RPar)),
                |cast_to: Ty, rhs, _| ExprKind::Typecast(cast_to, Box::new(rhs)).into(),
            ),
            postfix(
                17,
                expr.clone()
                    .delimited_by(just(Token::LBrack), just(Token::RBrack)),
                |arr, ind, _| ExprKind::Index(Box::new(arr), Box::new(ind)).into(),
            ),
            postfix(
                17,
                just(Token::Dot).ignore_then(ident().boxed()),
                |str, field, _| ExprKind::FieldAccess(Box::new(str), field).into(),
            ),
            postfix(
                17,
                expr.clone()
                    .separated_by(just(Token::Comma))
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LPar), just(Token::RPar)),
                |fn_name, args, _| ExprKind::CallFn((fn_name, args).into()).into(),
            ),
        ))
    })
}

fn item<'tok, 'src: 'tok, I>() -> impl Parser<'tok, I, Item, Extras<'tok, 'src>>
where
    I: ValueInput<'tok, Span = SimpleSpan, Token = Token<'src>>,
{
    let static_var = just(Token::Static)
        .ignore_then(ident().boxed())
        .then(
            just(Token::Colon)
                .ignore_then(typ().boxed())
                .or_not()
                .map(|ty| ty.unwrap_or(TyKind::Infer.into())),
        )
        .then(just(Token::Assign).ignore_then(literal().boxed()).or_not())
        .then_ignore(just(Token::Semi))
        .map(|((ident, ty), value)| ItemKind::Static((ident, ty, value).into()).into());
    let fn_params = group((ident().boxed(), just(Token::Colon).ignored(), typ().boxed()))
        .map(|(name, _, ty)| Param { name, ty })
        .separated_by(just(Token::Comma))
        .collect::<Vec<Param>>();
    let fn_sig = group((
        ident().boxed(),
        just(Token::LPar).ignored(),
        fn_params,
        just(Token::RPar).ignored(),
        just(Token::Colon).ignored(),
        typ().boxed(),
    ))
    .map(|(name, _, params, _, _, ty)| FnSig { name, params, ty });
    let fn_decl = fn_sig
        .clone()
        .then_ignore(just(Token::Semi))
        .map(|sig| ItemKind::FnDecl(FnDecl { sig }).into());
    let fn_defn = group((
        fn_sig.clone(),
        just(Token::LBrace).rewind().ignored(),
        stmt().boxed(),
    ))
    .map(|(sig, _, block)| {
        ItemKind::FnDefn(FnDefn {
            sig,
            decl: None,
            block,
        })
        .into()
    });
    let struct_decl = group((
        just(Token::Struct).ignored(),
        ident().boxed(),
        just(Token::LBrace).ignored(),
        ident()
            .then_ignore(just(Token::Colon))
            .then(typ().boxed())
            .then_ignore(just(Token::Semi))
            .map(|(name, ty)| Field { name, ty })
            .repeated()
            .collect::<Vec<Field>>(),
        just(Token::RBrace).ignored(),
    ))
    .map(|(_, name, _, fields, _)| ItemKind::StructDecl(StructDecl { name, fields }).into());
    choice((static_var, fn_decl, fn_defn, struct_decl))
}

fn stmt<'tok, 'src: 'tok, I>() -> impl Parser<'tok, I, Stmt, Extras<'tok, 'src>>
where
    I: ValueInput<'tok, Span = SimpleSpan, Token = Token<'src>>,
{
    recursive(|stmt| {
        let block = group((
            just(Token::LBrace).ignored(),
            stmt.clone().repeated().collect::<Vec<_>>(),
            just(Token::RBrace).ignored(),
        ))
        .map(|(_, stmts, _)| StmtKind::Block(stmts.into()).into());
        let local_var_defn = choice((
            just(Token::Let)
                .ignore_then(ident().boxed())
                .then_ignore(just(Token::Colon))
                .then(typ().boxed())
                .then_ignore(just(Token::Assign))
                .then(expr().boxed())
                .then_ignore(just(Token::Semi))
                .map(|((ident, ty), expr)| StmtKind::Local((ident, ty, Some(expr)).into()).into()),
            just(Token::Let)
                .ignore_then(ident().boxed())
                .then_ignore(just(Token::Define))
                .then(expr().boxed())
                .then_ignore(just(Token::Semi))
                .map(|(ident, expr)| {
                    StmtKind::Local((ident, TyKind::Infer.into(), Some(expr)).into()).into()
                }),
        ));
        let local_var_decl = just(Token::Let)
            .ignore_then(ident().boxed())
            .then_ignore(just(Token::Colon))
            .then(typ().boxed())
            .then_ignore(just(Token::Semi))
            .map(|(ident, ty)| StmtKind::Local((ident, ty, None).into()).into());
        let whl = group((
            just(Token::While).ignored(),
            just(Token::LPar).ignored(),
            expr().boxed(),
            just(Token::RPar).ignored(),
            stmt.clone(),
        ))
        .map(|(_, _, expr, _, stmt)| StmtKind::While(expr, Box::new(stmt)).into());
        let if_parser = group((
            just(Token::If).ignored(),
            just(Token::LPar).ignored(),
            expr().boxed(),
            just(Token::RPar).ignored(),
            stmt.clone(),
            just(Token::Else).ignore_then(stmt.clone()).or_not(),
        ))
        .map(|(_, _, expr, _, then, els)| match els {
            Some(els) => StmtKind::If(expr, Box::new(then), Some(Box::new(els))).into(),
            None => StmtKind::If(expr, Box::new(then), None).into(),
        });
        let ret = group((
            just(Token::Return).ignored(),
            expr().boxed().or_not(),
            just(Token::Semi).ignored(),
        ))
        .map(|(_, expr, _)| StmtKind::Return(expr).into());
        let expr_stmt = expr()
            .boxed()
            .then_ignore(just(Token::Semi).or_not())
            .map(|exp| StmtKind::Expr(exp).into());
        let brk = group((just(Token::Break), just(Token::Semi))).to(StmtKind::Break.into());
        let cnt = group((just(Token::Continue), just(Token::Semi))).to(StmtKind::Continue.into());
        choice((
            block,
            local_var_decl,
            local_var_defn,
            whl,
            if_parser,
            ret,
            expr_stmt,
            brk,
            cnt,
        ))
    })
}

fn typ<'tok, 'src: 'tok, I>() -> impl Parser<'tok, I, Ty, Extras<'tok, 'src>>
where
    I: ValueInput<'tok, Span = SimpleSpan, Token = Token<'src>>,
{
    recursive(|typ| {
        let struct_type = just(Token::Struct).ignore_then(select! {
            Token::Identifier(s) => TyKind::Struct(s.into()).into()
        });
        let base_type = choice((
            select! {
                Token::Int => TyKind::Primitive(Primitive::Int).into(),Token::Char => TyKind::Primitive(Primitive::Char).into(),Token::Void => TyKind::Void.into(),Token::Underscore => TyKind::Infer.into(),
            },
            struct_type,
        ));
        let ptr_type = just(Token::And)
            .ignore_then(typ.clone().memoized())
            .map(|ty| TyKind::Pointer(ty).into());
        let arr_type = typ
            .clone().memoized()
            .then(
                select! {
                    Token::IntLiteral(i) => i.parse::<usize>().unwrap()
                }
                .delimited_by(just(Token::LBrack), just(Token::RBrack))
                .repeated()
                .collect::<Vec<_>>(),
            )
            .map(make_array_type);
        choice((
            arr_type,
            ptr_type,
            base_type,
            typ.memoized().delimited_by(just(Token::LPar), just(Token::RPar)),
        ))
    }).memoized()
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

#[cfg(test)]
mod tests {
    use std::io::Write;

    use anyhow::{bail, Result};
    use ariadne::FileCache;
    use chumsky::Parser;
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
    #[case::int("23", "23")]
    #[case::hello_world("\"Hello, world!\"", "\"Hello, world!\"")]
    #[case::char_a("'a'", "'a'")]
    fn test_literal(
        #[case] input: String,
        #[case] expected: String,
        cache: FileCache,
    ) -> Result<()> {
        let src_file = src(input, cache);
        let inputs = super::token_stream(&src_file);
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
    #[case::infer("_", "_")]
    #[case::int("int", "int")]
    #[case::char("char", "char")]
    #[case::int_ptr("&char", "(&char)")]
    #[case::int_arr("char[4]", "(char[4])")]
    #[case::ptr_arr("(&int)[10]", "((&int)[10])")]
    #[case::arr_ptr("&int[10]", "(&(int[10]))")]
    #[case::multidim_arr("int[4][5]", "(int[4][5])")]
    fn test_type(#[case] input: String, #[case] expected: String, cache: FileCache) {
        let src_file = src(input, cache);
        let inputs = super::token_stream(&src_file);
        match super::typ().parse(inputs).into_result() {
            Ok(recovered) => assert_eq!(recovered.to_string(), expected),
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
    #[case::plus_minus("3 + 4 - 6 + 7 + 8", "((((3 + 4) - 6) + 7) + 8)")]
    #[case::arithmetic("3 + 4 * 7 - 6 + 3 * 4", "(((3 + (4 * 7)) - 6) + (3 * 4))")]
    #[case::plus_parens("3 + (4 + 6) + 7", "((3 + (4 + 6)) + 7)")]
    #[case::deref("x * y + *z", "((x * y) + (*z))")]
    #[case::cast_char_to_int("x = (int)'c'", "(x = ((int) 'c'))")]
    #[case::simple_fn_call("x(1, 2, 3)", "(x(1, 2, 3))")]
    fn test_expr(#[case] input: String, #[case] expected: String, cache: FileCache) {
        let src_file = src(input, cache);
        let inputs = super::token_stream(&src_file);
        match super::expr().parse(inputs).into_result() {
            Ok(recovered) => assert_eq!(recovered.to_string(), expected),
            Err(errs) => {
                super::print_errors(&src_file.source, errs);
                panic!("Parsing error");
            }
        }
    }
}
