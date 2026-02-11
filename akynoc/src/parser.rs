use chumsky::{
    extra::Err,
    input::{Input, Stream, ValueInput},
    prelude::*,
    span::SimpleSpan,
    Parser,
};

use crate::{
    ast::{
        exprs::{Expr, ExprKind, FnParam, Literal},
        patterns::{Ident, PatternKind},
        types::{Primitive, TyKind},
    },
    lexer::{lex, SourceFile, Token},
};

pub fn token_stream<'tok, 'src: 'tok>(
    src: &'src SourceFile,
) -> impl ValueInput<'tok, Span = SimpleSpan, Token = Token<'src>> {
    let token_iter = lex(src);
    Stream::from_iter(token_iter).map((0..src.source.len()).into(), |(t, s)| (t, s))
}

pub fn parser<'tok, 'src: 'tok, I>() -> impl Parser<'tok, I, Vec<Expr>, Err<Rich<'tok, Token<'src>>>>
where
    I: ValueInput<'tok, Span = SimpleSpan, Token = Token<'src>>,
{
    recursive(|expr| {
        let typ = recursive(|typ| {
            let base_type = select! {
                Token::Int => TyKind::Primitive(Primitive::Int).into(),
                Token::Char => TyKind::Primitive(Primitive::Char).into(),
            };
            let ptr_type = just(Token::And)
                .ignore_then(typ.clone().memoized())
                .map(|inner_ty| TyKind::Pointer(inner_ty).into());
            let comma_sep_types = typ
                .clone()
                .memoized()
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LPar), just(Token::RPar));
            let tuple_type = comma_sep_types
                .clone()
                .map(|types| TyKind::Tuple(types).into());
            let fn_type = comma_sep_types
                .clone()
                .then_ignore(just(Token::FatArrow))
                .then(typ.clone().memoized())
                .map(|(params, ret)| TyKind::Fn(params, ret).into());
            choice((base_type, ptr_type, tuple_type, fn_type))
        });

        let ident = select! {
            Token::Identifier(s) => s.into(),
        };
        let ident_pattern = ident.clone().map(|id| PatternKind::Single(id).into());

        let pattern = choice((
            ident_pattern.clone(),
            ident
                .clone()
                .separated_by(just(Token::Comma))
                .collect::<Vec<Ident>>()
                .delimited_by(just(Token::LPar), just(Token::RPar))
                .map(|ids| PatternKind::Tuple(ids).into()),
        ));
        let pattern_expr = pattern.clone().map(|pat| ExprKind::Pattern(pat).into());
        let literal = select! {
            Token::IntLiteral(i) => Literal::Int(i.parse::<u32>().unwrap()).into()
        };

        let let_expr = just(Token::Let)
            .ignore_then(pattern.clone())
            .then_ignore(just(Token::Assign))
            .then(expr.clone().memoized())
            .map(|(pat, expr)| ExprKind::Let(pat, Box::new(expr)).into());

        let fn_params = ident
            .clone()
            .then_ignore(just(Token::Colon))
            .then(typ.clone())
            .map(|(name, ty)| FnParam { name, ty })
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>();

        choice((pattern_expr, literal, let_expr))
    })
    .repeated()
    .collect::<Vec<Expr>>()
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
