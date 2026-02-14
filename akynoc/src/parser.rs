use chumsky::{
    extra::Err,
    input::{Input, Stream, ValueInput},
    prelude::*,
    span::SimpleSpan,
    Parser,
};

use crate::{
    ast::{
        exprs::{Expr, ExprKind, FnParam, FnSig, Literal},
        patterns::{Ident, PatternKind},
        types::{Primitive, TyKind},
    },
    lexer::{lex, SourceFile, Token},
};

pub fn token_stream<'tok, 'src: 'tok>(src: &'src SourceFile) -> impl ValueInput<'tok, Span = SimpleSpan, Token = Token<'src>> {
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
            let tuple_type = comma_sep_types.clone().map(|types| TyKind::Tuple(types).into());
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
            Token::IntLiteral(i) => Literal::Int(i.parse::<u32>().unwrap()).into(),
            Token::CharLiteral(c) => any::<&str,Err<chumsky::error::EmptyErr>>().parse(c).into_result().unwrap().into(),
        };

        let let_expr = just(Token::Let)
            .ignore_then(pattern.clone())
            .then_ignore(just(Token::Assign))
            .then(expr.clone().memoized())
            .map(|(pat, expr)| ExprKind::Let(pat, Box::new(expr)).into());

        let block_expr = expr
            .clone()
            .memoized()
            .separated_by(just(Token::Semi))
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LBrace), just(Token::RBrace))
            .map(|exprs| ExprKind::Block(exprs).into());

        let if_expr = just(Token::If)
            .ignore_then(expr.clone().memoized().delimited_by(just(Token::LPar), just(Token::RPar)))
            .then(expr.clone().memoized())
            .then(just(Token::Else).ignore_then(expr.clone().memoized()).or_not())
            .map(|((exp, then), els)| {
                if let Some(els) = els {
                    ExprKind::If(Box::new(exp), Box::new(then), Some(Box::new(els))).into()
                } else {
                    ExprKind::If(Box::new(exp), Box::new(then), None).into()
                }
            });

        let while_expr = just(Token::While)
            .ignore_then(expr.clone().memoized().delimited_by(just(Token::LPar), just(Token::RPar)))
            .then(expr.clone().memoized())
            .map(|(exp, lp)| ExprKind::While(Box::new(exp), Box::new(lp)).into());

        let fn_params = ident
            .clone()
            .then_ignore(just(Token::Colon))
            .then(typ.clone())
            .map(|(name, ty)| FnParam { name, ty })
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>();

        let fn_sig = just(Token::Fn)
            .ignore_then(ident.clone())
            .then(fn_params.clone().delimited_by(just(Token::LPar), just(Token::RPar)))
            .then(just(Token::Colon).ignore_then(typ.clone()).or_not())
            .map(|((name, params), ty)| {
                if let Some(ty) = ty {
                    FnSig { name, params, ty }
                } else {
                    FnSig {
                        name,
                        params,
                        ty: TyKind::Infer.into(),
                    }
                }
            });

        let inline_fn = fn_sig
            .clone()
            .then_ignore(just(Token::FatArrow))
            .then_ignore(just(Token::LBrace).not())
            .then(expr.clone().memoized())
            .map(|(sig, expr)| ExprKind::Fn(sig, Box::new(expr)).into());

        let block_fn = fn_sig
            .clone()
            .then(block_expr.clone())
            .map(|(sig, block)| ExprKind::Fn(sig, Box::new(block)).into());

        choice((pattern_expr, literal, let_expr, block_expr, if_expr, while_expr, inline_fn, block_fn))
    })
    .repeated()
    .collect::<Vec<Expr>>()
}

pub fn print_errors<'tok, 'src: 'tok>(source: &ariadne::Source, errs: Vec<Rich<'tok, Token<'src>>>) {
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

#[cfg(test)]
pub mod test {
    use std::io::Write;

    use anyhow::{bail, Result};
    use ariadne::FileCache;
    use chumsky::Parser;
    use rstest::{fixture, rstest};
    use tempfile::NamedTempFile;

    use crate::{ast::exprs::Expr, lexer::SourceFile};

    #[fixture]
    fn cache() -> FileCache {
        FileCache::default()
    }

    fn src(s: String, cache: FileCache) -> Result<SourceFile> {
        let mut f = NamedTempFile::new()?;
        write!(f, "{}", s)?;
        Ok(SourceFile::from_path(f.path(), cache))
    }

    fn ast_to_string(ast: Vec<Expr>) -> String {
        ast.iter().map(Expr::to_string).collect::<Vec<String>>().join("\n")
    }

    #[rstest]
    #[case::int("23", "23")]
    #[case::char("'a'", "'a'")]
    fn test_literal(#[case] input: String, #[case] expected: String, cache: FileCache) -> Result<()> {
        let src_file = src(input, cache)?;
        let tokens = super::token_stream(&src_file);
        match super::parser().parse(tokens).into_result() {
            Ok(ast) => assert_eq!(ast_to_string(ast), expected),
            Err(errs) => {
                super::print_errors(&src_file.source, errs);
                bail!("Parsing error");
            }
        }
        Ok(())
    }
}
