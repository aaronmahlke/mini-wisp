use crate::error::{ParseError, ParseErrorKind};
use crate::lexer::{LiteralKind as LexerLiteralKind, Token, TokenKind};
use crate::span::Span;

#[derive(Debug)]
pub struct Ast {
    nodes: Vec<AstNode>,
}

#[derive(Debug)]
enum AstNode {
    Fn(Box<FnDef>),
}

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum ExprKind {
    Literal(LiteralKind),
    Variable(String),
    BinaryOp {
        left: Box<Expr>,
        op: BinaryOperator,
        right: Box<Expr>,
    },
    CallExpr(CallExpr),
    MethodExpr,
    ArgList,
    Arg,
    TrailingBlock,
    Unkown,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOperator {
    Add,      // +
    Subtract, // -
    Multiply, // *  (TODO: add to lexer)
    Divide,   // /  (TODO: add to lexer)
    Gt,       // >
    Lt,       // <  (TODO: add to lexer)
    Gte,      // >= (TODO: add to lexer)
    Lte,      // <= (TODO: add to lexer)
    Eq,       // == (TODO: add to lexer)
    Neq,      // != (TODO: add to lexer)
}

impl BinaryOperator {
    fn precedence(&self) -> u8 {
        match self {
            BinaryOperator::Multiply | BinaryOperator::Divide => 5,
            BinaryOperator::Add | BinaryOperator::Subtract => 4,
            BinaryOperator::Gt | BinaryOperator::Lt | BinaryOperator::Gte | BinaryOperator::Lte => {
                3
            }
            BinaryOperator::Eq | BinaryOperator::Neq => 2,
        }
    }
}

#[derive(Debug)]
pub enum LiteralKind {
    Int(i32),
    Bool(bool),
}

#[derive(Debug)]
pub struct CallExpr {
    pub arglist: Box<Expr>,
}

#[derive(Debug)]
pub enum Type {
    I32,
    Bool,
    String,
    Unit,

    Named { name: String },
}

#[derive(Debug)]
pub struct Parameter {
    ident: String,
    ty: Option<Type>,
    span: Span,
}

#[derive(Debug)]
pub struct FnSignature {
    params: Vec<Parameter>,
    ret: Option<Type>,
}

#[derive(Debug)]
pub enum BlockItem {
    Stmt(Expr),
    Expr(Expr),
}

#[derive(Debug)]
pub struct Block {
    nodes: Vec<BlockItem>,
    span: Span,
}

#[derive(Debug)]
pub struct FnDef {
    name: String,
    sign: FnSignature,
    body: Block,
    span: Span,
}

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(stream: Vec<Token>) -> Self {
        Parser {
            tokens: stream,
            pos: 0,
        }
    }

    pub fn build(&mut self) -> Result<Ast, ParseError> {
        let mut nodes = Vec::new();

        while self.peek().is_some() {
            let token = self.peek().unwrap();
            let ast_node = match &token.kind {
                TokenKind::Ident { sym } => self.ident(sym.clone())?,
                _ => {
                    self.eat();
                    continue;
                }
            };
            nodes.push(ast_node);
        }

        Ok(Ast { nodes })
    }

    fn ident(&mut self, sym: String) -> Result<AstNode, ParseError> {
        let symbol = sym.as_str();
        match symbol {
            "fn" => self.function_def(),
            _ => {
                let token = self.peek().ok_or_else(|| {
                    ParseError::new(ParseErrorKind::UnexpectedEof, self.last_token_span())
                })?;
                Err(ParseError::unexpected_token(
                    TokenKind::Ident {
                        sym: "fn".to_string(),
                    },
                    token.clone(),
                ))
            }
        }
    }

    fn function_def(&mut self) -> Result<AstNode, ParseError> {
        let start_token = self.peek().ok_or_else(|| {
            ParseError::new(ParseErrorKind::UnexpectedEof, self.last_token_span())
        })?;
        let start_span = start_token.span;

        self.expect(TokenKind::Ident {
            sym: "fn".to_string(),
        })?;

        let name = self.expect_any_ident()?;

        self.expect(TokenKind::OpenParen)?;
        let sign = self.fn_signature()?;

        self.expect(TokenKind::OpenBrace)?;
        let body = self.block()?;
        let body_end = body.span.end;

        Ok(AstNode::Fn(Box::new(FnDef {
            name,
            sign,
            body,
            span: Span {
                start: start_span.start,
                end: body_end,
            },
        })))
    }

    fn block(&mut self) -> Result<Block, ParseError> {
        let start_token = self.peek().ok_or_else(|| {
            ParseError::new(ParseErrorKind::UnexpectedEof, self.last_token_span())
        })?;
        let start_span = start_token.span;

        let mut nodes = Vec::new();

        while self.peek().is_some() && self.peek().unwrap().kind != TokenKind::CloseBrace {
            let expr = self.parse_expr()?;

            if let Some(next) = self.peek() {
                if next.kind == TokenKind::Semi {
                    self.eat();
                    nodes.push(BlockItem::Stmt(expr))
                } else {
                    nodes.push(BlockItem::Expr(expr))
                }
            } else {
                nodes.push(BlockItem::Expr(expr));
            }
        }

        let end_span = if let Some(token) = self.peek() {
            token.span
        } else {
            self.last_token_span()
        };
        Ok(Block {
            nodes,
            span: Span {
                start: start_span.start,
                end: end_span.end,
            },
        })
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_expr_with_precedence(0)
    }

    fn parse_expr_with_precedence(&mut self, min_precedence: u8) -> Result<Expr, ParseError> {
        let mut left = self.parse_primary_expr()?;

        loop {
            let op = match self.peek() {
                Some(token) => match self.token_to_binary_op(&token.kind) {
                    Some(op) if op.precedence() >= min_precedence => op,
                    _ => break,
                },
                None => break,
            };

            self.eat();

            let right = self.parse_expr_with_precedence(op.precedence() + 1)?;

            let start = left.span.start;
            let end = right.span.end;
            left = Expr {
                kind: ExprKind::BinaryOp {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                },
                span: Span { start, end },
            };
        }

        Ok(left)
    }

    fn token_to_binary_op(&self, token: &TokenKind) -> Option<BinaryOperator> {
        match token {
            TokenKind::Plus => Some(BinaryOperator::Add),
            TokenKind::Minus => Some(BinaryOperator::Subtract),
            TokenKind::Gt => Some(BinaryOperator::Gt),
            _ => None,
        }
    }

    fn parse_primary_expr(&mut self) -> Result<Expr, ParseError> {
        let start_token = self.peek().ok_or_else(|| {
            ParseError::new(ParseErrorKind::UnexpectedEof, self.last_token_span())
        })?;
        let start_span = start_token.span;
        let token_kind = start_token.kind.clone();

        let kind = match &token_kind {
            TokenKind::Literal { kind } => match kind {
                LexerLiteralKind::Int(val) => {
                    let val = val.clone();
                    self.eat();
                    let number: i32 = val.parse().unwrap();
                    ExprKind::Literal(LiteralKind::Int(number))
                }
            },
            TokenKind::OpenParen => {
                self.eat(); // consume '('
                let inner = self.parse_expr()?;
                self.expect(TokenKind::CloseParen)?; // expect ')'
                return Ok(inner);
            }
            TokenKind::Ident { sym } if sym == "return" => {
                self.eat();
                self.eat_until_one_of(&[TokenKind::Semi, TokenKind::CloseBrace]);
                ExprKind::Unkown
            }
            TokenKind::Ident { sym } if sym == "let" => {
                self.eat();
                self.eat_until_one_of(&[TokenKind::Semi]);
                ExprKind::Unkown
            }
            TokenKind::Ident { sym } => {
                let name = sym.clone();
                self.eat();
                ExprKind::Variable(name)
            }
            _ => {
                self.eat();
                ExprKind::Unkown
            }
        };

        let end = self.last_token_span().end;
        Ok(Expr {
            kind,
            span: Span {
                start: start_span.start,
                end,
            },
        })
    }

    fn fn_signature(&mut self) -> Result<FnSignature, ParseError> {
        let mut params = Vec::new();

        while self.peek().is_some() && self.peek().unwrap().kind != TokenKind::CloseParen {
            params.push(self.param()?);

            if let Some(next) = self.peek() {
                if next.kind == TokenKind::Comma {
                    self.eat();
                } else if next.kind != TokenKind::CloseParen {
                    return Err(ParseError::unexpected_token(TokenKind::Comma, next.clone()));
                }
            }
        }

        self.expect(TokenKind::CloseParen)?;

        let ret = if let Some(token) = self.peek() {
            if token.kind == TokenKind::Minus {
                self.eat();
                self.expect(TokenKind::Gt)?;
                Some(self.parse_type()?)
            } else {
                None
            }
        } else {
            None
        };

        Ok(FnSignature { params, ret })
    }

    fn param(&mut self) -> Result<Parameter, ParseError> {
        let ident_token = self.peek().ok_or_else(|| {
            ParseError::new(ParseErrorKind::UnexpectedEof, self.last_token_span())
        })?;
        let start = ident_token.span.start;

        let ident = self.expect_any_ident()?;
        self.expect(TokenKind::Colon)?;
        let ty = self.parse_type()?;
        let end = self.last_token_span().end;

        Ok(Parameter {
            ident,
            ty: Some(ty),
            span: Span { start, end },
        })
    }

    fn parse_type(&mut self) -> Result<Type, ParseError> {
        let type_name = self.expect_any_ident()?;

        let ty = match type_name.as_str() {
            "i32" => Type::I32,
            "bool" => Type::Bool,
            "String" => Type::String,
            name => Type::Named {
                name: name.to_string(),
            },
        };
        Ok(ty)
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn eat(&mut self) -> Option<&Token> {
        let token = self.tokens.get(self.pos)?;
        self.pos += 1;
        Some(token)
    }

    fn last_token_span(&self) -> Span {
        if self.pos > 0 {
            self.tokens[self.pos - 1].span
        } else {
            Span { start: 0, end: 0 }
        }
    }

    fn expect_any_ident(&mut self) -> Result<String, ParseError> {
        let eof_span = self.last_token_span();
        let next_token = self
            .eat()
            .ok_or_else(|| ParseError::new(ParseErrorKind::UnexpectedEof, eof_span))?;
        match &next_token.kind {
            TokenKind::Ident { sym } => Ok(sym.clone()),
            _ => Err(ParseError::new(
                ParseErrorKind::ExpectedIdent(next_token.clone()),
                next_token.span,
            )),
        }
    }

    fn expect(&mut self, expected: TokenKind) -> Result<Token, ParseError> {
        let eof_span = self.last_token_span();
        let next_token = self
            .eat()
            .ok_or_else(|| ParseError::new(ParseErrorKind::UnexpectedEof, eof_span))?;
        if next_token.kind == expected {
            Ok(next_token.clone())
        } else {
            Err(ParseError::unexpected_token(expected, next_token.clone()))
        }
    }

    /// Consume tokens while the predicate function returns true
    fn eat_while<F>(&mut self, mut predicate: F)
    where
        F: FnMut(&Token) -> bool,
    {
        while let Some(token) = self.peek() {
            if !predicate(token) {
                break;
            }
            self.eat();
        }
    }

    /// Consume tokens until the predicate is true (or EOF)
    fn eat_until<F>(&mut self, mut predicate: F)
    where
        F: FnMut(&Token) -> bool,
    {
        while let Some(token) = self.peek() {
            if predicate(token) {
                break;
            }
            self.eat();
        }
    }

    /// Consume tokens until we hit one of the specified token kinds
    fn eat_until_one_of(&mut self, kinds: &[TokenKind]) {
        while let Some(token) = self.peek() {
            if kinds.iter().any(|k| &token.kind == k) {
                break;
            }
            self.eat();
        }
    }
}
