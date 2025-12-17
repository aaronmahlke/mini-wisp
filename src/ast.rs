use crate::lexer::{Token, TokenKind};

#[derive(Debug)]
pub struct Ast {
    nodes: Vec<AstNode>,
}

#[derive(Debug)]
enum AstNode {
    Fn(Box<FnDef>),
    Expr {},
}

#[derive(Debug, Clone)]
pub enum ParseError {
    UnexpectedToken { expected: TokenKind, found: Token },
    UnexpectedEof,
    ExpectedIdent(Token),
}

#[derive(Debug)]
pub enum Expr {
    CallExpr(CallExpr),
    MethodExpr,
    ArgList,
    Arg,
    TrailingBlock,
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
}

#[derive(Debug)]
pub struct FnDef {
    name: String,
    sign: FnSignature,
    body: Block,
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

        while let Some(token) = self.peek() {
            let ast_node = match &token.kind {
                TokenKind::Ident { sym } => self.ident(sym.clone())?,
                _ => {
                    self.eat();
                    continue;
                }
            };
            nodes.push(ast_node);
        }

        Ok(Ast { nodes: nodes })
    }

    fn ident(&mut self, sym: String) -> Result<AstNode, ParseError> {
        let symbol = sym.as_str();
        let node = match symbol {
            "fn" => self.function_def()?,
            _ => AstNode::Expr {},
        };
        Ok(node)
    }

    fn function_def(&mut self) -> Result<AstNode, ParseError> {
        self.expect(TokenKind::Ident {
            sym: "fn".to_string(),
        })?;

        let name = self.expect_any_ident()?;

        self.expect(TokenKind::OpenParen)?;
        let sign = self.fn_signature()?;

        self.expect(TokenKind::OpenBrace)?;
        let body = self.block()?;

        Ok(AstNode::Fn(Box::new(FnDef { name, sign, body })))
    }

    fn block(&mut self) -> Result<Block, ParseError> {
        let mut nodes = Vec::new();

        while let Some(token) = self.peek() {
            if token.kind == TokenKind::CloseBrace {
                break;
            }

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

        Ok(Block { nodes })
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        if let Some(token) = self.peek() {
            match &token.kind {
                TokenKind::Ident { sym } if sym == "return" => {
                    self.eat();
                    while let Some(t) = self.peek() {
                        if t.kind == TokenKind::Semi || t.kind == TokenKind::CloseBrace {
                            break;
                        }
                        self.eat();
                    }
                    Ok(Expr::ArgList)
                }
                TokenKind::Ident { sym } if sym == "let" => {
                    self.eat();
                    while let Some(t) = self.peek() {
                        if t.kind == TokenKind::Semi {
                            break;
                        }
                        self.eat();
                    }
                    Ok(Expr::ArgList)
                }
                TokenKind::Ident { sym } => {
                    self.eat();
                    Ok(Expr::CallExpr(CallExpr {
                        arglist: Box::new(Expr::ArgList),
                    }))
                }
                _ => {
                    self.eat();
                    Ok(Expr::ArgList)
                }
            }
        } else {
            Err(ParseError::UnexpectedEof)
        }
    }

    fn fn_signature(&mut self) -> Result<FnSignature, ParseError> {
        let mut params = Vec::new();

        while let Some(token) = self.peek() {
            if token.kind == TokenKind::CloseParen {
                break;
            }

            params.push(self.param()?);

            if let Some(next) = self.peek() {
                if next.kind == TokenKind::Comma {
                    self.eat();
                } else if next.kind != TokenKind::CloseParen {
                    return Err(ParseError::UnexpectedToken {
                        expected: TokenKind::Comma,
                        found: next.clone(),
                    });
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
        let ident = self.expect_any_ident()?;
        self.expect(TokenKind::Colon)?;
        let ty = self.parse_type()?;
        Ok(Parameter {
            ident,
            ty: Some(ty),
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

    fn expect_any_ident(&mut self) -> Result<String, ParseError> {
        let next_token = self.eat().ok_or(ParseError::UnexpectedEof)?;
        match &next_token.kind {
            TokenKind::Ident { sym } => Ok(sym.clone()),
            _ => Err(ParseError::ExpectedIdent(next_token.clone())),
        }
    }

    fn expect(&mut self, expected: TokenKind) -> Result<Token, ParseError> {
        let next_token = self.eat().ok_or(ParseError::UnexpectedEof)?;
        if next_token.kind == expected {
            Ok(next_token.clone())
        } else {
            Err(ParseError::UnexpectedToken {
                expected,
                found: next_token.clone(),
            })
        }
    }
}
