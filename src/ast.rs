use crate::lexer::{Token, TokenKind};

#[derive(Debug)]
enum AstNodeKind {
    Fn(Box<FnDef>),
    Expr {},
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
pub struct FnArgs {
    ident: String,
}

#[derive(Debug)]
pub struct FnDef {
    name: String,
    args: Vec<FnArgs>,
    body: Vec<Expr>,
    ret: Option<Expr>,
}

#[derive(Debug)]
pub struct TokenStream {
    tokens: Vec<Token>,
}

#[derive(Debug)]
pub struct AstNode {
    nodes: Vec<AstNode>,
    kind: AstNodeKind,
}

impl TokenStream {
    pub fn new(stream: Vec<Token>) -> Self {
        TokenStream { tokens: stream }
    }

    pub fn build(&self) -> AstNode {
        for token in &self.tokens {
            // println!("{:?}", token)

            let ast_node = match &token.kind {
                TokenKind::Ident { sym } => println!("ident: {}", sym),
                _ => println!("idk"),
            };
        }

        AstNode {
            nodes: Vec::new(),
            kind: AstNodeKind::Expr {},
        }
    }

    fn ident(&self, sym: String) -> AstNode {
        let symbol = sym.as_str();
        let node = match symbol {
            "fn" => self.function_def(),
            _ => AstNode {
                nodes: Vec::new(),
                kind: AstNodeKind::Expr {},
            },
        };
        AstNode {
            nodes: Vec::new(),
            kind: AstNodeKind::Expr {},
        }
    }

    fn function_def(&self) -> AstNode {
        self.expect_next(TokenKind::Ident);
        AstNode {
            nodes: Vec::new(),
            kind: AstNodeKind::Fn(Box::new(FnDef {
                name: "something".to_string(),
                args: Vec::new(),
                body: Vec::new(),
                ret: None,
            })),
        }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.iter().clone().next()
    }

    fn eat(&mut self) -> Option<&Token> {
        self.tokens.iter().next()
    }

    fn expect_next(&mut self, next: TokenKind) -> bool {
        let Some(next_token) = self.peek() else {
            return false;
        };
        if (next_token.kind == next) {
            return true;
        } else {
            println!("shit is ass");
            return false;
        }
    }
}
