use std::str::Chars;

const SPACE_CHAR: char = ' ';

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Ident { sym: String },
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Literal { kind: LiteralKind },
    Eq,
    Semi,
    Colon,
    Minus,
    Plus,
    Comma,
    Gt,
    Unknown,
    Eof,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralKind {
    Int(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
}

pub struct Cursor<'a> {
    chars: Chars<'a>,
}

impl<'a> Cursor<'a> {
    pub fn new(src: &'a str) -> Self {
        Cursor { chars: src.chars() }
    }

    fn advance(&mut self) -> TokenKind {
        let Some(c) = self.peek() else {
            return TokenKind::Eof;
        };

        match c {
            c if is_ident_start(c) => self.ident(),
            '0'..='9' => self.number(),
            '(' => self.bump_and_return(TokenKind::OpenParen),
            ')' => self.bump_and_return(TokenKind::CloseParen),
            '{' => self.bump_and_return(TokenKind::OpenBrace),
            '}' => self.bump_and_return(TokenKind::CloseBrace),
            ';' => self.bump_and_return(TokenKind::Semi),
            ':' => self.bump_and_return(TokenKind::Colon),
            '=' => self.bump_and_return(TokenKind::Eq),
            '-' => self.bump_and_return(TokenKind::Minus),
            '+' => self.bump_and_return(TokenKind::Plus),
            '>' => self.bump_and_return(TokenKind::Gt),
            ',' => self.bump_and_return(TokenKind::Comma),
            _ => self.bump_and_return(TokenKind::Unknown),
        }
    }

    fn bump_and_return(&mut self, kind: TokenKind) -> TokenKind {
        self.bump();
        kind
    }

    pub fn bump(&mut self) -> Option<char> {
        let mut c = self.chars.next();
        if c.is_some() { c.take() } else { None }
    }

    pub fn ident(&mut self) -> TokenKind {
        TokenKind::Ident {
            sym: self.eat_while(is_ident_next),
        }
    }

    pub fn number(&mut self) -> TokenKind {
        TokenKind::Literal {
            kind: LiteralKind::Int(self.eat_while(|c| c.is_ascii_digit())),
        }
    }

    pub fn eat_while(&mut self, predicate: impl Fn(char) -> bool) -> String {
        let mut result = String::new();
        while let Some(c) = self.peek() {
            if !predicate(c) {
                break;
            };
            result.push(c);
            self.chars.next();
        }
        result
    }

    pub fn peek(&mut self) -> Option<char> {
        self.chars.clone().next()
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        while !self.is_eof() {
            self.skip_whitespace();
            let token_kind = self.advance();
            tokens.push(Token { kind: token_kind })
        }
        tokens
    }

    fn is_eof(&self) -> bool {
        self.chars.as_str().is_empty()
    }

    pub fn skip_whitespace(&mut self) {
        while self.peek().is_some() {
            let Some(c) = self.peek() else {
                break;
            };
            if is_whitespace(c) {
                self.chars.next();
            } else {
                break;
            }
        }
    }
}

pub fn is_ident_start(c: char) -> bool {
    c.is_ascii_alphabetic()
}

pub fn is_ident_next(c: char) -> bool {
    c.is_ascii_alphanumeric()
}

pub fn is_whitespace(c: char) -> bool {
    c.is_whitespace()
}
