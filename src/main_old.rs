use std::env;
use std::fs;
use std::str::Chars;

#[derive(Debug, Copy, Clone, PartialEq)]
enum TokenKind {
    Ident(&'static str),
    Semicolon,
    Whitespace,
    Eof,
}

const EOF_CHAR: char = '\0';

pub fn is_whitespace(c: char) -> bool {
    matches!(
        c,
        // End-of-line characters
        | '\u{000A}' // line feed (\n)
        | '\u{000B}' // vertical tab
        | '\u{000C}' // form feed
        | '\u{000D}' // carriage return (\r)
        | '\u{0085}' // next line (from latin1)
        | '\u{2028}' // LINE SEPARATOR
        | '\u{2029}' // PARAGRAPH SEPARATOR

        // `Default_Ignorable_Code_Point` characters
        | '\u{200E}' // LEFT-TO-RIGHT MARK
        | '\u{200F}' // RIGHT-TO-LEFT MARK

        // Horizontal space characters
        | '\u{0009}'   // tab (\t)
        | '\u{0020}' // space
    )
}

struct Token {
    kind: TokenKind,
}

struct Cursor<'a> {
    chars: Chars<'a>,
}

fn is_ident_start(c: char) -> bool {
    c.is_ascii_alphabetic()
}

fn is_ident_next(c: char) -> bool {
    c.is_ascii_alphanumeric()
}

impl<'a> Cursor<'a> {
    fn new(src: &'a str) -> Self {
        Cursor { chars: src.chars() }
    }

    fn bump(&mut self) -> Option<char> {
        let c = self.chars.next()?;
        Some(c)
    }

    fn first(&self) -> char {
        let mut chars = self.chars.clone();
        chars.next().unwrap_or(EOF_CHAR)
    }

    fn second(&self) -> char {
        let mut chars = self.chars.clone();
        chars.next();
        chars.next().unwrap_or(EOF_CHAR)
    }

    fn third(&self) -> char {
        let mut chars = self.chars.clone();
        chars.next();
        chars.next();
        chars.next().unwrap_or(EOF_CHAR)
    }

    fn eat_while(&mut self, mut predicate: impl FnMut(char) -> bool) {
        while predicate(self.first()) && !self.is_eof() {
            self.bump();
        }
    }

    fn is_eof(&self) -> bool {
        self.chars.as_str().is_empty()
    }

    fn advance_token(&mut self) -> Token {
        let Some(first_char) = self.bump() else {
            return Token {
                kind: TokenKind::Eof,
            };
        };

        let token_kind = match first_char {
            c if is_whitespace(c) => TokenKind::Whitespace,
            c if is_ident_start(c) => self.ident(),
            _ => TokenKind::Eof,
        };

        Token { kind: token_kind }
    }

    fn ident(&mut self) -> TokenKind {
        self.eat_while(is_ident_next);
        TokenKind::Ident
    }
}

fn tokenize(input: &str) -> impl Iterator<Item = Token> + '_ {
    let mut cursor = Cursor::new(input);
    std::iter::from_fn(move || {
        let token = cursor.advance_token();
        if token.kind != TokenKind::Eof {
            Some(token)
        } else {
            None
        }
    })
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_src = &args[1];
    println!("Source: {}", file_src);

    let file_content = fs::read_to_string(file_src).expect("Something went wrong reading the file");

    for token in tokenize(file_content.as_str()) {
        println!("{:?}", token.kind)
    }
}
