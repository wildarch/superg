use std::collections::VecDeque;

#[derive(Debug, PartialEq)]
pub enum Token {
    Integer(i32),
    // Todo: borrow from source string
    Symbol(String),
    LParen,
    RParen,
}

pub fn lex(s: &str) -> VecDeque<Token> {
    // Trick from https://vishpat.github.io/lisp-rs/lexer.html
    let padded = s.replace("(", " ( ").replace(")", " ) ");
    let raw_tokens = padded.split_whitespace();
    raw_tokens.map(lex_raw_token).collect()
}

fn lex_raw_token(t: &str) -> Token {
    match t {
        "(" => Token::LParen,
        ")" => Token::RParen,
        t => {
            if let Ok(i) = t.parse::<i32>() {
                Token::Integer(i)
            } else {
                Token::Symbol(t.to_owned())
            }
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    fn int(i: i32) -> Token {
        Token::Integer(i)
    }
    fn symb(s: &str) -> Token {
        Token::Symbol(s.to_owned())
    }
    const LP: Token = Token::LParen;
    const RP: Token = Token::RParen;

    #[test]
    fn lex_factorial() {
        let program = r#"
(defun fac (n) 
  (if (= n 1)
      1
      (* n (fac (- n 1)))))
        "#;

        let lexed = lex(program);
        assert_eq!(
            lexed,
            vec![
                LP,
                symb("defun"),
                symb("fac"),
                LP,
                symb("n"),
                RP,
                LP,
                symb("if"),
                LP,
                symb("="),
                symb("n"),
                int(1),
                RP,
                int(1),
                LP,
                symb("*"),
                symb("n"),
                LP,
                symb("fac"),
                LP,
                symb("-"),
                symb("n"),
                int(1),
                RP,
                RP,
                RP,
                RP,
                RP,
            ]
        );
    }
}
