use std::io;

use io::BufRead;
use io::BufReader;
use monkey::lexer::Lexer;
use monkey::token::TokenKind;

const PROMPT: &str = ">> ";

pub(crate) fn Start(i: &mut dyn io::Read, o: &mut dyn io::Write) -> io::Result<()> {
    let mut scanner = BufReader::new(i);
    let mut line = String::new();

    loop {
        line.clear();
        o.write_all(PROMPT.as_bytes())?;
        o.flush()?;

        if scanner.read_line(&mut line)? == 0 {
            writeln!(o)?;
            break;
        }

        let mut l = Lexer::new(&line);

        loop {
            let tok = l.NextToken();
            if tok.kind == TokenKind::EOF {
                break;
            }

            writeln!(o, "{}", tok)?;
        }
    }
    Ok(())
}
