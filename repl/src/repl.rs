use std::io;

use io::BufRead;
use io::BufReader;
use monkey::ast::Node;
use monkey::lexer::Lexer;
use monkey::parser::Parser;

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

        let l = Lexer::New(&line);
        let mut p = Parser::New(l);
        let program = p.ParseProgram();
        if !p.errors.is_empty() {
            for error in p.errors {
                eprintln!("parse error: {}", error);
            }
            continue;
        }

        println!("{}", program.String());
    }
    Ok(())
}
