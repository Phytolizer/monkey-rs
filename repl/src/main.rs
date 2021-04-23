#![allow(non_snake_case)]

use std::io;

use io::stdin;
use io::stdout;

mod repl;

fn main() -> io::Result<()> {
    repl::Start(&mut stdin(), &mut stdout())
}
