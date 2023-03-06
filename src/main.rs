//! Execute an SQ query.

use sqr::cli;
use sqr::error;
use sqr::lexer;
use sqr::parser;
use sqr::results;
use sqr::system;

fn run_sq(args: &cli::Cli) -> error::Result<()> {
    let tokens = lexer::lex(&args.query)?;

    let mut p = parser::Parser::new(&args.query, &tokens);
    let ast = p.parse()?;

    let mut serializer = serde_json::Serializer::pretty(std::io::stdout());
    results::generate_results(&ast, &mut serializer, system::root())
        // Print a newline even if we error: we've probably already written partial output to
        // stdout.
        .map_err(|e| {
            println!();
            e
        })?;

    // The serializer won't add a newline at the end of its output, so do it manually.
    println!();

    Ok(())
}

fn main() -> miette::Result<()> {
    let args = cli::parse();
    run_sq(&args).map_err(|e| miette::Report::new_boxed(e).with_source_code(args.query))
}
