// SPDX-FileCopyrightText: 2023 Jonathan Haigh <jonathanhaigh@gmail.com>
//
// SPDX-License-Identifier: MIT

//! Types and methods related to parsing the command line.

use clap::Parser;

/// Command line arguments passed to SQ.
#[derive(Parser)]
#[command(author, version, about, long_about=None)]
#[must_use]
pub struct Cli {
    /// The SQ query
    pub query: String,
}

/// Parse the command line.
///
/// # Returns
/// - a `Cli` struct containing the command line arguments.
pub fn parse() -> Cli {
    Cli::parse()
}
