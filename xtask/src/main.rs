mod build_readme;

use std::path::PathBuf;

use anyhow::Result;
use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(author, version, about)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Generate README.md's table of contents and configuration reference.
    Readme {
        /// README to update or check.
        #[arg(long, default_value = "README.md")]
        readme: PathBuf,
        /// Check whether README.md is current without writing it.
        #[arg(long)]
        check: bool,
    },
}

fn main() -> Result<()> {
    match Cli::parse().command {
        Command::Readme { readme, check } => build_readme::generate_readme(&readme, check),
    }
}
