use clap::{Parser, ValueEnum};

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct Cli {
    /// Specify the maximum number of servers added to favorites.json [Note: current server-browser gets buggy after 100]
    #[arg(short, long)]
    pub limit: Option<usize>,

    /// Specify a minimum number of players a server must have
    #[arg(short, long)]
    pub player_min: Option<u16>,

    /// Specify region
    #[arg(short, long, value_enum)]
    pub region: Option<Region>,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug)]
pub enum Region {
    #[value(aliases(["na", "Na", "nA", "NorthAmerica", "northAmerica", "northamerica"]))]
    NA,
    #[value(aliases(["eu", "Eu", "eU", "Europe", "europe"]))]
    EU,
    #[value(aliases(["APAC", "Asia", "AsiaPacific", "asia", "pacific"]))]
    Apac,
}

#[derive(Debug)]
pub struct Filters {
    pub limit: usize,
    pub player_min: u16,
    pub region: Option<Region>,
}
