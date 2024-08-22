use clap::{Parser, ValueEnum};

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
pub struct Cli {
    /// Specify the maximum number of servers added to favorites.json
    /// {n}[Note: current server-browser gets buggy after 100] [Default: 100]
    #[arg(short, long)]
    pub limit: Option<usize>,

    /// Specify a minimum number of players a server must have [Default: 0]
    #[arg(short, long)]
    pub player_min: Option<u16>,

    /// Specify region [Default: include all regions]
    #[arg(short, long, value_enum)]
    pub region: Option<Region>,

    /// Server name must include [Note: case-insensitive]
    #[arg(short, long)]
    pub includes: Option<String>,

    /// Server name must exclude [Note: case-insensitive, exclude has higher priority]
    #[arg(short, long)]
    pub excludes: Option<String>,
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
    pub includes: Option<String>,
    pub excludes: Option<String>,
}
