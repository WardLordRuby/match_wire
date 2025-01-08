use crate::{commands::reconnect::HISTORY_MAX, H2M_MAX_CLIENT_NUM, H2M_MAX_TEAM_SIZE};
use clap::{value_parser, ArgAction, Args, Parser, Subcommand, ValueEnum};

#[derive(Parser, Debug)]
#[command(about, long_about = None)]
pub struct UserCommand {
    #[command(subcommand)]
    pub command: Command,
}

#[derive(Subcommand, Debug)]
pub enum Command {
    /// Create a new favorites.json using various filter options
    #[command(alias = "Filter")]
    Filter {
        #[clap(flatten)]
        args: Option<Filters>,
    },

    /// Reconnect to last server joined
    #[command(alias = "Reconnect")]
    Reconnect {
        #[clap(flatten)]
        args: HistoryArgs,
    },

    /// Launch HMW/H2M
    #[command(alias = "Launch")]
    Launch,

    /// Commands to reset and update the cache file
    #[command(alias = "Cache")]
    Cache {
        #[arg(value_enum)]
        option: CacheCmd,
    },

    /// Opens H2M/HMW game console
    #[command(aliases(["Logs", "logs", "Console"]))]
    Console,

    /// Open MWR(2017) directory
    #[command(aliases(["Gamedir", "gamedir", "GameDir"]))]
    GameDir,

    /// Quit the program
    #[command(alias = "Quit")]
    Quit,

    /// Print version
    #[command(alias = "Version")]
    Version,

    /// Open the current local data directory
    #[command(aliases(["Localenv", "localenv", "LocalEnv"]), hide = true)]
    LocalEnv,
}

#[derive(Args, Debug)]
#[group(multiple = false)]
pub struct HistoryArgs {
    /// Display previously connected servers
    #[arg(short = 'H', long, action = ArgAction::SetTrue)]
    pub history: bool,

    /// Connect to numbered entry in history
    #[arg(short, long, value_parser = value_parser!(u8).range(1..=HISTORY_MAX as i64))]
    pub connect: Option<u8>,
}

#[derive(Args, Debug, Clone, Default)]
pub struct Filters {
    /// Specify the maximum number of servers added to favorites.json
    /// {n}  [Note: H2M server-browser gets buggy after 100, this bug is fixed in HMW]
    /// {n}  [H2M Default: 100] [HMW Default: uncapped]
    #[arg(short, long)]
    pub limit: Option<usize>,

    /// Specify a minimum number of players a server must have [Default: 0]
    #[arg(short, long, value_parser = value_parser!(u8).range(0..=H2M_MAX_CLIENT_NUM))]
    pub player_min: Option<u8>,

    /// Specify a maximum team size [Default: 9]
    #[arg(short, long, value_parser = value_parser!(u8).range(1..=H2M_MAX_TEAM_SIZE))]
    pub team_size_max: Option<u8>,

    /// Server contains bot players
    #[arg(long, group = "bots")]
    pub with_bots: bool,

    /// Server does not contain bot players
    #[arg(long, group = "bots")]
    pub without_bots: bool,

    /// Include servers that do not respond to a 'getInfo' request
    #[arg(long)]
    pub include_unresponsive: bool,

    /// Specify region(s) [Default: include all]
    #[arg(short, long, value_enum, num_args(1..=REGION_LEN))]
    pub region: Option<Vec<Region>>,

    /// Specify source(s) [Default: include all]
    #[arg(short, long, value_enum, num_args(1..=SOURCE_LEN))]
    pub source: Option<Vec<Source>>,

    /// Server name must contain any 1 of the following terms
    /// {n}  [Note: search terms are case-insensitive]
    #[arg(short, long, num_args(1..))]
    pub includes: Option<Vec<String>>,

    /// Server name must not contain any 1 of the following terms
    /// {n}  [Note: exclude has higher priority] [Examples]
    /// {n}  [-e term1 term2] searches for "term1" or "term2"
    /// {n}  [-e "one long term"] searches for "one long term"
    #[arg(short, long, num_args(1..))]
    pub excludes: Option<Vec<String>>,

    /// Specify a maximum number of 'getInfo' retries [Default: 3]
    #[arg(long, value_parser = value_parser!(u8).range(0..=20))]
    pub retry_max: Option<u8>,
}

pub const REGION_LEN: usize = 3;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug)]
pub enum Region {
    #[value(aliases(["Na", "nA", "NorthAmerica", "northAmerica", "northamerica"]))]
    NA,
    #[value(aliases(["Eu", "eU", "Europe", "europe"]))]
    EU,
    #[value(aliases(["APAC", "Asia", "AsiaPacific", "asia", "pacific"]))]
    Apac,
}

pub const SOURCE_LEN: usize = 2;

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, ValueEnum, Debug)]
pub enum Source {
    #[value(alias = "iw4")]
    Iw4Master,
    #[value(alias = "hmw")]
    HmwMaster,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug)]
pub enum CacheCmd {
    /// Clears entire cache file including connection history then starts a fresh cache file
    #[value(alias = "clear")]
    Reset,
    /// Updates all server names in the cache  
    /// {n}  Try this if 'reconnect' is returning: "Could not find server in cache"
    Update,
}
