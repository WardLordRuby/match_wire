use crate::{
    MAX_H2M_CLIENT_NUM, MAX_H2M_TEAM_SIZE,
    commands::{reconnect::MAX_CONNECTION_HISTORY, settings::RETRIES_MAX},
};

use clap::{Args, Parser, ValueEnum, value_parser};

#[cfg(not(debug_assertions))]
#[derive(Parser, Debug)]
pub struct Cli {
    #[arg(long)]
    pub no_launch: bool,
}

#[derive(Parser, Debug)]
#[command(name = "", about, long_about = None)]
pub(crate) enum Command {
    /// Create a new favorites.json using various filter options
    #[command(alias = "Filter")]
    Filter {
        #[clap(flatten)]
        args: Option<Filters>,
    },

    /// Displays the results from the last filter command that included '--stats'
    #[command(alias = "Last")]
    Last {
        /// Refresh and display stats about the last executed `Filter` command
        #[arg(short, long)]
        refresh: bool,
    },

    /// Reconnect or queue into a recently joined server
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

    /// Opens HMW/H2M game console
    #[command(aliases(["Logs", "logs", "Console"]))]
    Console {
        /// Re-print all logs captured from MW2 Remastered
        #[arg(long)]
        all: bool,
    },

    /// Open MWR(2017) directory
    #[command(aliases(["Gamedir", "gamedir", "GameDir"]))]
    GameDir,

    /// Print version
    #[command(alias = "Version")]
    Version {
        /// Verify all HMW mod files are up-to-date
        #[arg(long, alias = "verify")]
        verify_all: bool,
    },

    /// Modify MatchWire defaults
    #[command(alias = "Settings")]
    Settings {
        /// Reset settings to MatchWire defaults
        #[arg(long, alias = "verify")]
        use_default: bool,
    },

    /// Quit the program
    #[command(alias = "Quit")]
    Quit,

    /// Open the current local data directory
    #[command(aliases(["Localenv", "localenv", "LocalEnv"]), hide = true)]
    LocalEnv,
}

#[derive(Args, Debug)]
pub(crate) struct HistoryArgs {
    /// Display previously connected servers
    #[arg(short = 'H', long, conflicts_with_all = ["abort", "queue", "connect"])]
    pub(crate) history: bool,

    /// Aborts the queued connection attempt if one is pending
    #[arg(long, conflicts_with_all = ["history", "queue", "connect"])]
    pub(crate) abort: bool,

    /// Queue a server connection attempt, waiting to connect until the server has free space
    #[arg(short, long)]
    pub(crate) queue: bool,

    /// Connect to numbered entry in history
    #[arg(short, long, value_name = "HISTORY_ENTRY", value_parser = value_parser!(u8).range(1..=MAX_CONNECTION_HISTORY as i64))]
    pub(crate) connect: Option<u8>,
}

#[derive(Args, Debug, Clone, Default)]
pub(crate) struct Filters {
    /// Specify the maximum number of servers added to favourites.json
    /// {n}  [Note: H2M server-browser gets buggy after 100, this bug is fixed in HMW]
    /// {n}  [H2M Default: 100] [HMW Default: uncapped]
    #[arg(short, long)]
    pub(crate) limit: Option<usize>,

    /// Specify a minimum number of players a server must have [Default: 0]
    #[arg(short, long, value_name = "MIN_PLAYERS", value_parser = value_parser!(u8).range(0..=MAX_H2M_CLIENT_NUM as i64))]
    pub(crate) player_min: Option<u8>,

    /// Specify a maximum team size [Default: 9]
    #[arg(short, long, value_name = "MAX_TEAM_SIZE", value_parser = value_parser!(u8).range(1..=MAX_H2M_TEAM_SIZE as i64))]
    pub(crate) team_size_max: Option<u8>,

    /// Server contains bot players
    #[arg(long, group = "bots")]
    pub(crate) with_bots: bool,

    /// Server does not contain bot players
    #[arg(long, group = "bots")]
    pub(crate) without_bots: bool,

    /// Include servers that do not respond to a 'getInfo' request
    #[arg(long, conflicts_with_all = [
        "excludes", "includes", "player_min", "team_size_max", "with_bots", "without_bots", "stats"
    ])]
    pub(crate) include_unresponsive: bool,

    /// Specify region(s) [Default: include all]
    #[arg(short, long, value_enum, num_args(1..=REGION_LEN))]
    pub(crate) region: Option<Vec<Region>>,

    /// Specify source(s) [Default: include all]
    #[arg(short, long, value_enum, num_args(1..=SOURCE_LEN))]
    pub(crate) source: Option<Vec<Source>>,

    /// Server name must contain any 1 of the following terms
    /// {n}  [Note: search terms are case-insensitive]
    #[arg(short, long, value_name = "STRING", num_args(1..))]
    pub(crate) includes: Option<Vec<String>>,

    /// Server name must not contain any 1 of the following terms
    /// {n}  [Note: exclude has higher priority] [Examples]
    /// {n}  [-e term1 term2] searches for "term1" or "term2"
    /// {n}  [-e "one long term"] searches for "one long term"
    #[arg(short, long, value_name = "STRING", num_args(1..))]
    pub(crate) excludes: Option<Vec<String>>,

    /// Specify a maximum number of 'getInfo' retries [Default: 3]
    #[arg(long, value_name = "MAX_ATTEMPTS", value_parser = value_parser!(u8).range(0..=RETRIES_MAX as i64))]
    pub(crate) retry_max: Option<u8>,

    /// Display statistics about the servers found in the current filter
    #[arg(short = 'S', long, alias = "verbose")]
    pub(crate) stats: bool,
}

pub(crate) const REGION_LEN: usize = 3;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug)]
pub(crate) enum Region {
    #[value(aliases(["Na", "nA", "NorthAmerica", "northAmerica", "northamerica"]))]
    NA,
    #[value(aliases(["Sa", "sA", "SouthAmerica", "southAmerica", "southamerica"]))]
    SA,
    #[value(aliases(["Eu", "eU", "Europe", "europe"]))]
    EU,
    #[value(aliases(["APAC", "Asia", "AsiaPacific", "asia", "pacific"]))]
    Apac,
}

pub(crate) const SOURCE_LEN: usize = 2;

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, ValueEnum, Debug)]
pub enum Source {
    #[value(alias = "iw4")]
    Iw4Master,
    #[value(alias = "hmw")]
    HmwMaster,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug)]
pub(crate) enum CacheCmd {
    /// Clears entire cache file including connection history then starts a fresh cache file
    #[value(aliases = ["Reset", "Clear", "clear"])]
    Reset,

    /// Updates all server names in the cache  
    /// {n}  Try this if 'reconnect' is returning: "Could not find server in cache"
    #[value(alias = "Update")]
    Update,
}
