use crate::{commands::reconnect::HISTORY_MAX, H2M_MAX_CLIENT_NUM, H2M_MAX_TEAM_SIZE};
use clap::{value_parser, ArgAction, Args, Parser, Subcommand, ValueEnum};

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
pub struct UserCommand {
    #[command(subcommand)]
    pub command: Command,
}

#[derive(Subcommand, Debug)]
pub enum Command {
    /// Create a new favorites.json using various filter options
    /// {n}  Using no arguments will take the top 100 servers with highest playercounts
    #[command(alias = "Filter")]
    Filter {
        #[command(flatten)]
        args: Option<Filters>,
    },

    /// Reconnect to last server joined
    #[command(alias = "Reconnect")]
    Reconnect {
        #[clap(flatten)]
        args: HistoryArgs,
    },

    /// Launch H2M-Mod
    #[command(alias = "Launch")]
    Launch,

    /// Clear and rebuild the internal server cache list
    /// {n}  Try this if 'reconnect' is returning: "Could not find server in cache"
    #[command(aliases(["Reset", "reset", "update", "Update"]))]
    UpdateCache,

    /// Print H2M console logs
    #[command(aliases(["Display", "display", "logs", "Logs"]))]
    DisplayLogs,

    /// Open MWR(2017) directory
    #[command(aliases(["Gamedir", "gamedir", "GameDir"]))]
    GameDir,

    /// Quit the program
    #[command(alias = "Quit")]
    Quit,

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
    #[arg(short, long, value_parser = value_parser!(u8).range(1..=HISTORY_MAX))]
    pub connect: Option<u8>,
}

#[derive(Args, Debug, Clone, Default)]
pub struct Filters {
    /// Specify the maximum number of servers added to favorites.json
    /// {n}  [Note: current server-browser gets buggy after 100] [Default: 100]
    #[arg(short, long)]
    pub limit: Option<usize>,

    /// Specify a minimum number of players a server must have [Default: 0]
    #[arg(short, long, value_parser = value_parser!(u8).range(0..=H2M_MAX_CLIENT_NUM))]
    pub player_min: Option<u8>,

    /// Specify a maximum team size [Default: 9]
    #[arg(short, long, value_parser = value_parser!(u8).range(1..=H2M_MAX_TEAM_SIZE))]
    pub team_size_max: Option<u8>,

    /// Specify region [Default: include all regions]
    #[arg(short, long, value_enum)]
    pub region: Option<Region>,

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
