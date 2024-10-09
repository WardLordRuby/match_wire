use crate::{
    cli::{REGION_LEN, SOURCE_LEN},
    utils::input::completion::{CommandScheme, InnerScheme, RecData, RecKind, ROOT},
};

// MARK: IMPROVE
// HARD: this ideally would be done by a proc-macro
impl CommandScheme {
    pub const fn init() -> Self {
        CommandScheme::new(
            RecData::new(
                None,
                Some(&COMMANDS_ALIAS),
                None,
                Some(&COMMAND_RECS),
                RecKind::Command,
                false,
            ),
            &COMMAND_INNER,
        )
    }
}

const COMMAND_RECS: [&str; 13] = [
    "filter",
    "reconnect",
    "launch",
    "cache",
    "console",
    "game-dir",
    "local-env",
    "quit",
    "version",
    "help",
    "logs",
    "gamedir",
    "localenv",
];
const COMMANDS_ALIAS: [(usize, usize); 3] = [(4, 10), (5, 11), (6, 12)];

const FILTER_RECS: [&str; 12] = [
    "limit",
    "player-min",
    "team-size-max",
    "region",
    "source",
    "includes",
    "excludes",
    "with-bots",
    "without-bots",
    "include-unresponsive",
    "retry-max",
    "help",
];
const FILTER_SHORT: [(usize, &str); 8] = [
    (0, "l"),
    (1, "p"),
    (2, "t"),
    (3, "r"),
    (4, "s"),
    (5, "i"),
    (6, "e"),
    (11, "h"),
];

const FILTER_REGIONS: [&str; 8] = [
    "na",
    "eu",
    "apac",
    "northamerica",
    "europe",
    "asia",
    "pacific",
    "asiapacific",
];
const FILTER_REGIONS_ALIAS: [(usize, usize); 5] = [(0, 3), (1, 4), (2, 5), (2, 6), (2, 7)];

const FILTER_SOURCE_RECS: [&str; 4] = ["iw4-master", "hmw-master", "iw4", "hmw"];
const FILTER_SOURCE_ALIAS: [(usize, usize); 2] = [(0, 2), (1, 3)];

const RECONNECT_RECS: [&str; 3] = ["history", "connect", "help"];
const RECONNECT_SHORT: [(usize, &str); 3] = [(0, "H"), (1, "c"), (2, "h")];

const CACHE_RECS: [&str; 3] = ["reset", "update", "clear"];
const CACHE_ALIAS: [(usize, usize); 1] = [(0, 2)];

const COMMAND_INNER: [InnerScheme; 10] = [
    // filter
    InnerScheme::new(
        RecData::new(
            Some(ROOT),
            None,
            Some(&FILTER_SHORT),
            Some(&FILTER_RECS),
            RecKind::Argument,
            false,
        ),
        Some(&FILTER_INNER),
    ),
    // reconnect
    InnerScheme::new(
        RecData::new(
            Some(ROOT),
            None,
            Some(&RECONNECT_SHORT),
            Some(&RECONNECT_RECS),
            RecKind::Argument,
            false,
        ),
        Some(&RECONNECT_INNTER),
    ),
    // launch
    InnerScheme::end(ROOT),
    // cache
    InnerScheme::new(
        RecData::new(
            Some(ROOT),
            Some(&CACHE_ALIAS),
            None,
            Some(&CACHE_RECS),
            RecKind::value_with_num_args(1),
            true,
        ),
        None,
    ),
    // game-console
    InnerScheme::end(ROOT),
    // game-dir
    InnerScheme::end(ROOT),
    // local-env
    InnerScheme::end(ROOT),
    // quit
    InnerScheme::end(ROOT),
    // version
    InnerScheme::end(ROOT),
    // help
    InnerScheme::help(),
];

const FILTER_INNER: [InnerScheme; 12] = [
    // limit
    InnerScheme::empty_with("filter", RecKind::user_defined_with_num_args(1), false),
    // player-min
    InnerScheme::empty_with("filter", RecKind::user_defined_with_num_args(1), false),
    // team-size-max
    InnerScheme::empty_with("filter", RecKind::user_defined_with_num_args(1), false),
    // region
    InnerScheme::new(
        RecData::new(
            Some("filter"),
            Some(&FILTER_REGIONS_ALIAS),
            None,
            Some(&FILTER_REGIONS),
            RecKind::value_with_num_args(REGION_LEN),
            false,
        ),
        None,
    ),
    // source
    InnerScheme::new(
        RecData::new(
            Some("filter"),
            Some(&FILTER_SOURCE_ALIAS),
            None,
            Some(&FILTER_SOURCE_RECS),
            RecKind::value_with_num_args(SOURCE_LEN),
            false,
        ),
        None,
    ),
    // includes
    InnerScheme::empty_with(
        "filter",
        RecKind::user_defined_with_num_args(usize::MAX),
        false,
    ),
    // excludes
    InnerScheme::empty_with(
        "filter",
        RecKind::user_defined_with_num_args(usize::MAX),
        false,
    ),
    // with-bots
    InnerScheme::flag("filter", false),
    // without-bots
    InnerScheme::flag("filter", false),
    // include-unresponsive
    InnerScheme::flag("filter", false),
    // retry-max
    InnerScheme::empty_with("filter", RecKind::user_defined_with_num_args(1), false),
    // help
    InnerScheme::help(),
];

const RECONNECT_INNTER: [InnerScheme; 3] = [
    // history
    InnerScheme::end("reconnect"),
    // connect
    InnerScheme::empty_with("reconnect", RecKind::user_defined_with_num_args(1), true),
    // help
    InnerScheme::help(),
];
