use super::cli::{REGION_LEN, SOURCE_LEN};

use repl_oxide::completion::{CommandScheme, InnerScheme, Parent, RecData, RecKind};

pub const COMPLETION: CommandScheme = init_command_scheme();

// MARK: IMPROVE
// HARD: this ideally would be done by a proc-macro
const fn init_command_scheme() -> CommandScheme {
    CommandScheme::new(
        RecData::command_set(Some(&COMMANDS_ALIAS), Some(&COMMAND_RECS), false),
        &COMMAND_INNER,
    )
}

const COMMAND_RECS: [&str; 12] = [
    "filter",
    "reconnect",
    "launch",
    "cache",
    "console",
    "game-dir",
    "local-env",
    "quit",
    "version",
    "logs",
    "gamedir",
    "localenv",
];
const COMMANDS_ALIAS: [(usize, usize); 3] = [(4, 9), (5, 10), (6, 11)];

const FILTER_RECS: [&str; 11] = [
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
];
const FILTER_SHORT: [(usize, &str); 7] = [
    (0, "l"),
    (1, "p"),
    (2, "t"),
    (3, "r"),
    (4, "s"),
    (5, "i"),
    (6, "e"),
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

const RECONNECT_RECS: [&str; 2] = ["history", "connect"];
const RECONNECT_SHORT: [(usize, &str); 2] = [(0, "H"), (1, "c")];

const CACHE_RECS: [&str; 3] = ["reset", "update", "clear"];
const CACHE_ALIAS: [(usize, usize); 1] = [(0, 2)];

const CONSOLE_RECS: [&str; 1] = ["all"];

const COMMAND_INNER: [InnerScheme; 9] = [
    // filter
    InnerScheme::new(
        RecData::new(
            Parent::Root,
            None,
            Some(&FILTER_SHORT),
            Some(&FILTER_RECS),
            RecKind::argument_with_no_required_inputs(),
            false,
        ),
        Some(&FILTER_INNER),
    ),
    // reconnect
    InnerScheme::new(
        RecData::new(
            Parent::Root,
            None,
            Some(&RECONNECT_SHORT),
            Some(&RECONNECT_RECS),
            RecKind::argument_with_no_required_inputs(),
            false,
        ),
        Some(&RECONNECT_INNTER),
    ),
    // launch
    InnerScheme::end(Parent::Root),
    // cache
    InnerScheme::new(
        RecData::new(
            Parent::Root,
            Some(&CACHE_ALIAS),
            None,
            Some(&CACHE_RECS),
            RecKind::value_with_num_args(1),
            false,
        ),
        None,
    ),
    // console
    InnerScheme::new(
        RecData::new(
            Parent::Root,
            None,
            None,
            Some(&CONSOLE_RECS),
            RecKind::argument_with_no_required_inputs(),
            false,
        ),
        Some(&CONSOLE_INNER),
    ), // game-dir
    InnerScheme::end(Parent::Root),
    // local-env
    InnerScheme::end(Parent::Root),
    // quit
    InnerScheme::end(Parent::Root),
    // version
    InnerScheme::end(Parent::Root),
];

const FILTER_INNER: [InnerScheme; 11] = [
    // limit
    InnerScheme::empty_with(
        Parent::Entry(COMMAND_RECS[0]),
        RecKind::user_defined_with_num_args(1),
        false,
    ),
    // player-min
    InnerScheme::empty_with(
        Parent::Entry(COMMAND_RECS[0]),
        RecKind::user_defined_with_num_args(1),
        false,
    ),
    // team-size-max
    InnerScheme::empty_with(
        Parent::Entry(COMMAND_RECS[0]),
        RecKind::user_defined_with_num_args(1),
        false,
    ),
    // region
    InnerScheme::new(
        RecData::new(
            Parent::Entry(COMMAND_RECS[0]),
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
            Parent::Entry(COMMAND_RECS[0]),
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
        Parent::Entry(COMMAND_RECS[0]),
        RecKind::user_defined_with_num_args(usize::MAX),
        false,
    ),
    // excludes
    InnerScheme::empty_with(
        Parent::Entry(COMMAND_RECS[0]),
        RecKind::user_defined_with_num_args(usize::MAX),
        false,
    ),
    // with-bots
    InnerScheme::flag(Parent::Entry(COMMAND_RECS[0]), false),
    // without-bots
    InnerScheme::flag(Parent::Entry(COMMAND_RECS[0]), false),
    // include-unresponsive
    InnerScheme::flag(Parent::Entry(COMMAND_RECS[0]), false),
    // retry-max
    InnerScheme::empty_with(
        Parent::Entry(COMMAND_RECS[0]),
        RecKind::user_defined_with_num_args(1),
        false,
    ),
];

const RECONNECT_INNTER: [InnerScheme; 2] = [
    // history
    InnerScheme::end(Parent::Entry(COMMAND_RECS[1])),
    // connect
    InnerScheme::empty_with(
        Parent::Entry(COMMAND_RECS[1]),
        RecKind::user_defined_with_num_args(1),
        true,
    ),
];

const CONSOLE_INNER: [InnerScheme; 1] = [
    // all
    InnerScheme::end(Parent::Entry(COMMAND_RECS[4])),
];
