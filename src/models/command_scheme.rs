use super::cli::{REGION_LEN, SOURCE_LEN};
use crate::{H2M_MAX_CLIENT_NUM, H2M_MAX_TEAM_SIZE, main_thread_state};

use repl_oxide::completion::{CommandScheme, InnerScheme, Parent, RecData, RecKind};

// MARK: IMPROVE
// HARD: this ideally would be done by a proc-macro
pub const COMPLETION: &CommandScheme = &init_command_scheme();

const COMMAND_RECS: &[&str] = &[
    "filter",
    "reconnect",
    "launch",
    "cache",
    "console",
    "game-dir",
    "local-env",
    "quit",
    "version",
    "last",
    "settings",
    "logs",
    "gamedir",
    "localenv",
];
const COMMANDS_ALIAS: &[(usize, usize)] = &[(4, 11), (5, 12), (6, 13)];

const fn init_command_scheme() -> CommandScheme {
    CommandScheme::new(
        RecData::new(RecKind::Command)
            .with_recommendations(COMMAND_RECS)
            .with_alias(COMMANDS_ALIAS),
        COMMAND_INNER,
    )
}

const FILTER_RECS: &[&str] = &[
    "limit",
    "player-min",
    "team-size-max",
    "region",
    "source",
    "includes",
    "excludes",
    "stats",
    "with-bots",
    "without-bots",
    "include-unresponsive",
    "retry-max",
    "verbose",
];
const FILTER_SHORT: &[(usize, &str)] = &[
    (0, "l"),
    (1, "p"),
    (2, "t"),
    (3, "r"),
    (4, "s"),
    (5, "i"),
    (6, "e"),
    (7, "S"),
];
const FILTER_ALIAS: &[(usize, usize)] = &[(7, 12)];

const RECONNECT_RECS: &[&str] = &["history", "connect", "queue", "abort"];
const RECONNECT_SHORT: &[(usize, &str)] = &[(0, "H"), (1, "c"), (2, "q")];

const CACHE_RECS: &[&str] = &["reset", "update", "clear"];
const CACHE_ALIAS: &[(usize, usize)] = &[(0, 2)];

const CONSOLE_RECS: &[&str] = &["all"];

const COMMAND_INNER: &[InnerScheme] = &[
    // filter
    InnerScheme::new(
        RecData::new(RecKind::argument_with_no_required_inputs())
            .with_parent(Parent::Root)
            .with_recommendations(FILTER_RECS)
            .with_alias(FILTER_ALIAS)
            .with_short(FILTER_SHORT),
        Some(FILTER_INNER),
    ),
    // reconnect
    InnerScheme::new(
        RecData::new(RecKind::argument_with_no_required_inputs())
            .with_parent(Parent::Root)
            .with_recommendations(RECONNECT_RECS)
            .with_short(RECONNECT_SHORT),
        Some(RECONNECT_INNER),
    ),
    // launch
    InnerScheme::end(Parent::Root),
    // cache
    InnerScheme::new(
        RecData::new(RecKind::value_with_num_args(1))
            .with_parent(Parent::Root)
            .with_recommendations(CACHE_RECS)
            .with_alias(CACHE_ALIAS),
        None,
    ),
    // console
    InnerScheme::new(
        RecData::new(RecKind::argument_with_no_required_inputs())
            .with_parent(Parent::Root)
            .with_recommendations(CONSOLE_RECS),
        Some(CONSOLE_INNER),
    ),
    // game-dir
    InnerScheme::end(Parent::Root),
    // local-env
    InnerScheme::end(Parent::Root),
    // quit
    InnerScheme::end(Parent::Root),
    // version
    InnerScheme::new(
        RecData::new(RecKind::argument_with_no_required_inputs())
            .with_parent(Parent::Root)
            .with_recommendations(VERSION_RECS)
            .with_alias(VERSION_ALIAS),
        Some(VERSION_INNER),
    ),
    // last
    InnerScheme::new(
        RecData::new(RecKind::argument_with_no_required_inputs())
            .with_parent(Parent::Root)
            .with_recommendations(LAST_RECS),
        Some(LAST_INNER),
    ),
    // settings
    InnerScheme::new(
        RecData::new(RecKind::argument_with_no_required_inputs())
            .with_parent(Parent::Root)
            .with_recommendations(SETTINGS_RECS),
        Some(SETTINGS_INNER),
    ),
];

const FILTER_REGIONS: &[&str] = &[
    "na",
    "sa",
    "eu",
    "apac",
    "northamerica",
    "southamerica",
    "europe",
    "asia",
    "pacific",
    "asiapacific",
];
const FILTER_REGIONS_ALIAS: &[(usize, usize)] = &[(0, 4), (1, 5), (2, 6), (3, 7), (3, 8), (3, 9)];
const FILTER_SOURCE_RECS: &[&str] = &["iw4-master", "hmw-master", "iw4", "hmw"];
const FILTER_SOURCE_ALIAS: &[(usize, usize)] = &[(0, 2), (1, 3)];

fn search_term_parser(value: &str) -> bool {
    !value.starts_with('-')
}

fn is_unsigned(value: &str) -> bool {
    value.parse::<usize>().is_ok()
}

fn u8_bounds(value: &str, valid: impl Fn(u8) -> bool) -> bool {
    value.parse::<u8>().is_ok_and(valid)
}

const FILTER_INNER: &[InnerScheme] = &[
    // limit
    InnerScheme::user_defined(1)
        .with_parent(Parent::Entry(COMMAND_RECS[0]))
        .with_parsing_rule(is_unsigned),
    // player-min
    InnerScheme::user_defined(1)
        .with_parent(Parent::Entry(COMMAND_RECS[0]))
        .with_parsing_rule(|value| u8_bounds(value, |v| v <= H2M_MAX_CLIENT_NUM)),
    // team-size-max
    InnerScheme::user_defined(1)
        .with_parent(Parent::Entry(COMMAND_RECS[0]))
        .with_parsing_rule(|value| u8_bounds(value, |v| v > 0 && v <= H2M_MAX_TEAM_SIZE)),
    // region
    InnerScheme::new(
        RecData::new(RecKind::value_with_num_args(REGION_LEN))
            .with_parent(Parent::Entry(COMMAND_RECS[0]))
            .with_recommendations(FILTER_REGIONS)
            .with_alias(FILTER_REGIONS_ALIAS)
            .without_help(),
        None,
    ),
    // source
    InnerScheme::new(
        RecData::new(RecKind::value_with_num_args(SOURCE_LEN))
            .with_parent(Parent::Entry(COMMAND_RECS[0]))
            .with_recommendations(FILTER_SOURCE_RECS)
            .with_alias(FILTER_SOURCE_ALIAS)
            .without_help(),
        None,
    ),
    // includes
    InnerScheme::user_defined(usize::MAX)
        .with_parent(Parent::Entry(COMMAND_RECS[0]))
        .with_parsing_rule(search_term_parser),
    // excludes
    InnerScheme::user_defined(usize::MAX)
        .with_parent(Parent::Entry(COMMAND_RECS[0]))
        .with_parsing_rule(search_term_parser),
    // stats
    InnerScheme::flag().with_parent(Parent::Entry(COMMAND_RECS[0])),
    // with-bots
    InnerScheme::flag().with_parent(Parent::Entry(COMMAND_RECS[0])),
    // without-bots
    InnerScheme::flag().with_parent(Parent::Entry(COMMAND_RECS[0])),
    // include-unresponsive
    InnerScheme::flag().with_parent(Parent::Entry(COMMAND_RECS[0])),
    // retry-max
    InnerScheme::user_defined(1)
        .with_parent(Parent::Entry(COMMAND_RECS[0]))
        .with_parsing_rule(is_unsigned),
];

const RECONNECT_INNER: &[InnerScheme] = &[
    // history
    InnerScheme::end(Parent::Entry(COMMAND_RECS[1])),
    // connect
    InnerScheme::user_defined(1)
        .with_parent(Parent::Entry(COMMAND_RECS[1]))
        .with_parsing_rule(|value| {
            u8_bounds(value, |v| {
                v > 0 && v <= main_thread_state::Cache::history_len()
            })
        }),
    // queue
    InnerScheme::flag().with_parent(Parent::Entry(COMMAND_RECS[1])),
    // abort
    InnerScheme::flag().with_parent(Parent::Entry(COMMAND_RECS[1])),
];

const CONSOLE_INNER: &[InnerScheme] = &[
    // all
    InnerScheme::end(Parent::Entry(COMMAND_RECS[4])),
];

const VERSION_RECS: &[&str] = &["verify-all", "verify"];
const VERSION_ALIAS: &[(usize, usize)] = &[(0, 1)];

const VERSION_INNER: &[InnerScheme] = &[
    // verify-all
    InnerScheme::end(Parent::Entry(COMMAND_RECS[8])),
];

const LAST_RECS: &[&str] = &["refresh"];

const LAST_INNER: &[InnerScheme] = &[
    // refresh
    InnerScheme::end(Parent::Entry(COMMAND_RECS[9])),
];

const SETTINGS_RECS: &[&str] = &["use-default"];

const SETTINGS_INNER: &[InnerScheme] = &[
    // use-default
    InnerScheme::end(Parent::Entry(COMMAND_RECS[10])),
];
