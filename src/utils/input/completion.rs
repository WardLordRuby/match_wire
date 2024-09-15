use crate::utils::input::line::LineReader;
use std::{collections::HashMap, io};

const NO_PARENT: &str = "NULL";
const UNIVERSAL: &str = "ANY";

const USER_INPUT: i8 = -1;
const COMMANDS: usize = 0;
const INVALID: usize = 1;

// MARK: IMPROVE
// we could solve name-space collisions by making the data structure into a tree

/// The current implementation only works when the name-space of commands, arguments, and fixed values do not overlap  
/// overlaping names must return the exact same `RecData`, help is special cased to work as both a command and argument  
/// `inner` must ALWAYS contain the same number of elements as `RecData.starting_alias`  
pub struct CommandScheme {
    /// command names followed by aliases
    commands: RecData,

    /// static empty node used for invalid inputs
    empty: RecData,

    /// inner data shares indices with `commands.recs`
    inner: Vec<InnerScheme>,
}

/// `inner` must adhere to the following
///  - if kind is `Kind::Argument` `inner` must contain the same number of elements as `RecData.starting_alias`  
///  - if kind is `Kind::Value` `inner` must contain the same number of elements as `RecData.recs` && `inner.inner` must be `None`  
///  - for all other kinds Inner must be `None`
#[derive(Clone)]
struct InnerScheme {
    /// data that describes recomendations context
    data: RecData,

    /// inner data shares indices with `data.recs`
    inner: Option<Vec<InnerScheme>>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct RecData {
    /// name of the parent entry
    parent: &'static str,
    /// the starting index of aliases
    starting_alias: usize,
    /// recomendations followed by recomendation aliases
    recs: Vec<&'static str>,
    /// kind of data stored
    kind: RecKind,
    /// signals no commands can come after
    end: bool,
}

impl RecData {
    fn ending_rec(parent: &'static str) -> Self {
        RecData {
            parent,
            starting_alias: 0,
            recs: Vec::new(),
            kind: RecKind::Null,
            end: true,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum RecKind {
    Command,
    Argument,
    Value,
    ValidInput,
    UserDefined,
    Help,
    Null,
}

impl InnerScheme {
    fn empty_with(parent: &'static str, kind: RecKind, end: bool) -> Self {
        InnerScheme {
            data: RecData {
                parent,
                starting_alias: 0,
                recs: Vec::new(),
                kind,
                end,
            },
            inner: None,
        }
    }
    fn help() -> Self {
        InnerScheme {
            data: RecData {
                parent: UNIVERSAL,
                starting_alias: 0,
                recs: Vec::new(),
                kind: RecKind::Help,
                end: true,
            },
            inner: None,
        }
    }
    fn ending_rec(parent: &'static str) -> Self {
        InnerScheme {
            data: RecData::ending_rec(parent),
            inner: None,
        }
    }
}

// MARK: IMPROVE
// HARD: this ideally would be done by a proc-macro
pub fn init_completion() -> CommandScheme {
    CommandScheme {
        commands: RecData {
            parent: NO_PARENT,
            starting_alias: 10,
            recs: vec![
                "filter",
                "reconnect",
                "launch",
                "update-cache",
                "display-logs",
                "game-dir",
                "local-env",
                "quit",
                "version",
                "help",
                "update",
                "reset",
                "display",
                "logs",
                "gamedir",
                "localenv",
            ],
            kind: RecKind::Command,
            end: false,
        },
        empty: RecData::ending_rec(NO_PARENT),
        inner: vec![
            // filter
            InnerScheme {
                data: RecData {
                    parent: "ROOT",
                    starting_alias: 7,
                    recs: vec![
                        "limit",
                        "player-min",
                        "team-size-max",
                        "region",
                        "includes",
                        "excludes",
                        "help",
                    ],
                    kind: RecKind::Argument,
                    end: false,
                },
                inner: Some(vec![
                    // limit
                    InnerScheme::empty_with("filter", RecKind::UserDefined, false),
                    // player-min
                    InnerScheme::empty_with("filter", RecKind::UserDefined, false),
                    // team-size-max
                    InnerScheme::empty_with("filter", RecKind::UserDefined, false),
                    // region
                    InnerScheme {
                        data: RecData {
                            parent: "filter",
                            starting_alias: 3,
                            recs: vec![
                                "na",
                                "eu",
                                "apac",
                                "northamerica",
                                "europe",
                                "asia",
                                "pacific",
                                "asiapacific",
                            ],
                            kind: RecKind::Value,
                            end: false,
                        },
                        inner: Some(vec![
                            // valid regions
                            InnerScheme::empty_with(
                                "region",
                                RecKind::ValidInput,
                                false
                            );
                            8
                        ]),
                    },
                    // includes
                    InnerScheme::empty_with("filter", RecKind::UserDefined, false),
                    // excludes
                    InnerScheme::empty_with("filter", RecKind::UserDefined, false),
                    // help
                    InnerScheme::help(),
                ]),
            },
            // reconnect
            InnerScheme {
                data: RecData {
                    parent: "ROOT",
                    starting_alias: 3,
                    recs: vec!["history", "connect", "help"],
                    kind: RecKind::Argument,
                    end: false,
                },
                inner: Some(vec![
                    // history
                    InnerScheme::ending_rec("reconnect"),
                    // connect
                    InnerScheme::empty_with("reconnect", RecKind::UserDefined, true),
                    // help
                    InnerScheme::help(),
                ]),
            },
            // launch
            InnerScheme::ending_rec("ROOT"),
            // update-cache
            InnerScheme::ending_rec("ROOT"),
            // display-logs
            InnerScheme::ending_rec("ROOT"),
            // game-dir
            InnerScheme::ending_rec("ROOT"),
            // local-env
            InnerScheme::ending_rec("ROOT"),
            // quit
            InnerScheme::ending_rec("ROOT"),
            // version
            InnerScheme::ending_rec("ROOT"),
            // help
            InnerScheme::help(),
        ],
    }
}

pub struct Completion {
    recomendations: Vec<&'static str>,
    rec_i: i8,
    input: CompletionState,
    curr_val: usize,
    rec_map: HashMap<&'static str, usize>,
    rec_list: Vec<&'static RecData>,
}

#[derive(Default, Debug)]
struct CompletionState {
    curr_command: Option<String>,
    curr_argument: Option<String>,
    user_input: String,
}

impl Completion {
    fn last_key(&self) -> Option<&String> {
        self.input
            .curr_argument
            .as_ref()
            .or(self.input.curr_command.as_ref())
    }

    fn command_valid(&self) -> Option<usize> {
        self.input.curr_command.as_ref().map(|command| {
            if command.starts_with('-') {
                INVALID
            } else {
                self.rec_map.get(command.as_str()).map_or(INVALID, |&i| i)
            }
        })
    }

    fn curr_argment_valid(&self) -> Option<usize> {
        self.input.curr_argument.as_ref().map(|arg| {
            self.rec_map
                .get(arg.trim_start_matches('-'))
                .map_or(INVALID, |&i| {
                    if self.rec_list[i].parent == UNIVERSAL
                        || self.rec_list[i].parent
                            == self
                                .input
                                .curr_command
                                .as_ref()
                                .expect("can only set arg if command is valid")
                    {
                        i
                    } else {
                        INVALID
                    }
                })
        })
    }
}

impl From<&'static CommandScheme> for Completion {
    fn from(value: &'static CommandScheme) -> Self {
        fn walk_inner(
            inner: &'static InnerScheme,
            list: &mut Vec<&RecData>,
            map: &mut HashMap<&str, usize>,
        ) {
            match inner.data {
                RecData {
                    starting_alias,
                    ref recs,
                    kind: RecKind::Argument,
                    ..
                } => {
                    assert_eq!(starting_alias, inner.inner.as_ref().unwrap().len());
                    for (&argument, inner) in recs
                        .iter()
                        .zip(inner.inner.as_ref().expect("is some"))
                        .take(starting_alias)
                    {
                        list.push(&inner.data);
                        assert!(match map.insert(argument, list.len() - 1,) {
                            None => true,
                            Some(j) => *list[j] == inner.data,
                        });
                        walk_inner(inner, list, map);
                    }
                }
                RecData {
                    ref recs,
                    kind: RecKind::Value,
                    ..
                } => {
                    assert_eq!(recs.len(), inner.inner.as_ref().unwrap().len());
                    for (&value, inner) in recs.iter().zip(inner.inner.as_ref().expect("is some")) {
                        list.push(&inner.data);
                        assert!(match map.insert(value, list.len() - 1,) {
                            None => true,
                            Some(j) => *list[j] == inner.data,
                        });
                        assert!(inner.inner.is_none());
                    }
                }
                _ => assert!(inner.inner.is_none()),
            }
        }

        assert_eq!(value.commands.starting_alias, value.inner.len());
        let mut rec_map = HashMap::new();
        let mut rec_list = Vec::new();
        rec_list.push(&value.commands);
        rec_list.push(&value.empty);
        for (&command, inner) in value
            .commands
            .recs
            .iter()
            .zip(value.inner.iter())
            .take(value.commands.starting_alias)
        {
            rec_list.push(&inner.data);
            assert!(match rec_map.insert(command, rec_list.len() - 1) {
                None => true,
                Some(j) => *rec_list[j] == inner.data,
            });
            walk_inner(inner, &mut rec_list, &mut rec_map);
        }
        Completion {
            recomendations: value.commands.recs[..value.commands.starting_alias].to_vec(),
            rec_i: USER_INPUT,
            input: CompletionState::default(),
            rec_map,
            rec_list,
            curr_val: COMMANDS,
        }
    }
}

// MARK: TODO
// port in open quote error from style
// remove error code from style after

macro_rules! any_false {
    ($($x:expr),+ $(,)?) => {
        !($($x)&&+)
    };
}

trait Validity {
    fn as_bool(&self) -> bool;
}

impl Validity for Option<usize> {
    fn as_bool(&self) -> bool {
        !matches!(self, Some(i) if *i == INVALID)
    }
}

impl LineReader<'_> {
    pub fn update_completeion(&mut self) {
        let line_trim_start = self.line.input().trim_start();
        if line_trim_start.is_empty() {
            self.reset_completion();
            return;
        }
        if let RecData { end: true, .. } = self.completion.rec_list[self.completion.curr_val] {
            if self.line.input().to_lowercase().contains(
                self.completion
                    .last_key()
                    .expect("state is managed correctly"),
            ) {
                return;
            }
        }
        self.completion.rec_i = USER_INPUT;
        self.completion.input.curr_command = line_trim_start
            .split_once(' ')
            .map(|split| split.0.to_lowercase());

        let command_valid = self.completion.command_valid();

        self.completion.input.user_input =
            if let Some((beginning, ending_token)) = line_trim_start.rsplit_once(' ') {
                if command_valid.as_bool() {
                    self.completion.input.curr_argument = beginning
                        .split_whitespace()
                        .next_back()
                        .and_then(|arg| arg.starts_with('-').then_some(arg.to_lowercase()))
                }
                ending_token
            } else {
                line_trim_start
            }
            .trim_start_matches('-')
            .to_string();

        let arg_valid = self.completion.curr_argment_valid();

        self.completion.curr_val = {
            match (command_valid, arg_valid) {
                (Some(i), Some(j)) if i != INVALID && j != INVALID => j,
                (Some(i), None) if i != INVALID => i,
                (None, None) => COMMANDS,
                _ => INVALID,
            }
            // MARK: TODOS

            // special case ValidInput

            // errors for missing user-input

            // error for invalid Values
        };
        let rec_data = self.completion.rec_list[self.completion.curr_val];

        self.line
            .found_err(any_false!(command_valid.as_bool(), arg_valid.as_bool()));

        if rec_data.recs.is_empty() {
            self.completion.recomendations = Vec::new();
            return;
        }

        if self.completion.input.user_input.is_empty() {
            self.completion.recomendations = rec_data.recs[..rec_data.starting_alias].to_vec();
            return;
        }

        let input_lower = self.completion.input.user_input.to_lowercase();
        let mut recomendations = rec_data
            .recs
            .iter()
            .filter(|rec| rec.contains(&input_lower))
            .copied()
            .collect::<Vec<_>>();
        recomendations.sort_unstable_by(|a, b| {
            let a_starts = a.starts_with(&input_lower);
            let b_starts = b.starts_with(&input_lower);
            b_starts.cmp(&a_starts)
        });
        self.completion.recomendations = recomendations;
    }

    pub fn try_completion(&mut self, by: i8) -> io::Result<()> {
        if self.completion.recomendations.is_empty() {
            return Ok(());
        }

        let last = if self.completion.rec_i == USER_INPUT {
            &self.completion.input.user_input
        } else {
            self.completion.recomendations[self.completion.rec_i as usize]
        };

        self.completion.rec_i += by;
        match self.completion.rec_i {
            i if i >= USER_INPUT && i < self.completion.recomendations.len() as i8 => (),
            ..USER_INPUT => self.completion.rec_i = self.completion.recomendations.len() as i8 - 1,
            _ => self.completion.rec_i = USER_INPUT,
        }

        let new_line = {
            let recomendation = if self.completion.rec_i == USER_INPUT {
                &self.completion.input.user_input
            } else {
                self.completion.recomendations[self.completion.rec_i as usize]
            };

            let format_arg = |last: &str, recomendation: &str| -> String {
                format!(
                    "{}{}{recomendation}",
                    self.line
                        .input()
                        .trim_end_matches(last)
                        .trim_end_matches('-'),
                    if recomendation.is_empty() { "" } else { "--" }
                )
            };

            let format_value = |recomendation: &str| -> String {
                self.line.input().rsplit_once(' ').map_or_else(
                    || recomendation.to_string(),
                    |split| format!("{} {recomendation}", split.0),
                )
            };

            match self.completion.rec_list[self.completion.curr_val] {
                RecData {
                    kind: RecKind::Argument,
                    ..
                } => format_arg(last, recomendation),
                RecData {
                    kind: RecKind::Value | RecKind::Command,
                    ..
                } => format_value(recomendation),
                RecData {
                    kind: RecKind::Help,
                    ..
                } => {
                    if self.completion.input.curr_command.is_some() {
                        format_arg(last, recomendation)
                    } else {
                        format_value(recomendation)
                    }
                }
                RecData {
                    kind: RecKind::UserDefined | RecKind::ValidInput | RecKind::Null,
                    ..
                } => unreachable!(),
            }
        };
        self.change_line(new_line)
    }

    pub fn reset_completion(&mut self) {
        self.completion.input.curr_command = None;
        self.completion.input.curr_argument = None;
        self.completion.curr_val = COMMANDS;
        let commands = self.completion.rec_list[COMMANDS];
        self.completion.recomendations = commands.recs[..commands.starting_alias].to_vec();
        self.completion.input.user_input.clear();
        self.completion.rec_i = USER_INPUT;
    }
}
