use crate::utils::input::line::LineReader;
use std::{collections::HashMap, io};

/// The current implementation only works when the name-space of commands, arguments, and fixed values do not overlap  
/// overlaping names must return the exact same `Option<RecData>`, `inner` must ALWAYS contain the same number of  
/// elements as `RecData.starting_alias`
pub struct CommandScheme {
    /// command names followed by aliases
    commands: RecData,

    /// inner data shares indices with `commands.recs`
    inner: Vec<InnerScheme>,
}

/// `inner` must contain the same number of elements as `RecData.starting_alias` if kind is set as `Kind::Argument`
#[derive(Clone)]
struct InnerScheme {
    data: Option<RecData>,

    /// inner data shares indices with `Some(data.recs)`
    inner: Option<Vec<InnerScheme>>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct RecData {
    /// the starting index of aliases
    starting_alias: usize,
    /// recomendations followed by recomendation aliases
    recs: Vec<&'static str>,
    /// kind of data stored
    kind: RecKind,
    /// signals no commands can come after
    end: bool,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum RecKind {
    Command,
    Argument,
    Value,
    UserDefined,
}

impl InnerScheme {
    fn none() -> Self {
        InnerScheme {
            data: None,
            inner: None,
        }
    }
    fn user_defined() -> Self {
        InnerScheme {
            data: Some(RecData {
                starting_alias: 0,
                recs: Vec::new(),
                kind: RecKind::UserDefined,
                end: false,
            }),
            inner: None,
        }
    }
    fn ending_rec() -> Self {
        InnerScheme {
            data: Some(RecData {
                starting_alias: 0,
                recs: Vec::new(),
                kind: RecKind::UserDefined,
                end: true,
            }),
            inner: None,
        }
    }
}

// MARK: IMPROVE
// HARD: this ideally would be done by a proc-macro
pub fn init_completion() -> CommandScheme {
    CommandScheme {
        commands: RecData {
            starting_alias: 9,
            recs: vec![
                "filter",
                "reconnect",
                "launch",
                "update-cache",
                "display-logs",
                "game-dir",
                "quit",
                "local-env",
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
        inner: vec![
            // filter
            InnerScheme {
                data: Some(RecData {
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
                }),
                inner: Some(vec![
                    // limit
                    InnerScheme::user_defined(),
                    // player-min
                    InnerScheme::user_defined(),
                    // team-size-max
                    InnerScheme::user_defined(),
                    // region
                    InnerScheme {
                        data: Some(RecData {
                            starting_alias: 3,
                            recs: vec![
                                "na",
                                "eu",
                                "apac",
                                "northamerica",
                                "europe",
                                "asia",
                                "pacific",
                            ],
                            kind: RecKind::Value,
                            end: false,
                        }),
                        inner: None,
                    },
                    // includes
                    InnerScheme::user_defined(),
                    // excludes
                    InnerScheme::user_defined(),
                    // help
                    InnerScheme::ending_rec(),
                ]),
            },
            // reconnect
            InnerScheme {
                data: Some(RecData {
                    starting_alias: 2,
                    recs: vec!["history", "connect"],
                    kind: RecKind::Argument,
                    end: false,
                }),
                inner: Some(vec![InnerScheme::ending_rec(); 2]),
            },
            // launch
            InnerScheme::none(),
            // update-cache
            InnerScheme::none(),
            // display-logs
            InnerScheme::none(),
            // game-dir
            InnerScheme::none(),
            // quit
            InnerScheme::none(),
            // local-env
            InnerScheme::none(),
            // help
            InnerScheme::ending_rec(),
        ],
    }
}

const USER_INPUT: i8 = -1;
const COMMANDS: usize = 0;

pub struct Completion {
    recomendations: Vec<&'static str>,
    rec_i: i8,
    input: CompletionState,
    curr_val: usize,
    rec_map: HashMap<&'static str, usize>,
    rec_list: Vec<Option<&'static RecData>>,
}

#[derive(Default)]
struct CompletionState {
    curr_command: Option<String>,
    curr_argument: Option<String>,
    user_input: String,
}

impl Completion {
    fn get_last_key(&self) -> Option<&String> {
        self.input
            .curr_argument
            .as_ref()
            .or(self.input.curr_command.as_ref())
    }
}

impl From<&'static CommandScheme> for Completion {
    fn from(value: &'static CommandScheme) -> Self {
        fn walk_inner(
            inner: &'static InnerScheme,
            list: &mut Vec<Option<&RecData>>,
            map: &mut HashMap<&str, usize>,
        ) {
            match inner.data {
                Some(RecData {
                    starting_alias,
                    ref recs,
                    kind: RecKind::Argument,
                    ..
                }) => {
                    assert_eq!(starting_alias, inner.inner.as_ref().unwrap().len());
                    for (i, argument) in recs.iter().enumerate().take(starting_alias) {
                        list.push(inner.inner.as_ref().expect("is some")[i].data.as_ref());
                        assert!(match map.insert(argument, list.len() - 1,) {
                            None => true,
                            Some(j) =>
                                list[j] == inner.inner.as_ref().expect("is some")[i].data.as_ref(),
                        });
                        walk_inner(&inner.inner.as_ref().expect("is some")[i], list, map);
                    }
                }
                _ => assert!(inner.inner.is_none()),
            }
        }

        assert_eq!(value.commands.starting_alias, value.inner.len());
        let mut rec_map = HashMap::new();
        let mut rec_list = Vec::new();
        rec_list.push(Some(&value.commands));
        for i in 0..value.commands.starting_alias {
            rec_list.push(value.inner[i].data.as_ref());
            assert!(
                match rec_map.insert(value.commands.recs[i], rec_list.len() - 1) {
                    None => true,
                    Some(j) => rec_list[j] == value.inner[i].data.as_ref(),
                }
            );
            walk_inner(&value.inner[i], &mut rec_list, &mut rec_map);
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
// move error calculation from style.rs into below methods
// store error bool in `LineData`

impl LineReader<'_> {
    pub fn update_completeion(&mut self) {
        let line_trim_start = self.line.input().trim_start();
        if line_trim_start.is_empty() {
            self.reset_completion();
            return;
        }
        if let Some(RecData { end: true, .. }) = self.completion.rec_list[self.completion.curr_val]
        {
            if self.line.input().contains(
                self.completion
                    .get_last_key()
                    .expect("state is managed correctly"),
            ) {
                return;
            }
        }
        self.completion.rec_i = USER_INPUT;
        self.completion.input.curr_command = line_trim_start
            .split_once(' ')
            .map(|split| split.0.to_lowercase());

        let mut tokens_rev = line_trim_start.rsplit(' ');
        self.completion.input.user_input = if let Some(ending_token) = tokens_rev.next() {
            if let Some(ref command) = self.completion.input.curr_command {
                self.completion.input.curr_argument = tokens_rev
                    .next()
                    .and_then(|arg| {
                        arg.starts_with('-').then(|| {
                            let arg = arg.trim_start_matches('-').to_lowercase();
                            (command != &arg).then_some(arg)
                        })
                    })
                    .flatten();
            }
            ending_token
        } else {
            line_trim_start
        }
        .trim_start_matches('-')
        .to_string();

        self.completion.curr_val = {
            let data_i = self
                .completion
                .get_last_key()
                .and_then(|key| self.completion.rec_map.get(key.as_str()));

            if let Some((i, Some(rec_data))) =
                data_i.and_then(|&i| self.completion.rec_list.get(i).map(|data| (i, data)))
            {
                if self.completion.input.user_input.is_empty() {
                    self.completion.curr_val = i;
                    self.completion.recomendations =
                        rec_data.recs[..rec_data.starting_alias].to_vec();
                    return;
                }
                i
            } else {
                COMMANDS
            }
        };

        let input_lower = self.completion.input.user_input.to_lowercase();
        let mut recomendations = self.completion.rec_list[self.completion.curr_val]
            .expect("checked is some above")
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
            match self.completion.rec_list[self.completion.curr_val]
                .expect("update completion filters out None case")
            {
                RecData {
                    kind: RecKind::Argument,
                    ..
                } => {
                    format!(
                        "{}{}{recomendation}",
                        self.line
                            .input()
                            .trim_end_matches(last)
                            .trim_end_matches('-'),
                        if recomendation.is_empty() { "" } else { "--" }
                    )
                }
                RecData {
                    kind: RecKind::Value | RecKind::Command,
                    ..
                } => self.line.input().rsplit_once(' ').map_or_else(
                    || recomendation.to_string(),
                    |split| format!("{} {recomendation}", split.0),
                ),
                RecData {
                    kind: RecKind::UserDefined,
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
        let commands = self.completion.rec_list[COMMANDS].expect("commands is always some");
        self.completion.recomendations = commands.recs[..commands.starting_alias].to_vec();
        self.completion.input.user_input.clear();
        self.completion.rec_i = USER_INPUT;
    }
}
