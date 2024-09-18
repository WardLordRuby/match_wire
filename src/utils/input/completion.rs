use crate::utils::input::line::LineReader;
use std::{
    collections::{HashMap, HashSet},
    io,
    ops::Range,
};

const NO_PARENT: &str = "NULL";
const ROOT: &str = "ROOT";
const UNIVERSAL: &str = "ANY";

const USER_INPUT: i8 = -1;
const COMMANDS: usize = 0;
const INVALID: usize = 1;

macro_rules! any_true {
    ($($x:expr),+ $(,)?) => {
        $($x)||+
    };
}

// MARK: IMPROVE
// we could solve name-space collisions by making the data structure into a prefix-tree

/// The current implementation only works when the name-space of commands and arguments do not overlap, overlaping  
/// names must return the exact same `RecData`, help is special cased to work as both a command and argument  
/// `inner` must ALWAYS contain the same number of elements as `commands.starting_alias`  
pub struct CommandScheme {
    /// command names followed by aliases
    commands: RecData,

    /// static empty node used for invalid inputs
    empty: RecData,

    /// inner data shares indices with `commands.recs`
    inner: Vec<InnerScheme>,
}

/// Notes:  
/// - Recomendations within `data` set as `Kind::Value` will be flattened into a HashSet.  
///   Access to the set is provided through a seprate map `value_sets` where the lookup key  
///   is the index you get back from `rec_map` when hasing the parent node  
/// - `RecKinds` `Value` and `UserInput` must provide a `Range<usize>` of inputs that are expected to follow  
///
/// field `data` must adhere to the following  
///  - kind can not be `Kind::Command` commands are only supported at the top level  
///
/// field `inner` must adhere to the following
///  - if `data.kind` is `Kind::Argument` `inner` must contain the same number of elements as `data.starting_alias`  
///  - for all other kinds `inner` must be `None`
struct InnerScheme {
    /// data that describes recomendations context
    data: RecData,

    /// inner data shares indices with `data.recs`
    inner: Option<Vec<InnerScheme>>,
}

#[derive(PartialEq, Eq, Debug)]
pub struct RecData {
    /// name of the parent entry
    parent: &'static str,
    /// the starting index of aliases
    starting_alias: usize,
    /// recomendations followed by recomendation aliases
    recs: Vec<&'static str>,
    /// kind of data stored
    kind: RecKind,
    /// signals this is a leaf node
    end: bool,
}

impl RecData {
    fn empty() -> Self {
        RecData {
            parent: NO_PARENT,
            starting_alias: 0,
            recs: Vec::new(),
            kind: RecKind::Null,
            end: false,
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
enum RecKind {
    Command,
    Argument,
    Value(Range<usize>),
    UserDefined(Range<usize>),
    Help,
    Null,
}

impl RecKind {
    /// minimum of 1 arg is assumed
    fn user_defined_with_num_args(max: usize) -> Self {
        RecKind::UserDefined(Range {
            start: 1,
            end: max.saturating_add(1),
        })
    }
    /// minimum of 1 arg is assumed
    fn value_with_num_args(max: usize) -> Self {
        RecKind::Value(Range {
            start: 1,
            end: max.saturating_add(1),
        })
    }
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
            data: RecData {
                parent,
                starting_alias: 0,
                recs: Vec::new(),
                kind: RecKind::Null,
                end: true,
            },
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
        empty: RecData::empty(),
        inner: vec![
            // filter
            InnerScheme {
                data: RecData {
                    parent: ROOT,
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
                    InnerScheme::empty_with(
                        "filter",
                        RecKind::user_defined_with_num_args(1),
                        false,
                    ),
                    // player-min
                    InnerScheme::empty_with(
                        "filter",
                        RecKind::user_defined_with_num_args(1),
                        false,
                    ),
                    // team-size-max
                    InnerScheme::empty_with(
                        "filter",
                        RecKind::user_defined_with_num_args(1),
                        false,
                    ),
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
                            kind: RecKind::value_with_num_args(1),
                            end: false,
                        },
                        inner: None,
                    },
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
                    // help
                    InnerScheme::help(),
                ]),
            },
            // reconnect
            InnerScheme {
                data: RecData {
                    parent: ROOT,
                    starting_alias: 3,
                    recs: vec!["history", "connect", "help"],
                    kind: RecKind::Argument,
                    end: false,
                },
                inner: Some(vec![
                    // history
                    InnerScheme::ending_rec("reconnect"),
                    // connect
                    InnerScheme::empty_with(
                        "reconnect",
                        RecKind::user_defined_with_num_args(1),
                        true,
                    ),
                    // help
                    InnerScheme::help(),
                ]),
            },
            // launch
            InnerScheme::ending_rec(ROOT),
            // update-cache
            InnerScheme::ending_rec(ROOT),
            // display-logs
            InnerScheme::ending_rec(ROOT),
            // game-dir
            InnerScheme::ending_rec(ROOT),
            // local-env
            InnerScheme::ending_rec(ROOT),
            // quit
            InnerScheme::ending_rec(ROOT),
            // version
            InnerScheme::ending_rec(ROOT),
            // help
            InnerScheme::help(),
        ],
    }
}

impl From<&'static CommandScheme> for Completion {
    fn from(value: &'static CommandScheme) -> Self {
        fn insert_rec_set(
            map: &mut HashMap<usize, HashSet<&'static str>>,
            recs: &[&'static str],
            at: usize,
        ) {
            assert!(
                match map.insert(at, HashSet::from_iter(recs.iter().copied())) {
                    None => true,
                    Some(set) => set == HashSet::from_iter(recs.iter().copied()),
                }
            )
        }
        fn insert_index(
            map: &mut HashMap<&'static str, usize>,
            key: &'static str,
            data: &'static RecData,
            list: &[&'static RecData],
        ) {
            assert!(match map.insert(key, list.len() - 1) {
                None => true,
                Some(j) => list[j] == data,
            });
        }

        fn walk_inner(
            inner: &'static InnerScheme,
            list: &mut Vec<&'static RecData>,
            map: &mut HashMap<&'static str, usize>,
            value_sets: &mut HashMap<usize, HashSet<&'static str>>,
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
                        insert_index(map, argument, &inner.data, list);
                        if let RecKind::Value(_) = inner.data.kind {
                            insert_rec_set(value_sets, &inner.data.recs, list.len() - 1);
                        }
                        walk_inner(inner, list, map, value_sets);
                    }
                }
                _ => assert!(inner.inner.is_none()),
            }
        }

        assert_eq!(value.commands.starting_alias, value.inner.len());
        let mut rec_map = HashMap::new();
        let mut value_sets = HashMap::new();
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
            insert_index(&mut rec_map, command, &inner.data, &rec_list);
            if let RecKind::Value(_) = inner.data.kind {
                insert_rec_set(&mut value_sets, &inner.data.recs, rec_list.len() - 1);
            }
            walk_inner(inner, &mut rec_list, &mut rec_map, &mut value_sets);
        }
        Completion {
            recomendations: value.commands.recs[..value.commands.starting_alias].to_vec(),
            rec_i: USER_INPUT,
            input: CompletionState::default(),
            rec_map,
            rec_list,
            value_sets,
            curr_i: COMMANDS,
        }
    }
}

pub struct Completion {
    recomendations: Vec<&'static str>,
    rec_i: i8,
    input: CompletionState,
    curr_i: usize,
    rec_map: HashMap<&'static str, usize>,
    value_sets: HashMap<usize, HashSet<&'static str>>,
    rec_list: Vec<&'static RecData>,
}

#[derive(Default, Debug)]
struct CompletionState {
    curr_command: Option<SliceData>,
    curr_argument: Option<SliceData>,
    curr_value: Option<SliceData>,
    ending: LineEnd,
}

#[derive(Default, Debug)]
/// representes a String slice entry into `LineData.line().trim_start()`
struct SliceData {
    byte_start: usize,
    slice_len: usize,
    hash_i: usize,
}

#[derive(Default, Debug)]
struct LineEnd {
    token: String,
    open_quote: Option<(usize, char)>,
}

impl CompletionState {
    fn check_state_unchecked(&mut self, line: &str) {
        if let Some(ref command) = self.curr_command {
            if self.ending.token == command.to_slice_unchecked(line) {
                (self.curr_command, self.curr_argument, self.curr_value) = (None, None, None);
                return;
            }
        }
        if let Some(ref arg) = self.curr_argument {
            if self.ending.token == arg.to_slice_unchecked(line) {
                (self.curr_argument, self.curr_value) = (None, None);
                return;
            }
        }
        if let Some(ref value) = self.curr_value {
            if self.ending.token == value.to_slice_unchecked(line) {
                self.curr_value = None;
            }
        }
    }

    fn update_curr_token(&mut self, line: &str) {
        let curr_token = line
            .rsplit_once(char::is_whitespace)
            .map_or(line, |(_, suf)| suf);
        self.ending.token = if let Some((l_i, quote)) = self.ending.open_quote {
            if let Some(r_i) = line.rfind(quote) {
                if r_i > l_i {
                    self.ending.open_quote = None;
                    &line[l_i..=r_i]
                } else {
                    let str = &line[l_i..];
                    if !str.starts_with(quote) {
                        self.ending.open_quote = None;
                    }
                    str
                }
            } else {
                self.ending.open_quote = None;
                curr_token
            }
        } else {
            let starting_quote = self
                .ending
                .token
                .starts_with(['\'', '\"'])
                .then(|| self.ending.token.chars().next().expect("starts with quote"));

            let r_find_quote = match starting_quote {
                Some(quote) => line.char_indices().rfind(|&(_, c)| c == quote),
                None => line.char_indices().rfind(|&(_, c)| c == '\'' || c == '\"'),
            };

            if let Some((r_i, quote)) = r_find_quote {
                let quote_num = line.chars().filter(|&c| c == quote).count();
                if quote_num & 1 == 0 {
                    if curr_token.ends_with(quote) {
                        let l_i = line[..r_i].rfind(quote).expect("quote num even");
                        &line[l_i..=r_i]
                    } else {
                        curr_token
                    }
                } else {
                    self.ending.open_quote = Some((r_i, quote));
                    &line[r_i..]
                }
            } else {
                curr_token
            }
        }
        .to_string();
    }
    /* -------------------------------- Debug tool --------------------------------------- */
    // fn display(&self, line: &str) -> String {
    //     let inner_fmt = |slice_data: &SliceData| -> String {
    //         format!(
    //             "'{}' hash_i: {}",
    //             slice_data.to_slice_unchecked(line),
    //             slice_data.hash_i
    //         )
    //     };

    //     let mut output = String::new();
    //     output.push_str(&format!(
    //         "\n    curr_command: {:?}\n",
    //         self.curr_command.as_ref().map(inner_fmt)
    //     ));
    //     output.push_str(&format!(
    //         "    curr_arg: {:?}\n",
    //         self.curr_argument.as_ref().map(inner_fmt)
    //     ));
    //     output.push_str(&format!(
    //         "    curr_value: {:?}\n",
    //         self.curr_value.as_ref().map(inner_fmt)
    //     ));
    //     output.push_str(&format!("    user_input: {:?}\n", self.ending));
    //     output
    // }
    /* ----------------------------------------------------------------------------------- */
}

trait Validity {
    /// returns `true` if `Some(hash_i == INVALID)` else `false`
    fn is_some_and_invalid(&self) -> bool;

    /// returns `true` if `Some(hash_i != INVALID)` else `false`
    fn is_some_and_valid(&self) -> bool;
}

impl Validity for Option<&SliceData> {
    fn is_some_and_invalid(&self) -> bool {
        matches!(self, Some(SliceData { hash_i, ..}) if *hash_i == INVALID)
    }
    fn is_some_and_valid(&self) -> bool {
        matches!(self, Some(SliceData { hash_i, ..}) if *hash_i != INVALID)
    }
}

impl SliceData {
    fn from_raw_unchecked(
        byte_start: usize,
        slice_len: usize,
        expected: RecKind,
        line: &str,
        completion: &Completion,
    ) -> Self {
        let mut data = SliceData {
            byte_start,
            slice_len,
            hash_i: INVALID,
        };
        match expected {
            RecKind::Command => completion.hash_command_unchecked(line, &mut data),
            RecKind::Argument => completion.hash_arg_unchecked(line, &mut data),
            _ => (),
        }
        data
    }

    fn to_slice_unchecked<'a>(&self, line: &'a str) -> &'a str {
        &line[self.byte_start..self.byte_start + self.slice_len]
    }
}

impl Completion {
    #[inline]
    fn last_key_unchecked<'a>(&self, line: &'a str) -> Option<&'a str> {
        self.curr_value()
            .or(self.curr_arg())
            .or(self.curr_command())
            .map(|key| key.to_slice_unchecked(line))
    }
    #[inline]
    fn curr_command(&self) -> Option<&SliceData> {
        self.input.curr_command.as_ref()
    }
    #[inline]
    fn curr_arg(&self) -> Option<&SliceData> {
        self.input.curr_argument.as_ref()
    }
    #[inline]
    fn curr_value(&self) -> Option<&SliceData> {
        self.input.curr_value.as_ref()
    }

    /// expects `RecKind::Value`
    fn value_valid(&self, value: &str, i: usize) -> bool {
        self.value_sets.get(&i).expect("kind value").contains(value)
    }

    fn try_parse_token_from_end(&self, line: &str, expected: RecKind) -> Option<SliceData> {
        self.input
            .ending
            .open_quote
            .is_none()
            .then(|| {
                let line_trim_end = line.trim_end();
                if line_trim_end.ends_with(['\'', '\"']) {
                    let quote = line_trim_end.chars().next_back().expect("outer if");
                    let l_i = line_trim_end[..line_trim_end.len() - quote.len_utf8()].rfind(quote);
                    debug_assert!(l_i.is_some(), "open quote is none yet still ends in quote");
                    l_i.map(|l_i| {
                        SliceData::from_raw_unchecked(
                            l_i,
                            line_trim_end.len() - l_i,
                            expected,
                            line,
                            self,
                        )
                    })
                } else {
                    line.rfind(|c: char| !c.is_whitespace())
                        .and_then(|token_end| {
                            let end = line.len() - (line.len() - token_end);
                            line[..=end].rfind(char::is_whitespace).map(|start| {
                                SliceData::from_raw_unchecked(
                                    start + ' '.len_utf8(),
                                    end - start,
                                    expected,
                                    line,
                                    self,
                                )
                            })
                        })
                }
            })
            .flatten()
    }

    fn hash_command_unchecked(&self, line: &str, command: &mut SliceData) {
        let command_str = command.to_slice_unchecked(line);
        if command_str.starts_with('-') {
            command.hash_i = INVALID;
            return;
        }
        if let Some(&i) = self.rec_map.get(command_str) {
            if self.rec_list[i].parent == ROOT || self.rec_list[i].parent == UNIVERSAL {
                command.hash_i = i;
                return;
            }
        }
        command.hash_i = INVALID
    }

    fn hash_arg_unchecked(&self, line: &str, arg: &mut SliceData) {
        let arg_str = arg.to_slice_unchecked(line);
        if !arg_str.starts_with('-') {
            arg.hash_i = INVALID;
            return;
        }
        let command = self
            .curr_command()
            .expect("can only set arg if command is valid")
            .to_slice_unchecked(line);
        let arg_str = arg_str.trim_start_matches('-');
        if let Some(&i) = self.rec_map.get(arg_str) {
            if self.rec_list[i].parent == command || self.rec_list[i].parent == UNIVERSAL {
                arg.hash_i = i;
                return;
            }
        }
        arg.hash_i = INVALID
    }
}

impl LineReader<'_> {
    #[inline]
    fn curr_token(&self) -> &str {
        &self.completion.input.ending.token
    }
    #[inline]
    fn open_quote(&self) -> Option<&(usize, char)> {
        self.completion.input.ending.open_quote.as_ref()
    }

    fn check_value_err(&self, user_input: &str) -> bool {
        match self.completion.rec_list[self.completion.curr_i].kind {
            RecKind::Value(_)
                if !self
                    .completion
                    .value_valid(user_input, self.completion.curr_i) =>
            {
                true
            }
            RecKind::UserDefined(_) if self.curr_token().is_empty() => true,
            _ => false,
        }
    }

    pub fn update_completeion(&mut self) {
        let line_trim_start = self.line.input().trim_start();
        if line_trim_start.is_empty() {
            self.reset_completion();
            return;
        }

        self.completion.input.update_curr_token(line_trim_start);
        self.completion.input.check_state_unchecked(line_trim_start);

        if self.completion.rec_list[self.completion.curr_i].end
            && self
                .completion
                .last_key_unchecked(line_trim_start)
                .is_some_and(|last| line_trim_start.contains(last))
        {
            return;
        }

        self.completion.rec_i = USER_INPUT;

        if self.completion.curr_command().is_none()
            && line_trim_start.ends_with(char::is_whitespace)
            && self.open_quote().is_none()
        {
            self.completion.input.curr_command = line_trim_start
                .split_once(char::is_whitespace)
                .map(|(pre, _)| {
                    SliceData::from_raw_unchecked(
                        0,
                        pre.len(),
                        RecKind::Command,
                        line_trim_start,
                        &self.completion,
                    )
                });
        }

        if self.completion.curr_command().is_some_and_valid()
            && self.completion.curr_arg().is_none()
            && self.open_quote().is_none()
        {
            let command = self.completion.curr_command().expect("outer if");
            self.completion.input.curr_argument = if line_trim_start[command.slice_len..]
                .trim_start()
                .ends_with(char::is_whitespace)
            {
                let mut prev_num_vals = 0;
                let mut prev_arg = None;

                // MARK: IMPROVE
                // quoted strings should be treated as one value
                // in our case it doesn't affect our desired behavior since the only arguments
                // that take in strings have no upper limit on how many values can be entered
                for token in line_trim_start
                    [command.slice_len..line_trim_start.len() - self.curr_token().len()]
                    .split_whitespace()
                    .rev()
                {
                    if !token.starts_with('-') {
                        prev_num_vals += 1
                    } else {
                        prev_arg = Some(token);
                        break;
                    }
                }

                let mut take_end = true;

                if let (Some(arg), true) = (prev_arg, prev_num_vals > 0) {
                    let hash = self
                        .completion
                        .rec_map
                        .get(arg.trim_start_matches('-'))
                        .copied()
                        .unwrap_or(INVALID);

                    match self.completion.rec_list[hash].kind {
                        RecKind::Value(ref r) | RecKind::UserDefined(ref r) => {
                            if r.contains(&prev_num_vals) {
                                take_end = false;
                            }
                        }
                        _ => (),
                    }
                }

                if take_end {
                    self.completion
                        .try_parse_token_from_end(line_trim_start, RecKind::Argument)
                } else {
                    None
                }
            } else {
                self.completion
                    .try_parse_token_from_end(
                        &line_trim_start[..line_trim_start.len() - self.curr_token().len()],
                        RecKind::Argument,
                    )
                    .filter(|arg| {
                        arg.hash_i != INVALID
                            && arg.to_slice_unchecked(line_trim_start).starts_with('-')
                    })
            };
        }

        if self.completion.curr_arg().is_some_and_valid()
            && self.completion.curr_value().is_none()
            && self.open_quote().is_none()
        {
            let arg = self.completion.curr_arg().expect("outer if");
            if line_trim_start[arg.byte_start + arg.slice_len..]
                .trim_start()
                .ends_with(char::is_whitespace)
            {
                match self.completion.rec_list[arg.hash_i].kind {
                    RecKind::Value(_) => {
                        if let Some(value) = self.completion.try_parse_token_from_end(
                            line_trim_start,
                            // we expect Value but passing in value does nothing in try_parse
                            RecKind::Null,
                        ) {
                            if self
                                .completion
                                .value_valid(value.to_slice_unchecked(line_trim_start), arg.hash_i)
                            {
                                self.completion.input.curr_argument = None;
                            } else {
                                self.completion.input.curr_value = Some(value);
                            }
                        };
                    }
                    RecKind::UserDefined(_) => {
                        if line_trim_start[arg.byte_start + arg.slice_len..]
                            .trim_start()
                            .ends_with(char::is_whitespace)
                        {
                            self.completion.input.curr_argument = None;
                        }
                    }
                    _ => (),
                }
            }
        }

        // eprintln!("{}", self.completion.input.display(line_trim_start));

        self.completion.curr_i = match (
            self.completion.curr_command(),
            self.completion.curr_arg(),
            self.completion.curr_value(),
        ) {
            (Some(&SliceData { hash_i: i, .. }), Some(&SliceData { hash_i: j, .. }), Some(_))
                if i != INVALID && j != INVALID =>
            {
                INVALID
            }
            (Some(&SliceData { hash_i: i, .. }), Some(&SliceData { hash_i: j, .. }), None)
                if i != INVALID && j != INVALID =>
            {
                j
            }
            (Some(&SliceData { hash_i: i, .. }), None, None) if i != INVALID => i,
            (None, None, None) if line_trim_start.split_whitespace().count() <= 1 => COMMANDS,
            _ => INVALID,
        };

        let rec_data = self.completion.rec_list[self.completion.curr_i];

        self.line.found_err(any_true!(
            self.completion.curr_command().is_some_and_invalid(),
            self.completion.curr_arg().is_some_and_invalid(),
            self.completion.curr_value().is_some(),
            self.open_quote().is_some(),
            self.check_value_err(self.curr_token())
        ));

        if rec_data.recs.is_empty() {
            self.completion.recomendations = Vec::new();
            return;
        }

        if self.curr_token().is_empty() {
            self.completion.recomendations = rec_data.recs[..rec_data.starting_alias].to_vec();
            return;
        }

        let input_lower = self.curr_token().trim_start_matches('-').to_lowercase();

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

        self.completion.rec_i += by;
        match self.completion.rec_i {
            i if i >= USER_INPUT && i < self.completion.recomendations.len() as i8 => (),
            ..USER_INPUT => self.completion.rec_i = self.completion.recomendations.len() as i8 - 1,
            _ => self.completion.rec_i = USER_INPUT,
        }

        let new_line = {
            let recomendation = if self.completion.rec_i == USER_INPUT {
                self.curr_token()
            } else {
                self.completion.recomendations[self.completion.rec_i as usize]
            };

            let format_line = |rec_is_arg: bool| -> String {
                self.line
                    .input()
                    .rsplit_once(char::is_whitespace)
                    .map_or_else(
                        || recomendation.to_string(),
                        |(pre, _)| {
                            format!(
                                "{pre} {}{recomendation}",
                                if rec_is_arg
                                    && !recomendation.is_empty()
                                    && self.completion.rec_i != USER_INPUT
                                {
                                    "--"
                                } else {
                                    ""
                                }
                            )
                        },
                    )
            };

            match self.completion.rec_list[self.completion.curr_i].kind {
                RecKind::Argument => format_line(true),
                RecKind::Value(_) | RecKind::Command => format_line(false),
                RecKind::Help => format_line(self.completion.curr_command().is_some()),
                RecKind::UserDefined(_) | RecKind::Null => unreachable!("by guard clause"),
            }
        };

        self.line.found_err(
            self.check_value_err(
                new_line
                    .rsplit_once(char::is_whitespace)
                    .map_or(&new_line, |(_, suf)| suf),
            ),
        );

        self.change_line(new_line)
    }

    pub fn reset_completion(&mut self) {
        self.completion.input = CompletionState::default();
        self.completion.curr_i = COMMANDS;
        let commands = self.completion.rec_list[COMMANDS];
        self.completion.recomendations = commands.recs[..commands.starting_alias].to_vec();
        self.completion.rec_i = USER_INPUT;
    }
}
