use crate::utils::input::line::LineReader;
use std::{
    collections::{HashMap, HashSet},
    io,
    ops::Range,
};

pub const ROOT: &str = "ROOT";
const UNIVERSAL: &str = "ANY";

const USER_INPUT: i8 = -1;
const COMMANDS: usize = 0;
const INVALID: usize = 1;
const VALID: usize = 2;

macro_rules! any_true {
    ($($x:expr),+ $(,)?) => {
        $($x)||+
    };
}

// MARK: IMPROVE
// we could solve name-space collisions by making the data structure into a prefix-tree
// this gets increasingly problematic to continue to support short arg syntax

/// The current implementation only works when the name-space of commands, arguments, and their aliases/shorts do not overlap,  
/// overlaping names must return the exact same `RecData`, help is special cased to work as both a command and argument  
/// `inner` must ALWAYS contain the same number of elements as `commands.starting_alias`  
pub struct CommandScheme {
    /// command names followed by aliases
    commands: RecData,

    /// static empty node used for invalid inputs
    invalid: RecData,

    /// static empty node used for valid inputs of `RecKind::Value`
    valid: RecData,

    /// inner data shares indices with `commands.recs`
    inner: &'static [InnerScheme],
}

/// Notes:  
/// - Recomendations within `data` set as `Kind::Value` will be flattened into a HashSet.  
///   Access to the set is provided through a seprate map `value_sets` where the lookup key  
///   is the index you get back from `rec_map` when hasing the parent node  
/// - `RecKinds`: `Value` and `UserInput` must provide a `Range<usize>` of inputs that are expected to follow  
///
/// field `data` must adhere to the following  
///  - kind can not be `Kind::Command` commands are only supported at the top level  
///
/// field `inner` must adhere to the following
///  - if `data.kind` is `Kind::Argument` `inner` must contain the same number of elements as `data.starting_alias`  
///  - for all other kinds `inner` must be `None`
pub struct InnerScheme {
    /// data that describes recomendations context
    data: RecData,

    /// inner data shares indices with `data.recs`
    inner: Option<&'static [Self]>,
}

#[derive(PartialEq, Eq, Debug)]
pub struct RecData {
    /// name of the parent entry
    parent: Option<&'static str>,
    /// required data if this node contains any aliases
    alias: Option<AliasData>,
    /// required data if containing recs support a short arg syntax
    short: Option<ShortData>,
    /// recomendations followed by recomendation aliases
    recs: Option<&'static [&'static str]>,
    /// kind of data stored
    kind: RecKind,
    /// signals this is a leaf node
    end: bool,
}

#[derive(PartialEq, Eq, Debug)]
struct AliasData {
    /// index of rec in `recs` -> index of alias in `recs`
    rec_mapping: &'static [(usize, usize)],
}

#[derive(PartialEq, Eq, Debug)]
struct ShortData {
    /// index of rec in `recs` -> short char
    short_mapping: &'static [(usize, &'static str)],
}

impl CommandScheme {
    pub const fn new(commands: RecData, inner: &'static [InnerScheme]) -> Self {
        CommandScheme {
            commands,
            invalid: RecData::empty(),
            valid: RecData::empty(),
            inner,
        }
    }
}

impl InnerScheme {
    pub const fn new(data: RecData, inner: Option<&'static [Self]>) -> Self {
        InnerScheme { data, inner }
    }

    pub const fn flag(parent: &'static str, end: bool) -> Self {
        InnerScheme {
            data: RecData {
                parent: Some(parent),
                alias: None,
                short: None,
                recs: None,
                kind: RecKind::Null,
                end,
            },
            inner: None,
        }
    }

    pub const fn empty_with(parent: &'static str, kind: RecKind, end: bool) -> Self {
        InnerScheme {
            data: RecData {
                parent: Some(parent),
                alias: None,
                short: None,
                recs: None,
                kind,
                end,
            },
            inner: None,
        }
    }

    pub const fn end(parent: &'static str) -> Self {
        InnerScheme {
            data: RecData {
                parent: Some(parent),
                alias: None,
                short: None,
                recs: None,
                kind: RecKind::Null,
                end: true,
            },
            inner: None,
        }
    }

    pub const fn help() -> Self {
        InnerScheme {
            data: RecData {
                parent: Some(UNIVERSAL),
                alias: None,
                short: None,
                recs: None,
                kind: RecKind::Help,
                end: true,
            },
            inner: None,
        }
    }
}

impl RecData {
    pub const fn new(
        parent: Option<&'static str>,
        alias: Option<&'static [(usize, usize)]>,
        short: Option<&'static [(usize, &'static str)]>,
        recs: Option<&'static [&'static str]>,
        kind: RecKind,
        end: bool,
    ) -> Self {
        RecData {
            parent,
            alias: if let Some(mapping) = alias {
                Some(AliasData {
                    rec_mapping: mapping,
                })
            } else {
                None
            },
            short: if let Some(mapping) = short {
                Some(ShortData {
                    short_mapping: mapping,
                })
            } else {
                None
            },
            recs,
            kind,
            end,
        }
    }

    const fn empty() -> Self {
        RecData {
            parent: None,
            alias: None,
            short: None,
            recs: None,
            kind: RecKind::Null,
            end: true,
        }
    }

    #[inline]
    fn rec_len(&self) -> usize {
        self.recs.map_or(0, |recs| recs.len())
    }

    fn unique_rec_end(&self) -> usize {
        self.alias.as_ref().map_or(self.rec_len(), |data| {
            self.rec_len() - data.rec_mapping.len()
        })
    }
}

#[derive(PartialEq, Eq, Debug)]
pub enum RecKind {
    Command,
    Argument,
    Value(Range<usize>),
    UserDefined(Range<usize>),
    Help,
    Null,
}

impl RecKind {
    /// minimum of 1 arg is assumed
    pub const fn user_defined_with_num_args(max: usize) -> Self {
        RecKind::UserDefined(Range {
            start: 1,
            end: max.saturating_add(1),
        })
    }
    /// minimum of 1 arg is assumed
    pub const fn value_with_num_args(max: usize) -> Self {
        RecKind::Value(Range {
            start: 1,
            end: max.saturating_add(1),
        })
    }
}

pub enum Direction {
    Next,
    Previous,
}

impl From<&'static CommandScheme> for Completion {
    fn from(value: &'static CommandScheme) -> Self {
        fn insert_rec_set(
            map: &mut HashMap<usize, HashSet<&'static str>>,
            recs: &[&'static str],
            at: usize,
        ) {
            assert!(map
                .insert(at, HashSet::from_iter(recs.iter().copied()))
                .is_none())
        }
        fn insert_index(
            map: &mut HashMap<&'static str, usize>,
            key: &'static str,
            val: usize,
            data: &'static RecData,
            list: &[&'static RecData],
        ) {
            assert!(
                match map.insert(key, val) {
                    None => true,
                    Some(j) => list[j] == data,
                },
                "duplicate recomendation entries _must_ have identical nodes"
            );
        }

        fn walk_inner(
            inner: &'static InnerScheme,
            list: &mut Vec<&'static RecData>,
            map: &mut HashMap<&'static str, usize>,
            value_sets: &mut HashMap<usize, HashSet<&'static str>>,
        ) {
            match inner.data {
                RecData {
                    ref alias,
                    ref recs,
                    ref short,
                    kind: RecKind::Argument,
                    ..
                } => {
                    let expected_len = inner.data.unique_rec_end();
                    assert_eq!(expected_len, inner.inner.unwrap().len());
                    for (i, (&argument, inner)) in recs
                        .expect("is some")
                        .iter()
                        .zip(inner.inner.expect("is some"))
                        .enumerate()
                        .take(expected_len)
                    {
                        list.push(&inner.data);
                        let l_i = list.len() - 1;
                        insert_index(map, argument, l_i, &inner.data, list);
                        if let Some(data) = short {
                            if let Some(&(_, short)) =
                                data.short_mapping.iter().find(|(map_i, _)| *map_i == i)
                            {
                                insert_index(map, short, l_i, &inner.data, list);
                            }
                        }
                        if let Some(data) = alias {
                            data.rec_mapping
                                .iter()
                                .filter(|(rec_i, _)| *rec_i == i)
                                .map(|&(_, alias_i)| recs.expect("is some")[alias_i])
                                .for_each(|alias| {
                                    insert_index(map, alias, l_i, &inner.data, list);
                                });
                        }
                        if let RecKind::Value(_) = inner.data.kind {
                            insert_rec_set(value_sets, inner.data.recs.expect("is some"), l_i);
                        }
                        walk_inner(inner, list, map, value_sets);
                    }
                }
                _ => {
                    assert!(inner.inner.is_none());
                    assert!(inner.data.short.is_none());
                }
            }
        }

        let expected_len = value.commands.unique_rec_end();
        assert_eq!(expected_len, value.inner.len());
        assert!(value.commands.short.is_none());
        let mut rec_map = HashMap::new();
        let mut value_sets = HashMap::new();
        let mut rec_list = Vec::new();
        rec_list.push(&value.commands);
        rec_list.push(&value.invalid);
        rec_list.push(&value.valid);
        for (i, (&command, inner)) in value
            .commands
            .recs
            .expect("is some")
            .iter()
            .zip(value.inner.iter())
            .enumerate()
            .take(expected_len)
        {
            rec_list.push(&inner.data);
            let l_i = rec_list.len() - 1;
            insert_index(&mut rec_map, command, l_i, &inner.data, &rec_list);
            if let Some(ref data) = value.commands.alias {
                data.rec_mapping
                    .iter()
                    .filter(|(rec_i, _)| *rec_i == i)
                    .map(|&(_, alias_i)| value.commands.recs.expect("is some")[alias_i])
                    .for_each(|alias| {
                        insert_index(&mut rec_map, alias, l_i, &inner.data, &rec_list);
                    });
            }
            if let RecKind::Value(_) = inner.data.kind {
                insert_rec_set(&mut value_sets, inner.data.recs.expect("is some"), l_i);
            }
            walk_inner(inner, &mut rec_list, &mut rec_map, &mut value_sets);
        }
        Completion {
            recomendations: value.commands.recs.expect("is some")[..expected_len].to_vec(),
            input: CompletionState::default(),
            rec_map,
            rec_list,
            value_sets,
            indexer: Indexer::default(),
        }
    }
}

/// on startup the `CommandScheme` tree structure gets flattended into this struct to provide  
/// efficent lookups to the correct data that should be used to compute the best recomendations  
/// for the user with any given input. `Completion` also holds the current line state in field `input`  
/// `CompletionState` aims to provide accurate slices into the string `LineReader.line.input`  
/// since this struct is nested within `LineReader` we manage str slicing by indexes and lens  
pub struct Completion {
    recomendations: Vec<&'static str>,
    input: CompletionState,
    indexer: Indexer,
    rec_list: Vec<&'static RecData>,
    rec_map: HashMap<&'static str, usize>,
    value_sets: HashMap<usize, HashSet<&'static str>>,
}

struct Indexer {
    list: (usize, usize),
    multiple: bool,
    in_list_2: Vec<i8>,
    recs: i8,
}

impl Default for Indexer {
    fn default() -> Self {
        Indexer {
            list: (COMMANDS, INVALID),
            multiple: false,
            in_list_2: Vec::new(),
            recs: USER_INPUT,
        }
    }
}

// impl std::fmt::Debug for Completion {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         writeln!(
//             f,
//             "\nrecomendations: {:?}\nrec index: {:?}\nlist index: {:?}",
//             self.recomendations, self.rec_i, self.list_i
//         )
//     }
// }

#[derive(Default)]
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

impl PartialEq for SliceData {
    fn eq(&self, other: &Self) -> bool {
        self.byte_start == other.byte_start && self.slice_len == other.slice_len
    }
}

impl Eq for SliceData {}

impl SliceData {
    fn exact_eq(&self, other: &Self) -> bool {
        self.byte_start == other.byte_start
            && self.slice_len == other.slice_len
            && self.hash_i == other.hash_i
    }
}

#[derive(Default, Debug)]
struct LineEnd {
    token: String,
    open_quote: Option<(usize, char)>,
}

impl CompletionState {
    /// returns `true` if method modifies `self`
    fn check_state(&mut self, line: &str) -> bool {
        if let Some(ref command) = self.curr_command {
            if line.len() == command.byte_start + command.slice_len {
                (self.curr_command, self.curr_argument, self.curr_value) = (None, None, None);
                return true;
            }
        }
        if let Some(ref arg) = self.curr_argument {
            if line.len() == arg.byte_start + arg.slice_len {
                (self.curr_argument, self.curr_value) = (None, None);
                return true;
            }
        }
        if let Some(ref value) = self.curr_value {
            if line.len() == value.byte_start + value.slice_len {
                self.curr_value = None;
                return true;
            }
        }
        false
    }

    fn update_curr_token(&mut self, line: &str) {
        let curr_token = line
            .rsplit_once(char::is_whitespace)
            .map_or(line, |(_, suf)| suf);
        self.ending.token = if let Some((l_i, quote)) = self.ending.open_quote {
            if let Some(r_i) = line.rfind(quote) {
                if l_i < r_i {
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
    // fn debug(&self, line: &str) -> String {
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
        expected: &RecKind,
        line: &str,
        completion: &Completion,
        arg_count: Option<usize>,
    ) -> Self {
        let mut data = SliceData {
            byte_start,
            slice_len,
            hash_i: INVALID,
        };
        match expected {
            RecKind::Command => completion.hash_command_unchecked(line, &mut data),
            RecKind::Argument => completion.hash_arg_unchecked(line, &mut data),
            RecKind::Value(ref r) => {
                completion.hash_value_unchecked(line, &mut data, r, arg_count.unwrap_or(1))
            }
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
    fn last_key(&self) -> Option<&SliceData> {
        self.curr_value()
            .or(self.curr_arg())
            .or(self.curr_command())
    }
    #[inline]
    fn arg_or_cmd(&self) -> Option<&SliceData> {
        self.curr_arg().or(self.curr_command())
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

    fn try_parse_token_from_end(
        &self,
        line: &str,
        expected: &RecKind,
        arg_count: Option<usize>,
    ) -> Option<SliceData> {
        if self.input.ending.open_quote.is_none() {
            let line_trim_end = line.trim_end();
            let start = if line_trim_end.ends_with(['\'', '\"']) {
                let quote = line_trim_end.chars().next_back().expect("outer if");
                line_trim_end[..line_trim_end.len() - quote.len_utf8()].rfind(quote)
            } else {
                let mut last = 0;
                let mut chars = line_trim_end.char_indices();
                while let Some((i, ch)) = chars.next_back() {
                    if ch.is_whitespace() {
                        break;
                    }
                    last = i
                }
                Some(last)
            };
            if let Some(byte_start) = start {
                let len = line_trim_end.len() - byte_start;
                return (len > 0).then(|| {
                    SliceData::from_raw_unchecked(byte_start, len, expected, line, self, arg_count)
                });
            }
        }
        None
    }

    /// only counts values until `count_till` is found, if `count_till` is not found it will return the last token in the input `slice`  
    /// `SliceData` is _only_ ever `None` if their are 0 tokens in the input `slice`, `Some(SliceData)` does not gaurentee  
    /// the containing `SliceData` is of `RecKind`: `count_till` or has a valid `hash_i` - in the case that the first token is returned  
    ///
    /// NOTES:  
    ///  - unexpected behavior is _gaurenteed_ for returned `SliceData` if the input `slice` has been sliced from the beginning,  
    ///    the start of `slice`, must align with the start of `line_trim_start`  
    ///  - if you only desire the count of vals, trim input `slice` to include slice of all vals to be counted plus the begining 'root' token  
    ///    then use `RecKind::nil` to avoid hashing counted tokens  
    fn count_vals_in_slice(&self, slice: &str, count_till: &RecKind) -> (Option<SliceData>, usize) {
        let mut nvals = 0;
        let mut prev_token = None;
        let mut end_i = slice.len();
        let beginning_token = self.last_key();

        while let Some(token) = self.try_parse_token_from_end(&slice[..end_i], count_till, None) {
            if token.hash_i != INVALID {
                return (Some(token), nvals);
            } else if beginning_token.map_or(false, |starting_token| token == *starting_token) {
                break;
            } else {
                nvals += 1;
                end_i = token.byte_start;
            }
            prev_token = Some(token)
        }
        (prev_token, nvals.saturating_sub(1))
    }

    fn hash_command_unchecked(&self, line: &str, command: &mut SliceData) {
        let command_str = command.to_slice_unchecked(line);
        if command_str.starts_with('-') {
            return;
        }
        let mut option = None;
        let hash_str = command_str.chars().next().map_or(command_str, |c| {
            if c.is_uppercase() {
                option = Some(format!(
                    "{}{}",
                    c.to_lowercase(),
                    &command_str[c.len_utf8()..]
                ));
                option.as_ref().unwrap()
            } else {
                command_str
            }
        });
        if let Some(&i) = self.rec_map.get(hash_str) {
            if let Some(parent) = self.rec_list[i].parent {
                if parent == ROOT || parent == UNIVERSAL {
                    command.hash_i = i;
                }
            }
        }
    }

    fn hash_arg_unchecked(&self, line: &str, arg: &mut SliceData) {
        let arg_str = arg.to_slice_unchecked(line);
        if !arg_str.starts_with('-') {
            return;
        }
        let command = self
            .curr_command()
            .expect("can only set arg if command is valid")
            .to_slice_unchecked(line);
        let arg_str = arg_str.trim_start_matches('-');
        if let Some(&i) = self.rec_map.get(arg_str) {
            if let Some(parent) = self.rec_list[i].parent {
                if parent == command || parent == UNIVERSAL {
                    arg.hash_i = i;
                }
            }
        }
    }

    fn hash_value_unchecked(
        &self,
        line: &str,
        arg: &mut SliceData,
        range: &Range<usize>,
        count: usize,
    ) {
        if !range.contains(&count) {
            return;
        }

        let val_str = arg.to_slice_unchecked(line);
        if val_str.starts_with('-') {
            return;
        }

        let parent = self
            .arg_or_cmd()
            .expect("can only set value if cmd or arg is some");

        if self.value_valid(val_str, parent.hash_i) {
            arg.hash_i = VALID;
        }
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
        let list_i = [
            self.completion.indexer.list.0,
            self.completion.indexer.list.1,
        ];
        let recs = [
            self.completion.rec_list[list_i[0]],
            self.completion.rec_list[list_i[1]],
        ];
        let input = self.line.input().trim_start();
        let (trailing, trailing_w_last) =
            self.completion.last_key().map_or((input, input), |key| {
                (
                    input[key.byte_start + key.slice_len..].trim(),
                    &input[key.byte_start..],
                )
            });
        let mut errs = [false, false];
        for (i, err) in errs.iter_mut().enumerate() {
            *err = match recs[i].kind {
                RecKind::Value(_) if !self.completion.value_valid(user_input, list_i[i]) => true,
                RecKind::UserDefined(_) if trailing.is_empty() => true,
                RecKind::UserDefined(ref r)
                    if !r.contains(
                        &self
                            .completion
                            .count_vals_in_slice(trailing_w_last, &RecKind::Null)
                            .1,
                    ) =>
                {
                    true
                }
                RecKind::Help | RecKind::Null if !trailing.is_empty() => true,
                _ => false,
            };
        }
        if self.completion.indexer.multiple {
            return errs[0] && errs[1];
        }
        errs[0]
    }

    fn check_for_errors(&mut self) {
        self.line.found_err(any_true!(
            self.completion.curr_command().is_some_and_invalid(),
            self.completion.curr_arg().is_some_and_invalid(),
            self.completion.curr_value().is_some_and_invalid(),
            self.open_quote().is_some(),
            self.check_value_err(self.curr_token())
        ));
    }

    pub fn update_completeion(&mut self) {
        let line_trim_start = self.line.input().trim_start();
        if line_trim_start.is_empty() {
            self.default_recomendations();
            return;
        }

        let multiple_switch_kind = self.completion.indexer.multiple
            && line_trim_start.ends_with(char::is_whitespace)
            && line_trim_start
                .split_whitespace()
                .next_back()
                .map_or(false, |end_token| end_token.starts_with('-'));

        if multiple_switch_kind {
            self.completion.indexer.multiple = false;
        }

        self.completion.input.update_curr_token(line_trim_start);
        let state_changed = self.completion.input.check_state(line_trim_start);

        if let RecData {
            recs: None,
            end: true,
            ..
        } = self.completion.rec_list[self.completion.indexer.list.0]
        {
            if !state_changed {
                self.check_for_errors();
                return;
            }
        }

        self.completion.indexer.recs = USER_INPUT;

        if self.completion.curr_command().is_none() && self.open_quote().is_none() {
            self.completion.input.curr_command = line_trim_start
                .split_once(char::is_whitespace)
                .map(|(pre, _)| {
                    SliceData::from_raw_unchecked(
                        0,
                        pre.len(),
                        &RecKind::Command,
                        line_trim_start,
                        &self.completion,
                        None,
                    )
                });
        }

        if self.open_quote().is_none()
            && self.completion.curr_value().is_none()
            && (self.completion.curr_arg().is_none() || multiple_switch_kind)
            && self.completion.curr_command().is_some_and(|cmd| {
                matches!(
                    self.completion.rec_list[cmd.hash_i].kind,
                    RecKind::Argument | RecKind::Value(_)
                )
            })
        {
            let command = self.completion.curr_command().expect("outer if");
            let command_recs = self.completion.rec_list[command.hash_i];

            let new = if line_trim_start[command.slice_len..]
                .trim_start()
                .ends_with(char::is_whitespace)
            {
                let mut take_end = true;
                let mut new = None;

                let (kind_match, nvals) = self
                    .completion
                    .count_vals_in_slice(line_trim_start, &command_recs.kind);

                if let Some(ref starting_token) = kind_match {
                    let start_token_meta = self.completion.rec_list[starting_token.hash_i];
                    if starting_token.hash_i == INVALID || nvals == 0 {
                        new = kind_match;
                        take_end = false;
                    } else if let RecKind::Value(ref r) | RecKind::UserDefined(ref r) =
                        start_token_meta.kind
                    {
                        if r.contains(&nvals) {
                            take_end = false;
                        }
                    }
                }

                if take_end {
                    new = self.completion.try_parse_token_from_end(
                        line_trim_start,
                        &command_recs.kind,
                        Some(nvals),
                    );
                }
                new
            } else {
                // make sure we set prev arg when backspacing
                let (kind_match, nvals) = self.completion.count_vals_in_slice(
                    &line_trim_start[..line_trim_start.len() - self.curr_token().len()],
                    &command_recs.kind,
                );
                kind_match.filter(|starting_token| {
                    match self.completion.rec_list[starting_token.hash_i].kind {
                        RecKind::UserDefined(_) if nvals == 0 => true,
                        RecKind::Value(ref c) if c.contains(&nvals) => {
                            self.completion.indexer.multiple = true;
                            true
                        }
                        _ if self.completion.rec_list[starting_token.hash_i].end => true,
                        _ => false,
                    }
                })
            };
            match command_recs.kind {
                RecKind::Argument => self.completion.input.curr_argument = new,
                RecKind::Value(_) => self.completion.input.curr_value = new,
                _ => unreachable!("by outer if"),
            }
        }

        if let Some(arg) = self.completion.curr_arg() {
            if let RecData {
                recs: None,
                kind: RecKind::Null,
                end: false,
                ..
            } = self.completion.rec_list[arg.hash_i]
            {
                // boolean flag found
                self.completion.input.curr_argument = None;
            }
        }

        if self.completion.curr_value().is_none()
            && self.open_quote().is_none()
            && self.completion.curr_command().is_some_and_valid()
            && self.completion.curr_arg().is_some_and(|arg| {
                arg.hash_i != INVALID
                    && line_trim_start[arg.byte_start + arg.slice_len..]
                        .trim_start()
                        .ends_with(char::is_whitespace)
            })
        {
            let command = self.completion.curr_command().expect("outer if");
            let command_recs = self.completion.rec_list[command.hash_i];

            let arg = self.completion.curr_arg().expect("outer if");
            let arg_recs = self.completion.rec_list[arg.hash_i];

            match arg_recs.kind {
                RecKind::Value(ref c) => {
                    if let Some(token) = self.completion.try_parse_token_from_end(
                        line_trim_start,
                        &arg_recs.kind,
                        None,
                    ) {
                        if token.hash_i != INVALID {
                            let (kind_match, nvals) = self
                                .completion
                                .count_vals_in_slice(line_trim_start, &command_recs.kind);
                            debug_assert!(kind_match.unwrap().exact_eq(arg));
                            if c.contains(&(nvals + 1)) {
                                self.completion.indexer.multiple = true;
                            } else {
                                self.completion.indexer.multiple = false;
                                self.completion.input.curr_argument = None;
                            }
                        } else {
                            self.completion.input.curr_value = Some(token);
                        }
                    };
                }
                RecKind::UserDefined(_) => self.completion.input.curr_argument = None,
                _ => (),
            }
        }

        // eprintln!("{}", self.completion.input.debug(line_trim_start));

        self.completion.indexer.list = match (
            self.completion.curr_command(),
            self.completion.curr_arg(),
            self.completion.curr_value(),
        ) {
            (Some(_), Some(&SliceData { hash_i: j, .. }), Some(&SliceData { hash_i: k, .. })) => {
                (k, j)
            }
            (Some(&SliceData { hash_i: i, .. }), None, Some(&SliceData { hash_i: k, .. })) => {
                (k, i)
            }
            (Some(&SliceData { hash_i: i, .. }), Some(&SliceData { hash_i: j, .. }), None) => {
                (j, i)
            }
            (Some(&SliceData { hash_i: i, .. }), None, None) => (i, INVALID),
            (None, None, None) if line_trim_start.split_whitespace().count() <= 1 => {
                (COMMANDS, INVALID)
            }
            _ => (INVALID, INVALID),
        };

        if self.completion.indexer.list.1 == INVALID {
            self.completion.indexer.multiple = false;
        }

        let rec_data_1 = self.completion.rec_list[self.completion.indexer.list.0];
        let rec_data_2 = self.completion.rec_list[self.completion.indexer.list.1];

        self.check_for_errors();

        let Some(recs) = rec_data_1.recs else {
            self.completion.recomendations = Vec::new();
            return;
        };

        if self.curr_token().is_empty() {
            let recs = &recs[..rec_data_1.unique_rec_end()];
            self.completion.recomendations = recs.to_vec();
            if self.completion.indexer.multiple {
                if let Some(recs2) = rec_data_2.recs {
                    let recs2 = &recs2[..rec_data_2.unique_rec_end()];
                    self.completion.indexer.in_list_2 =
                        (recs.len() as i8..recs.len() as i8 + recs2.len() as i8).collect();
                    self.completion.recomendations.extend(recs2);
                }
            }
            return;
        }

        let input_lower = self.curr_token().trim_start_matches('-').to_lowercase();
        let if_multiple = self
            .completion
            .indexer
            .multiple
            .then(|| rec_data_2.recs.map(|recs| recs.iter()))
            .flatten();

        let mut recomendations = recs
            .iter()
            .chain(if_multiple.unwrap_or([].iter()))
            .filter(|rec| rec.contains(&input_lower))
            .copied()
            .collect::<Vec<_>>();

        recomendations.sort_unstable_by(|a, b| {
            let a_starts = a.starts_with(&input_lower);
            let b_starts = b.starts_with(&input_lower);
            b_starts.cmp(&a_starts)
        });

        if self.completion.indexer.multiple {
            if let Some(recs2) = rec_data_2.recs {
                for (i, rec) in recomendations.iter().enumerate() {
                    if recs2.contains(rec) {
                        self.completion.indexer.in_list_2.push(i as i8);
                    }
                }
            }
        }

        self.completion.recomendations = recomendations;
    }

    pub fn try_completion(&mut self, direction: Direction) -> io::Result<()> {
        if self.completion.recomendations.is_empty() {
            return Ok(());
        }

        let recomendation = loop {
            if self.completion.recomendations.len() == 1
                && self.curr_token() == self.completion.recomendations[0]
            {
                return Ok(());
            }

            self.completion.indexer.recs += match direction {
                Direction::Next => 1,
                Direction::Previous => -1,
            };

            match self.completion.indexer.recs {
                i if i >= USER_INPUT && i < self.completion.recomendations.len() as i8 => (),
                ..USER_INPUT => {
                    self.completion.indexer.recs = self.completion.recomendations.len() as i8 - 1
                }
                _ => self.completion.indexer.recs = USER_INPUT,
            }

            if self.completion.indexer.recs == USER_INPUT {
                break self.curr_token();
            } else {
                let next = self.completion.recomendations[self.completion.indexer.recs as usize];
                if self.curr_token() != next {
                    break next;
                }
            };
        };

        let new_line = {
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
                                    && self.completion.indexer.recs != USER_INPUT
                                {
                                    "--"
                                } else {
                                    ""
                                }
                            )
                        },
                    )
            };

            let list_i = if self.completion.indexer.multiple
                && self
                    .completion
                    .indexer
                    .in_list_2
                    .contains(&self.completion.indexer.recs)
            {
                self.completion.indexer.list.1
            } else {
                self.completion.indexer.list.0
            };

            match self.completion.rec_list[list_i].kind {
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

    fn default_recomendations(&mut self) {
        let commands = self.completion.rec_list[COMMANDS];
        self.completion.recomendations = commands.recs.as_ref().expect("commands is not empty")
            [..commands.unique_rec_end()]
            .to_vec();
    }

    pub fn reset_completion(&mut self) {
        self.default_recomendations();
        self.completion.input = CompletionState::default();
        self.completion.indexer = Indexer::default();
    }
}
