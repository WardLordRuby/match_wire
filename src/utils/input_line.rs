use crossterm::{
    cursor,
    event::{Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers},
    style::{self, Stylize},
    terminal::{self, Clear, ClearType::FromCursorDown},
    QueueableCommand,
};
use std::{
    collections::HashMap,
    io::{self, Stdout, Write},
};

pub struct LineReader<'a> {
    prompt: String,
    prompt_len: u16,
    line: String,
    line_len: u16,
    history: History,
    term: &'a mut Stdout,
    /// (columns, rows)
    term_size: (u16, u16),
    uneventful: bool,
    cursor_at_start: bool,
    command_entered: bool,
    completion: Completion,
}

#[derive(Default, Debug)]
pub struct History {
    temp_top: String,
    prev_entries: Vec<String>,
    curr_index: usize,
}

// MARK: TODO
// 1. Add colors for:
//   - commands
//   - arguments
//   - quoted strings
// 2. Add support for a movable cursor

// would be nice if curr_command and user_input could be refs into LineReader.line
// without using unsafe raw pointers

struct Completion {
    recomendations: Vec<&'static str>,
    rec_i: i8,
    commands: Vec<&'static str>,
    commands_short: Vec<&'static str>,
    curr_command: Option<String>,
    user_input: String,
    argument_map: HashMap<&'static str, Option<&'static Vec<&'static str>>>,
}

const USER_INPUT: i8 = -1;

impl From<&'static [NameScheme]> for Completion {
    fn from(value: &'static [NameScheme]) -> Self {
        let (commands_short, commands, argument_map) = value.iter().fold(
            (Vec::with_capacity(value.len()), Vec::new(), HashMap::new()),
            |(mut commands_short, mut commands, mut argument_map), scheme| {
                for (i, &command) in scheme.command.iter().enumerate() {
                    if i == 0 {
                        commands_short.push(command)
                    }
                    commands.push(command);
                    argument_map.insert(command, scheme.arguments.as_ref());
                }
                (commands_short, commands, argument_map)
            },
        );
        Completion {
            recomendations: commands_short.clone(),
            rec_i: USER_INPUT,
            curr_command: None,
            user_input: String::new(),
            argument_map,
            commands,
            commands_short,
        }
    }
}

pub enum EventLoop {
    Continue,
    Break,
    TryProcessCommand,
}

pub struct NameScheme {
    command: Vec<&'static str>,
    arguments: Option<Vec<&'static str>>,
}

// MARK: IMPROVE
// HARD: this ideally should be done by a proc-macro
pub fn init_completion() -> Vec<NameScheme> {
    vec![
        NameScheme {
            command: vec!["filter"],
            arguments: Some(vec![
                "limit",
                "player-min",
                "team-size-max",
                "region",
                "includes",
                "excludes",
                "help",
            ]),
        },
        NameScheme {
            command: vec!["reconnect"],
            arguments: Some(vec!["history", "connect", "help"]),
        },
        NameScheme {
            command: vec!["launch"],
            arguments: None,
        },
        NameScheme {
            command: vec!["update-cache", "update", "reset"],
            arguments: None,
        },
        NameScheme {
            command: vec!["display-logs", "display", "logs"],
            arguments: None,
        },
        NameScheme {
            command: vec!["game-dir", "gamedir"],
            arguments: None,
        },
        NameScheme {
            command: vec!["quit"],
            arguments: None,
        },
        NameScheme {
            command: vec!["local-env", "localenv"],
            arguments: None,
        },
        NameScheme {
            command: vec!["help"],
            arguments: None,
        },
    ]
}

impl<'a> LineReader<'a> {
    pub fn new(
        prompt: &str,
        term: &'a mut Stdout,
        name_ctx: &'static [NameScheme],
    ) -> io::Result<Self> {
        let new = LineReader {
            prompt: String::from(prompt),
            prompt_len: prompt.chars().count() as u16,
            term,
            line: String::new(),
            line_len: 0,
            history: History::default(),
            term_size: terminal::size().unwrap(),
            uneventful: false,
            cursor_at_start: false,
            command_entered: true,
            completion: Completion::from(name_ctx),
        };
        new.term.queue(cursor::EnableBlinking)?;
        Ok(new)
    }

    pub async fn clear_unwanted_inputs(
        &mut self,
        stream: &mut crossterm::event::EventStream,
    ) -> io::Result<()> {
        use tokio_stream::StreamExt;

        let _ = tokio::time::timeout(tokio::time::Duration::from_millis(10), async {
            while stream.fuse().next().await.is_some() {}
        })
        .await;
        self.term.queue(cursor::Show)?;
        Ok(())
    }

    /// Note: will panic if called when nothing is in the history
    pub fn last_line(&self) -> &str {
        self.history
            .prev_entries
            .last()
            .expect("only called after `self.enter_command()`")
    }

    pub fn uneventful(&mut self) -> bool {
        std::mem::take(&mut self.uneventful)
    }

    pub fn command_entered(&mut self) -> bool {
        std::mem::take(&mut self.command_entered)
    }

    /// gets the number of lines wrapped
    pub fn line_height(&self, line_len: u16) -> u16 {
        line_len / self.term_size.0
    }

    /// gets the total length of the line (prompt + user input)
    pub fn line_len(&self) -> u16 {
        self.prompt_len.saturating_add(self.line_len)
    }

    fn line_remainder(&self, line_len: u16) -> u16 {
        line_len % self.term_size.0
    }

    pub fn move_to_beginning(&mut self, from: u16) -> io::Result<()> {
        let line_height = self.line_height(from);
        if line_height != 0 {
            self.term.queue(cursor::MoveUp(line_height))?;
        }
        self.term
            .queue(cursor::MoveToColumn(0))?
            .queue(Clear(FromCursorDown))?;

        self.cursor_at_start = true;
        Ok(())
    }

    fn move_to_line_end(&mut self, line_len: u16) -> io::Result<()> {
        let line_remaining_len = self.line_remainder(line_len);
        if line_remaining_len == 0 {
            writeln!(self.term)?;
        }
        self.term.queue(cursor::MoveToColumn(line_remaining_len))?;
        self.cursor_at_start = false;
        Ok(())
    }

    pub fn render(&mut self) -> io::Result<()> {
        let line_len = self.line_len();
        if !self.cursor_at_start {
            self.move_to_beginning(line_len.saturating_sub(1))?;
        }

        self.term.queue(style::ResetColor)?;

        write!(self.term, "{}{}", self.prompt, self.line)?;

        self.move_to_line_end(line_len)?;
        self.term.flush()
    }

    fn insert_char(&mut self, c: char) {
        self.line.push(c);
        self.line_len = self.line_len.saturating_add(1);
        self.update_completeion();
    }

    fn remove_char(&mut self) -> io::Result<()> {
        self.line.pop();
        self.move_to_beginning(self.line_len())?;
        self.line_len = self.line_len.saturating_sub(1);
        self.update_completeion();
        Ok(())
    }

    fn new_line(&mut self) -> io::Result<()> {
        writeln!(self.term)?;
        self.clear_line()
    }

    fn ctrl_c_line(&mut self) -> io::Result<()> {
        writeln!(self.term, "{}", "^C".red())?;
        self.clear_line()
    }

    fn clear_line(&mut self) -> io::Result<()> {
        self.line.clear();
        self.reset_completion();
        self.reset_history_idx();
        self.move_to_beginning(self.line_len())?;
        self.line_len = 0;
        Ok(())
    }

    fn change_line(&mut self, line: String) -> io::Result<()> {
        self.move_to_beginning(self.line_len())?;
        self.line = line;
        self.line_len = self.line.chars().count() as u16;
        Ok(())
    }

    fn enter_command(&mut self) -> io::Result<()> {
        self.history
            .prev_entries
            .push(std::mem::take(&mut self.line));
        self.reset_history_idx();
        self.new_line()?;
        self.term.queue(cursor::Hide)?;
        self.command_entered = true;
        Ok(())
    }

    fn reset_history_idx(&mut self) {
        self.history.curr_index = self.history.prev_entries.len();
    }

    fn history_back(&mut self) -> io::Result<()> {
        let prev_entries_len = self.history.prev_entries.len();
        if self.history.curr_index == 0 || prev_entries_len == 0 {
            return Ok(());
        }
        if !self.history.prev_entries.contains(&self.line)
            && self.history.curr_index == prev_entries_len
        {
            self.history.temp_top = std::mem::take(&mut self.line);
        }
        self.history.curr_index -= 1;
        self.change_line(self.history.prev_entries[self.history.curr_index].clone())
    }

    fn history_forward(&mut self) -> io::Result<()> {
        let prev_entries_len = self.history.prev_entries.len();
        if self.history.curr_index == prev_entries_len {
            return Ok(());
        }
        let new_line = if self.history.curr_index == prev_entries_len - 1 {
            self.history.curr_index = prev_entries_len;
            std::mem::take(&mut self.history.temp_top)
        } else {
            self.history.curr_index += 1;
            self.history.prev_entries[self.history.curr_index].clone()
        };
        self.change_line(new_line)
    }

    fn update_completeion(&mut self) {
        if self.line.is_empty() {
            self.reset_completion();
            return;
        }
        self.completion.rec_i = USER_INPUT;
        self.completion.user_input = if self.line.ends_with(' ') {
            if self.completion.curr_command.is_none() {
                self.completion.curr_command =
                    Some(self.line.split_once(' ').expect("outer if").0.to_string())
            }
            String::new()
        } else if let Some(i) = self.line.rfind(' ') {
            self.line[i + ' '.len_utf8()..]
                .trim_start_matches('-')
                .to_string()
        } else {
            self.completion.curr_command = None;
            self.line.trim_start_matches('-').to_string()
        };
        let mut recomendations = if let Some(ref command) = self.completion.curr_command {
            let Some(Some(command_args)) = self.completion.argument_map.get(command.as_str())
            else {
                self.completion.recomendations = Vec::new();
                return;
            };
            if self.completion.user_input.is_empty() {
                self.completion.recomendations = command_args.to_vec();
                return;
            }
            command_args
                .iter()
                .filter(|argument| argument.contains(&self.completion.user_input))
                .copied()
                .collect::<Vec<_>>()
        } else {
            self.completion
                .commands
                .iter()
                .filter(|command| command.contains(&self.completion.user_input))
                .copied()
                .collect::<Vec<_>>()
        };
        recomendations.sort_unstable_by(|a, b| {
            let a_starts = a.starts_with(&self.completion.user_input);
            let b_starts = b.starts_with(&self.completion.user_input);
            b_starts.cmp(&a_starts)
        });
        self.completion.recomendations = recomendations;
    }

    fn try_completion(&mut self, by: i8) -> io::Result<()> {
        if self.completion.recomendations.is_empty() {
            return Ok(());
        }
        let last = if self.completion.rec_i == USER_INPUT {
            &self.completion.user_input
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
                &self.completion.user_input
            } else {
                self.completion.recomendations[self.completion.rec_i as usize]
            };
            if self.completion.curr_command.is_some() {
                format!(
                    "{}--{recomendation}",
                    self.line.trim_end_matches(last).trim_end_matches('-')
                )
            } else {
                recomendation.to_string()
            }
        };
        self.change_line(new_line)
    }

    fn reset_completion(&mut self) {
        self.completion.curr_command = None;
        self.completion.recomendations = self.completion.commands_short.clone();
        self.completion.user_input = String::new();
        self.completion.rec_i = USER_INPUT;
    }

    pub fn process_input_event(&mut self, event: Event) -> io::Result<EventLoop> {
        match event {
            Event::Key(KeyEvent {
                kind: KeyEventKind::Release,
                ..
            }) => {
                self.uneventful = true;
                Ok(EventLoop::Continue)
            }
            Event::Key(KeyEvent {
                code: KeyCode::Char('c'),
                kind: KeyEventKind::Press,
                modifiers: KeyModifiers::CONTROL,
                ..
            }) => {
                if self.line.is_empty() {
                    return Ok(EventLoop::Break);
                }
                self.ctrl_c_line()?;
                Ok(EventLoop::Continue)
            }
            Event::Key(KeyEvent {
                code: KeyCode::Tab,
                kind: KeyEventKind::Press,
                ..
            }) => {
                self.try_completion(1)?;
                Ok(EventLoop::Continue)
            }
            Event::Key(KeyEvent {
                code: KeyCode::BackTab,
                kind: KeyEventKind::Press,
                ..
            }) => {
                self.try_completion(-1)?;
                Ok(EventLoop::Continue)
            }
            Event::Key(KeyEvent {
                code: KeyCode::Char(c),
                kind: KeyEventKind::Press,
                ..
            }) => {
                self.insert_char(c);
                Ok(EventLoop::Continue)
            }
            Event::Key(KeyEvent {
                code: KeyCode::Backspace,
                kind: KeyEventKind::Press,
                ..
            }) => {
                self.remove_char()?;
                Ok(EventLoop::Continue)
            }
            Event::Key(KeyEvent {
                code: KeyCode::Up,
                kind: KeyEventKind::Press,
                ..
            }) => {
                self.history_back()?;
                Ok(EventLoop::Continue)
            }
            Event::Key(KeyEvent {
                code: KeyCode::Down,
                kind: KeyEventKind::Press,
                ..
            }) => {
                self.history_forward()?;
                Ok(EventLoop::Continue)
            }
            Event::Key(KeyEvent {
                code: KeyCode::Enter,
                kind: KeyEventKind::Press,
                ..
            }) => {
                if self.line.trim().is_empty() {
                    self.new_line()?;
                    return Ok(EventLoop::Continue);
                }
                self.enter_command()?;
                Ok(EventLoop::TryProcessCommand)
            }
            Event::Resize(x, y) => {
                self.term_size = (x, y);
                Ok(EventLoop::Continue)
            }
            _ => Ok(EventLoop::Continue),
        }
    }
}
