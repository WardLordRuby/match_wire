use crate::{
    commands::handler::{CommandContext, Message},
    utils::input::{
        completion::{CommandScheme, Completion},
        style::PROMPT_END,
    },
};
use crossterm::{
    cursor,
    event::{Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers},
    style::Stylize,
    terminal::{self, Clear, ClearType::FromCursorDown},
    QueueableCommand,
};
use std::{
    collections::VecDeque,
    io::{self, Stdout, Write},
};
use tracing::{error, info};

pub type InputEventHook = dyn Fn(&mut LineReader, Event) -> io::Result<(EventLoop, bool)>;
pub type LineCallback = dyn Fn(&mut LineReader) -> io::Result<()>;
pub type ContextCallback = dyn Fn(&mut CommandContext);

pub struct LineReader<'a> {
    pub completion: Completion,
    pub line: LineData,
    history: History,
    term: &'a mut Stdout,
    /// (columns, rows)
    term_size: (u16, u16),
    uneventful: bool,
    cursor_at_start: bool,
    command_entered: bool,
    callback: VecDeque<Box<InputEventHook>>,
}

#[derive(Default)]
pub struct LineData {
    prompt: String,
    prompt_len: u16,
    input: String,
    len: u16,
    err: bool,
}

impl LineData {
    fn new(mut prompt: String) -> Self {
        if prompt.is_empty() {
            prompt = LineData::default_prompt();
        }
        LineData {
            prompt_len: LineData::prompt_len(&prompt),
            prompt,
            ..Default::default()
        }
    }

    #[inline]
    fn prompt_len(prompt: &str) -> u16 {
        prompt.chars().count() as u16 + PROMPT_END.chars().count() as u16
    }

    #[inline]
    pub fn default_prompt() -> String {
        format!("{}.exe", env!("CARGO_PKG_NAME"))
    }

    #[inline]
    pub fn input(&self) -> &str {
        &self.input
    }

    #[inline]
    pub fn take_input(&mut self) -> String {
        std::mem::take(&mut self.input)
    }

    #[inline]
    pub fn prompt(&self) -> &str {
        &self.prompt
    }

    #[inline]
    pub fn found_err(&mut self, found: bool) {
        self.err = found
    }

    #[inline]
    pub fn err(&self) -> bool {
        self.err
    }
}

#[derive(Default)]
pub struct History {
    temp_top: String,
    prev_entries: Vec<String>,
    curr_index: usize,
}

// MARK: TODO
// Add support for a movable cursor
// currently `CompletionState` only supports char events at line end
// `CompletionState` will have to be carefully mannaged if cursor is moveable

pub enum EventLoop {
    Continue,
    /// Sending a command and marking the calling `InputLineHook` as finished can lead to  
    /// undefined behavior if `try_send_command` errors
    SendCommand(String),
    Callback(Box<ContextCallback>),
    Break,
    TryProcessCommand,
}

impl<'a> LineReader<'a> {
    pub fn new(
        prompt: String,
        term: &'a mut Stdout,
        name_ctx: &'static CommandScheme,
    ) -> io::Result<Self> {
        let new = LineReader {
            line: LineData::new(prompt),
            history: History::default(),
            term,
            term_size: terminal::size().unwrap(),
            uneventful: false,
            cursor_at_start: false,
            command_entered: true,
            completion: Completion::from(name_ctx),
            callback: VecDeque::new(),
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

    pub fn print_background_msg(&mut self, msg: Message) -> io::Result<()> {
        self.move_to_beginning(self.line_len())?;
        match msg {
            Message::Str(msg) => println!("{msg}"),
            Message::Info(msg) => info!("{msg}"),
            Message::Err(msg) => error!("{msg}"),
        }
        Ok(())
    }

    /// Note: will panic if called when nothing is in the history
    pub fn last_line(&self) -> &str {
        self.history
            .prev_entries
            .last()
            .expect("only called after `self.enter_command()`")
    }

    pub fn set_prompt(&mut self, prompt: String) {
        self.line.prompt_len = LineData::prompt_len(&prompt);
        self.line.prompt = prompt;
    }

    #[inline]
    pub fn register_callback(&mut self, f: Box<InputEventHook>) {
        self.callback.push_back(f);
    }

    #[inline]
    /// pops the first queued callback
    pub fn pop_callback(&mut self) -> Option<Box<InputEventHook>> {
        self.callback.pop_front()
    }

    #[inline]
    pub fn uneventful(&mut self) -> bool {
        std::mem::take(&mut self.uneventful)
    }

    #[inline]
    pub fn command_entered(&mut self) -> bool {
        std::mem::take(&mut self.command_entered)
    }

    #[inline]
    /// gets the number of lines wrapped
    pub fn line_height(&self, line_len: u16) -> u16 {
        line_len / self.term_size.0
    }

    #[inline]
    /// gets the total length of the line (prompt + user input)
    pub fn line_len(&self) -> u16 {
        self.line.prompt_len.saturating_add(self.line.len)
    }

    #[inline]
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

        write!(self.term, "{}", self.line)?;

        self.move_to_line_end(line_len)?;
        self.term.flush()
    }

    pub fn insert_char(&mut self, c: char) {
        self.line.input.push(c);
        self.line.len = self.line.len.saturating_add(1);
        self.update_completeion();
    }

    pub fn remove_char(&mut self) -> io::Result<()> {
        self.line.input.pop();
        self.move_to_beginning(self.line_len())?;
        self.line.len = self.line.len.saturating_sub(1);
        self.update_completeion();
        Ok(())
    }

    pub fn new_line(&mut self) -> io::Result<()> {
        writeln!(self.term)?;
        self.clear_line()
    }

    pub fn ctrl_c_line(&mut self) -> io::Result<()> {
        writeln!(self.term, "{}", "^C".red())?;
        self.clear_line()
    }

    fn clear_line(&mut self) -> io::Result<()> {
        self.reset_line_data();
        self.move_to_beginning(self.line_len())?;
        self.reset_completion();
        self.reset_history_idx();
        Ok(())
    }

    fn reset_line_data(&mut self) {
        self.line.input.clear();
        self.line.len = 0;
        self.line.err = false;
    }

    pub fn change_line(&mut self, line: String) -> io::Result<()> {
        self.move_to_beginning(self.line_len())?;
        self.line.len = line.chars().count() as u16;
        self.line.input = line;
        Ok(())
    }

    fn enter_command(&mut self) -> io::Result<()> {
        self.history
            .prev_entries
            .push(std::mem::take(&mut self.line.input));
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
        if !self.history.prev_entries.contains(&self.line.input)
            && self.history.curr_index == prev_entries_len
        {
            self.history.temp_top = std::mem::take(&mut self.line.input);
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

    pub fn process_input_event(&mut self, event: Event) -> io::Result<EventLoop> {
        if !self.callback.is_empty() {
            if let Event::Key(KeyEvent {
                kind: KeyEventKind::Press,
                ..
            }) = event
            {
                let callback = self.pop_callback().expect("outer if");
                let (event_loop, finished) = callback(self, event)?;
                if !finished {
                    self.callback.push_front(callback);
                }
                return Ok(event_loop);
            }
        }
        match event {
            Event::Key(KeyEvent {
                code: KeyCode::Char('c'),
                kind: KeyEventKind::Press,
                modifiers: KeyModifiers::CONTROL,
                ..
            }) => {
                if self.line.input.is_empty() {
                    self.line.input.push_str("quit");
                    self.enter_command()?;
                    return Ok(EventLoop::TryProcessCommand);
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
                if self.line.input.trim().is_empty() {
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
            _ => {
                self.uneventful = true;
                Ok(EventLoop::Continue)
            }
        }
    }
}
