use crate::utils::input::{
    completion::{CommandScheme, Completion, Direction},
    style::PROMPT_END,
};
use crossterm::{
    cursor,
    event::{Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers},
    style::Stylize,
    terminal::{Clear, ClearType::FromCursorDown},
    QueueableCommand,
};
use shellwords::split as shellwords_split;
use std::{
    collections::VecDeque,
    fmt::Display,
    future::Future,
    io::{self, ErrorKind, Write},
    pin::Pin,
    sync::atomic::{AtomicUsize, Ordering},
};
use strip_ansi::strip_ansi;
use tokio::time::{timeout, Duration};
use tokio_stream::StreamExt;

pub type InputEventHook<Ctx, W> =
    dyn Fn(&mut LineReader<Ctx, W>, Event) -> io::Result<HookedEvent<Ctx>>;
pub type InitLineCallback<Ctx, W> = dyn FnOnce(&mut LineReader<Ctx, W>) -> io::Result<()>;
pub type Callback<Ctx> = dyn Fn(&mut Ctx);
pub type AsyncCallback<Ctx> =
    dyn for<'a> FnOnce(&'a mut Ctx) -> Pin<Box<dyn Future<Output = Result<(), InputHookErr>> + 'a>>;

pub trait Print {
    fn print(&self);
}

pub struct LineReader<Ctx, W: Write> {
    pub completion: Completion,
    pub line: LineData,
    history: History,
    term: W,
    /// (columns, rows)
    term_size: (u16, u16),
    uneventful: bool,
    custom_quit: Option<Vec<String>>,
    cursor_at_start: bool,
    command_entered: bool,
    input_hooks: VecDeque<InputHook<Ctx, W>>,
}

pub struct LineReaderBuilder<'a, W: Write> {
    completion: Option<&'static CommandScheme>,
    custom_quit: Option<&'a str>,
    term: Option<W>,
    term_size: Option<(u16, u16)>,
    prompt: Option<String>,
    prompt_end: Option<&'static str>,
}

impl<W: Write> Default for LineReaderBuilder<'_, W> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a, W: Write> LineReaderBuilder<'a, W> {
    pub fn with_custom_quit_command(mut self, quit_cmd: &'a str) -> Self {
        self.custom_quit = Some(quit_cmd);
        self
    }
}

impl<W: Write> LineReaderBuilder<'_, W> {
    pub fn new() -> Self {
        LineReaderBuilder {
            completion: None,
            custom_quit: None,
            term: None,
            term_size: None,
            prompt: None,
            prompt_end: None,
        }
    }

    /// `LineReader` must include a terminal that is compatable with executing commands via the `crossterm` crate.
    pub fn terminal(mut self, term: W) -> Self {
        self.term = Some(term);
        self
    }

    /// `LineReader` must include a terminal size so rendering the window is displayed correctly.  
    /// `size`: (columns, rows)
    pub fn terminal_size(mut self, size: (u16, u16)) -> Self {
        self.term_size = Some(size);
        self
    }

    pub fn with_completion(mut self, completion: &'static CommandScheme) -> Self {
        self.completion = Some(completion);
        self
    }

    pub fn with_prompt(mut self, prompt: &str) -> Self {
        self.prompt = Some(String::from(prompt));
        self
    }

    pub fn with_custom_prompt_separator(mut self, separator: &'static str) -> Self {
        self.prompt_end = Some(separator);
        self
    }

    pub fn build<Ctx>(self) -> io::Result<LineReader<Ctx, W>> {
        let mut term = self
            .term
            .ok_or_else(|| io::Error::new(ErrorKind::NotFound, "terminal is required"))?;
        let term_size = self
            .term_size
            .ok_or_else(|| io::Error::new(ErrorKind::NotFound, "terminal size is required"))?;
        let custom_quit = match self.custom_quit {
            Some(quit_cmd) => Some(shellwords_split(quit_cmd).map_err(|_| {
                io::Error::new(
                    ErrorKind::InvalidInput,
                    format!("Custom quit command: {quit_cmd}, contains mismatched quotes"),
                )
            })?),
            None => None,
        };
        term.queue(cursor::EnableBlinking)?;
        Ok(LineReader {
            line: LineData::new(self.prompt, self.prompt_end, self.completion.is_some()),
            history: History::default(),
            term,
            term_size,
            uneventful: false,
            cursor_at_start: false,
            command_entered: true,
            custom_quit,
            completion: self.completion.map(Completion::from).unwrap_or_default(),
            input_hooks: VecDeque::new(),
        })
    }
}

pub struct InputHook<Ctx, W: Write> {
    uid: HookUID,
    init: Option<Box<InitLineCallback<Ctx, W>>>,
    on_async_callback_err: Option<Box<Callback<Ctx>>>,
    event_hook: Box<InputEventHook<Ctx, W>>,
}

static CALLBACK_UID: AtomicUsize = AtomicUsize::new(0);

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct HookUID(usize);

impl HookUID {
    #[inline]
    pub fn new() -> Self {
        Self(CALLBACK_UID.fetch_add(1, Ordering::SeqCst))
    }
}

impl Default for HookUID {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

pub struct InputHookErr {
    uid: HookUID,
    err: String,
}

impl InputHookErr {
    pub fn new(uid: HookUID, err: String) -> Self {
        InputHookErr { uid, err }
    }
    pub fn uid(&self) -> HookUID {
        self.uid
    }
}

impl Display for InputHookErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.err)
    }
}

impl<Ctx, W: Write> InputHook<Ctx, W> {
    pub fn new(
        uid: HookUID,
        init: Option<Box<InitLineCallback<Ctx, W>>>,
        on_async_callback_err: Option<Box<Callback<Ctx>>>,
        event_hook: Box<InputEventHook<Ctx, W>>,
    ) -> Self {
        assert!(uid.0 < CALLBACK_UID.load(Ordering::SeqCst));
        InputHook {
            uid,
            init,
            on_async_callback_err,
            event_hook,
        }
    }

    pub fn with_new_uid(
        init: Option<Box<InitLineCallback<Ctx, W>>>,
        on_async_callback_err: Option<Box<Callback<Ctx>>>,
        event_hook: Box<InputEventHook<Ctx, W>>,
    ) -> Self {
        InputHook {
            uid: HookUID::new(),
            init,
            on_async_callback_err,
            event_hook,
        }
    }

    #[inline]
    pub fn uid(&self) -> HookUID {
        self.uid
    }
}

#[derive(Default)]
pub struct LineData {
    inital_prompt: String,
    prompt: String,
    prompt_end: &'static str,
    prompt_len: u16,
    input: String,
    len: u16,
    comp_enabled: bool,
    err: bool,
}

impl LineData {
    fn new(
        prompt: Option<String>,
        prompt_separator: Option<&'static str>,
        completion_enabled: bool,
    ) -> Self {
        let prompt = prompt.unwrap_or_else(Self::default_prompt);
        LineData {
            prompt_len: LineData::prompt_len(&prompt),
            prompt_end: prompt_separator.unwrap_or(PROMPT_END),
            inital_prompt: prompt.clone(),
            prompt,
            comp_enabled: completion_enabled,
            ..Default::default()
        }
    }

    #[inline]
    fn prompt_len(prompt: &str) -> u16 {
        let stripped = strip_ansi(prompt);
        stripped.chars().count() as u16 + PROMPT_END.chars().count() as u16
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
    pub fn prompt_separator(&self) -> &'static str {
        self.prompt_end
    }

    #[inline]
    pub fn found_err(&mut self, found: bool) {
        self.err = found
    }

    #[inline]
    pub fn err(&self) -> bool {
        self.err
    }

    #[inline]
    pub fn completion_enabled(&self) -> bool {
        self.comp_enabled
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

#[non_exhaustive]
pub enum ParseErr {
    MismatchedQuotes,
}

impl Display for ParseErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ParseErr::MismatchedQuotes => "Mismatched quotes",
            }
        )
    }
}

pub enum HookControl {
    Continue,
    Release,
}

pub struct HookedEvent<Ctx> {
    event: EventLoop<Ctx>,
    new_state: HookControl,
}

impl<Ctx> HookedEvent<Ctx> {
    #[inline]
    pub fn new(event: EventLoop<Ctx>, new_state: HookControl) -> io::Result<Self> {
        Ok(Self { event, new_state })
    }

    #[inline]
    pub fn continue_hook() -> io::Result<Self> {
        Ok(Self {
            event: EventLoop::Continue,
            new_state: HookControl::Continue,
        })
    }

    #[inline]
    pub fn release_hook() -> io::Result<Self> {
        Ok(Self {
            event: EventLoop::Continue,
            new_state: HookControl::Release,
        })
    }
}

pub enum EventLoop<Ctx> {
    Continue,
    AsyncCallback(Box<AsyncCallback<Ctx>>),
    Callback(Box<Callback<Ctx>>),
    Break,
    TryProcessInput(Result<Vec<String>, ParseErr>),
}

impl<Ctx, W: Write> LineReader<Ctx, W> {
    pub async fn clear_unwanted_inputs(
        &mut self,
        stream: &mut crossterm::event::EventStream,
    ) -> io::Result<()> {
        if !std::mem::take(&mut self.command_entered) {
            return Ok(());
        }

        let _ = timeout(Duration::from_millis(10), async {
            while stream.fuse().next().await.is_some() {}
        })
        .await;
        self.term.queue(cursor::Show)?;
        Ok(())
    }

    fn return_to_initial_state(&mut self) {
        if self.line.prompt != self.line.inital_prompt {
            self.set_prompt(self.line.inital_prompt.clone());
        }
        self.enable_completion();
    }

    /// makes sure the current `input_hook`'s initializer has been executed
    fn try_init_input_hook(&mut self) -> Option<io::Result<()>> {
        let callback = self.input_hooks.front_mut()?;
        let init = callback.init.take()?;
        Some(init(self))
    }

    #[inline]
    pub fn register_input_hook(&mut self, input_hook: InputHook<Ctx, W>) {
        self.input_hooks.push_back(input_hook);
    }

    pub fn conditionally_remove_hook(&mut self, ctx: &mut Ctx, uid: HookUID) {
        if self.next_input_hook().is_some_and(|hook| hook.uid() == uid) {
            self.return_to_initial_state();
            if let Some(err_callback) = self
                .pop_input_hook()
                .expect("`next_input_hook` & `pop_input_hook` both look at first queued hook")
                .on_async_callback_err
            {
                err_callback(ctx);
            };
        }
    }

    #[inline]
    /// pops the first queued `input_hook`
    pub fn pop_input_hook(&mut self) -> Option<InputHook<Ctx, W>> {
        self.input_hooks.pop_front()
    }

    #[inline]
    /// references the first queued `input_hook`
    pub fn next_input_hook(&mut self) -> Option<&InputHook<Ctx, W>> {
        self.input_hooks.front()
    }

    pub fn print_background_msg(&mut self, msg: impl Print) -> io::Result<()> {
        let res = self.move_to_beginning(self.line_len());
        msg.print();
        res
    }

    #[inline]
    pub fn completion_enabled(&self) -> bool {
        self.line.comp_enabled
    }

    #[inline]
    pub fn enable_completion(&mut self) {
        if self.completion.is_empty() {
            return;
        }
        self.line.comp_enabled = true
    }

    #[inline]
    pub fn disable_completion(&mut self) {
        self.line.comp_enabled = false
    }

    pub fn set_prompt(&mut self, prompt: String) {
        self.line.prompt_len = LineData::prompt_len(&prompt);
        self.line.prompt = prompt;
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
        if std::mem::take(&mut self.uneventful) {
            return Ok(());
        }
        if let Some(res) = self.try_init_input_hook() {
            res?
        };

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
        if self.line.comp_enabled {
            self.update_completeion();
        }
    }

    pub fn remove_char(&mut self) -> io::Result<()> {
        self.line.input.pop();
        self.move_to_beginning(self.line_len())?;
        self.line.len = self.line.len.saturating_sub(1);
        if self.line.comp_enabled {
            self.update_completeion();
        }
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

    fn enter_command(&mut self) -> io::Result<&str> {
        self.history
            .prev_entries
            .push(std::mem::take(&mut self.line.input));
        self.reset_history_idx();
        self.new_line()?;
        self.term.queue(cursor::Hide)?.flush()?;
        self.command_entered = true;

        Ok(self
            .history
            .prev_entries
            .last()
            .expect("just pushed into `prev_entries`"))
    }

    fn reset_history_idx(&mut self) {
        self.history.curr_index = self.history.prev_entries.len();
    }

    fn history_back(&mut self) -> io::Result<()> {
        if self.history.curr_index == 0 || self.history.prev_entries.is_empty() {
            return Ok(());
        }
        if !self.history.prev_entries.contains(&self.line.input)
            && self.history.curr_index == self.history.prev_entries.len()
        {
            self.history.temp_top = std::mem::take(&mut self.line.input);
        }
        self.history.curr_index -= 1;
        self.change_line(self.history.prev_entries[self.history.curr_index].clone())
    }

    fn history_forward(&mut self) -> io::Result<()> {
        if self.history.curr_index == self.history.prev_entries.len() {
            return Ok(());
        }
        let new_line = if self.history.curr_index == self.history.prev_entries.len() - 1 {
            self.history.curr_index = self.history.prev_entries.len();
            std::mem::take(&mut self.history.temp_top)
        } else {
            self.history.curr_index += 1;
            self.history.prev_entries[self.history.curr_index].clone()
        };
        self.change_line(new_line)
    }

    pub fn process_input_event(&mut self, event: Event) -> io::Result<EventLoop<Ctx>> {
        if !self.input_hooks.is_empty() {
            if let Event::Key(KeyEvent {
                kind: KeyEventKind::Press,
                ..
            }) = event
            {
                let hook = self.pop_input_hook().expect("outer if");
                debug_assert!(hook.init.is_none());
                let hook_output = (hook.event_hook)(self, event)?;
                match hook_output.new_state {
                    HookControl::Continue => self.input_hooks.push_front(hook),
                    HookControl::Release => self.return_to_initial_state(),
                }
                return Ok(hook_output.event);
            }
        }
        match event {
            Event::Key(KeyEvent {
                code: KeyCode::Char('c'),
                kind: KeyEventKind::Press,
                modifiers: KeyModifiers::CONTROL,
                ..
            }) => {
                let line_was_empty = self.line.input.is_empty();
                self.ctrl_c_line()?;
                if line_was_empty {
                    if let Some(quit_cmd) = self.custom_quit.clone() {
                        self.term.queue(cursor::Hide)?.flush()?;
                        self.command_entered = true;
                        return Ok(EventLoop::TryProcessInput(Ok(quit_cmd)));
                    }
                    return Ok(EventLoop::Break);
                }
                Ok(EventLoop::Continue)
            }
            Event::Key(KeyEvent {
                code: KeyCode::Tab,
                kind: KeyEventKind::Press,
                ..
            }) => {
                self.try_completion(Direction::Next)?;
                Ok(EventLoop::Continue)
            }
            Event::Key(KeyEvent {
                code: KeyCode::BackTab,
                kind: KeyEventKind::Press,
                ..
            }) => {
                self.try_completion(Direction::Previous)?;
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
                Ok(EventLoop::TryProcessInput(
                    shellwords_split(self.enter_command()?).map_err(|_| ParseErr::MismatchedQuotes),
                ))
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
