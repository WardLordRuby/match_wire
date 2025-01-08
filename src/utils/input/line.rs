use crate::{
    commands::handler::{CommandContext, Message},
    strip_ansi_sequences,
    utils::input::{
        completion::{CommandScheme, Completion, Direction},
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
    fmt::Display,
    future::Future,
    io::{self, Stdout, Write},
    pin::Pin,
    sync::atomic::{AtomicUsize, Ordering},
};

pub type InputEventHook = dyn Fn(&mut LineReader, Event) -> io::Result<(EventLoop, bool)>;
pub type InitLineCallback = dyn FnOnce(&mut LineReader) -> io::Result<()>;
pub type CtxCallback = dyn Fn(&mut CommandContext);
/// Note: Currently recursive callbacks are not supported in the returned result
pub type AsyncCtxCallback =
    dyn for<'a> FnOnce(
        &'a mut CommandContext,
    ) -> Pin<Box<dyn Future<Output = Result<EventLoop, InputHookErr>> + 'a>>;

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
    input_hooks: VecDeque<InputHook>,
}

pub struct InputHook {
    uid: usize,
    init: Option<Box<InitLineCallback>>,
    event_hook: Box<InputEventHook>,
}

pub struct InputHookErr {
    uid: usize,
    err: String,
}

impl InputHookErr {
    pub fn new(uid: usize, err: String) -> Self {
        InputHookErr { uid, err }
    }
    pub fn uid(&self) -> usize {
        self.uid
    }
}

impl Display for InputHookErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.err)
    }
}

static CALLBACK_UID: AtomicUsize = AtomicUsize::new(0);

impl InputHook {
    pub fn from(
        uid: usize,
        init: Option<Box<InitLineCallback>>,
        event_hook: Box<InputEventHook>,
    ) -> Self {
        assert_ne!(uid, CALLBACK_UID.load(Ordering::SeqCst));
        InputHook {
            uid,
            init,
            event_hook,
        }
    }

    pub fn with_new_uid(
        init: Option<Box<InitLineCallback>>,
        event_hook: Box<InputEventHook>,
    ) -> Self {
        InputHook {
            uid: Self::new_uid(),
            init,
            event_hook,
        }
    }

    #[inline]
    pub fn uid(&self) -> usize {
        self.uid
    }

    #[inline]
    pub fn new_uid() -> usize {
        CALLBACK_UID.fetch_add(1, Ordering::SeqCst)
    }
}

#[derive(Default)]
pub struct LineData {
    prompt: String,
    prompt_len: u16,
    input: String,
    len: u16,
    comp_enabled: bool,
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
            comp_enabled: true,
            ..Default::default()
        }
    }

    #[inline]
    fn prompt_len(prompt: &str) -> u16 {
        let stripped = strip_ansi_sequences(prompt);
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

pub enum EventLoop {
    Continue,
    AsyncCallback(Box<AsyncCtxCallback>),
    Callback(Box<CtxCallback>),
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
            input_hooks: VecDeque::new(),
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

    /// makes sure the current `input_hook`'s initializer has been executed
    pub fn try_init_input_hook(&mut self) -> Option<io::Result<()>> {
        let callback = self.input_hooks.front_mut()?;
        let init = callback.init.take()?;
        Some(init(self))
    }

    #[inline]
    pub fn register_input_hook(&mut self, input_hook: InputHook) {
        self.input_hooks.push_back(input_hook);
    }

    #[inline]
    /// pops the first queued `input_hook`
    pub fn pop_input_hook(&mut self) -> Option<InputHook> {
        self.input_hooks.pop_front()
    }

    #[inline]
    /// references the first queued `input_hook`
    pub fn next_input_hook(&mut self) -> Option<&InputHook> {
        self.input_hooks.front()
    }

    pub fn print_background_msg(&mut self, msg: Message) -> io::Result<()> {
        let res = self.move_to_beginning(self.line_len());
        msg.print();
        res
    }

    #[inline]
    pub fn completion_enabled(&self) -> bool {
        self.line.comp_enabled
    }

    #[inline]
    pub fn set_completion(&mut self, enabled: bool) {
        self.line.comp_enabled = enabled
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

    fn enter_command(&mut self) -> io::Result<()> {
        self.history
            .prev_entries
            .push(std::mem::take(&mut self.line.input));
        self.reset_history_idx();
        self.new_line()?;
        self.term.queue(cursor::Hide)?.flush()?;
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
        if !self.input_hooks.is_empty() {
            if let Event::Key(KeyEvent {
                kind: KeyEventKind::Press,
                ..
            }) = event
            {
                let hook = self.pop_input_hook().expect("outer if");
                debug_assert!(hook.init.is_none());
                let (event_loop, finished) = (hook.event_hook)(self, event)?;
                if !finished {
                    self.input_hooks.push_front(hook);
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
