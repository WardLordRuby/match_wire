use crossterm::{
    cursor,
    event::{Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers},
    style,
    terminal::{Clear, ClearType::*},
    QueueableCommand,
};
use std::io::{self, Stdout, Write};

pub struct LineReader<'a> {
    prompt: String,
    prompt_len: u16,
    pub line: String,
    line_len: u16,
    pub history: History,
    term: &'a mut Stdout,
    /// (columns, rows)
    term_size: (u16, u16),
    uneventful: bool,
    cursor_at_start: bool,
}

#[derive(Default, Debug)]
pub struct History {
    temp_top: String,
    prev_entries: Vec<String>,
    curr_index: usize,
}

impl History {
    pub fn last(&self) -> &str {
        self.prev_entries.last().unwrap()
    }
}

pub enum EventLoop {
    Continue,
    Break,
    TryProcessCommand,
}

impl<'a> LineReader<'a> {
    pub fn new(prompt: &str, term: &'a mut Stdout, term_size: (u16, u16)) -> io::Result<Self> {
        let new = LineReader {
            prompt: String::from(prompt),
            prompt_len: prompt.chars().count() as u16,
            term,
            line: String::new(),
            line_len: 0,
            history: History::default(),
            term_size,
            uneventful: false,
            cursor_at_start: false,
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

    pub fn uneventful(&mut self) -> bool {
        std::mem::take(&mut self.uneventful)
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
            self.term.queue(cursor::MoveDown(1))?;
        }
        self.term.queue(cursor::MoveToColumn(line_remaining_len))?;
        self.cursor_at_start = false;
        Ok(())
    }

    fn change_line(&mut self, line: String) {
        self.line = line;
        self.line_len = self.line.chars().count() as u16;
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

    fn new_line(&mut self) -> io::Result<()> {
        writeln!(self.term)?;
        self.clear()
    }

    fn insert_char(&mut self, c: char) {
        self.line.push(c);
        self.line_len = self.line_len.saturating_add(1);
    }

    fn remove_char(&mut self) -> io::Result<()> {
        self.line.pop();
        self.move_to_beginning(self.line_len())?;
        self.line_len = self.line_len.saturating_sub(1);
        Ok(())
    }

    fn clear(&mut self) -> io::Result<()> {
        self.line.clear();
        self.move_to_beginning(self.line_len())?;
        self.reset_history();
        self.line_len = 0;
        Ok(())
    }

    fn enter_command(&mut self) -> io::Result<()> {
        self.history
            .prev_entries
            .push(std::mem::take(&mut self.line));
        self.history.curr_index = self.history.prev_entries.len();
        self.new_line()?;
        self.term.queue(cursor::Hide)?;
        Ok(())
    }

    fn reset_history(&mut self) {
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
        self.move_to_beginning(self.line_len())?;
        self.history.curr_index -= 1;
        self.change_line(self.history.prev_entries[self.history.curr_index].clone());
        Ok(())
    }

    fn history_forward(&mut self) -> io::Result<()> {
        let prev_entries_len = self.history.prev_entries.len();
        if self.history.curr_index == prev_entries_len {
            return Ok(());
        }
        self.move_to_beginning(self.line_len())?;
        if self.history.curr_index == prev_entries_len - 1 {
            self.history.curr_index = prev_entries_len;
            let new_line = std::mem::take(&mut self.history.temp_top);
            self.change_line(new_line);
            return Ok(());
        }
        self.history.curr_index += 1;
        self.change_line(self.history.prev_entries[self.history.curr_index].clone());
        Ok(())
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
                self.clear()?;
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
                let line = self.line.trim();
                if line.is_empty() {
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
