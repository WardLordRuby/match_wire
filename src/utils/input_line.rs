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
    pub line: String,
    pub history: History,
    term: &'a mut Stdout,
    /// (columns, rows)
    term_size: (u16, u16),
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

// MARK: TODO
// deal with wrapping text

impl<'a> LineReader<'a> {
    pub fn new(prompt: &str, term: &'a mut Stdout, term_size: (u16, u16)) -> io::Result<Self> {
        let new = LineReader {
            prompt: String::from(prompt),
            term,
            line: String::new(),
            history: History::default(),
            term_size,
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

    /// gets the number of lines wrapped
    fn line_height(&self, pos: u16) -> u16 {
        pos / self.term_size.0
    }

    // MARK: TODO
    // why don't we always want to move from curr_col_len to beginning?
    pub fn move_to_beginning(&mut self, from: u16) -> io::Result<()> {
        let move_up = self.line_height(from.saturating_sub(1));
        self.term
            .queue(cursor::MoveToColumn(0))?
            .queue(Clear(FromCursorDown))?;
        if move_up != 0 {
            self.term.queue(cursor::MoveUp(move_up))?;
        }
        Ok(())
    }

    fn move_to_line_end(&mut self) -> io::Result<()> {
        let line_len = self.get_curr_len();
        let line_height = self.line_height(line_len.saturating_sub(1));
        let line_remaining_len = line_len % self.term_size.0; // Get the remaining length
        if line_height != 0 {
            self.term.queue(cursor::MoveDown(line_height))?;
        }
        self.term.queue(cursor::MoveToColumn(line_remaining_len))?;

        Ok(())
    }

    pub fn get_curr_len(&self) -> u16 {
        (self.prompt.chars().count() as u16).saturating_add(self.line.chars().count() as u16)
    }

    pub fn render(&mut self) -> io::Result<()> {
        let line_len = self.get_curr_len();
        self.move_to_beginning(line_len)?;
        self.term.queue(style::ResetColor)?;
        write!(self.term, "{}{}", self.prompt, self.line)?;
        self.move_to_line_end()?;
        self.term.flush()
    }

    fn new_line(&mut self) -> io::Result<()> {
        writeln!(self.term)
    }

    fn insert_char(&mut self, c: char) {
        self.line.push(c);
    }

    fn remove_char(&mut self) {
        self.line.pop();
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

    fn history_back(&mut self) {
        let prev_entries_len = self.history.prev_entries.len();
        if self.history.curr_index == 0 || prev_entries_len == 0 {
            return;
        }
        if !self.history.prev_entries.contains(&self.line)
            && self.history.curr_index == prev_entries_len
        {
            self.history.temp_top = std::mem::take(&mut self.line);
        }
        self.history.curr_index -= 1;
        self.line = self.history.prev_entries[self.history.curr_index].clone();
    }

    fn history_forward(&mut self) {
        let prev_entries_len = self.history.prev_entries.len();
        if self.history.curr_index == prev_entries_len {
            return;
        }
        if self.history.curr_index == prev_entries_len - 1 {
            self.history.curr_index = prev_entries_len;
            self.line = std::mem::take(&mut self.history.temp_top);
            return;
        }
        self.history.curr_index += 1;
        self.line = self.history.prev_entries[self.history.curr_index].clone();
    }

    pub fn process_input_event(&mut self, event: Event) -> io::Result<EventLoop> {
        match event {
            Event::Key(KeyEvent {
                code: KeyCode::Char('c'),
                kind: KeyEventKind::Press,
                modifiers: KeyModifiers::CONTROL,
                ..
            }) => {
                if self.line.is_empty() {
                    return Ok(EventLoop::Break);
                }
                self.line.clear();
                Ok(EventLoop::Continue)
            }
            Event::Key(KeyEvent {
                code: KeyCode::Esc,
                kind: KeyEventKind::Press,
                ..
            }) => Ok(EventLoop::Break),
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
                self.remove_char();
                Ok(EventLoop::Continue)
            }
            Event::Key(KeyEvent {
                code: KeyCode::Up,
                kind: KeyEventKind::Press,
                ..
            }) => {
                self.history_back();
                Ok(EventLoop::Continue)
            }
            Event::Key(KeyEvent {
                code: KeyCode::Down,
                kind: KeyEventKind::Press,
                ..
            }) => {
                self.history_forward();
                Ok(EventLoop::Continue)
            }
            Event::Key(KeyEvent {
                code: KeyCode::Enter,
                kind: KeyEventKind::Press,
                modifiers,
                ..
            }) if modifiers.is_empty() => {
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
