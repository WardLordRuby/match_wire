use super::{Repeat, Space};
use std::{
    io::{self, Write},
    num::NonZero,
    sync::mpsc::{self, TryRecvError},
    time::Duration,
};

use repl_oxide::ansi_code::{CLEAR_LINE, LIGHT_BLUE, RESET};
use tracing::error;

macro_rules! finish {
    ($indic:expr) => {{
        drop($indic.sender);
        match $indic.task.join() {
            Ok(Ok(())) => (),
            Ok(Err(err)) => error!(name: $crate::LOG_ONLY, "{err}"),
            Err(err) => error!(name: $crate::LOG_ONLY, "{err:?}"),
        }
    }};
}

const P_BAR_LEN: usize = 35;
const WRITE_INTERVAL: Duration = Duration::from_millis(60);

pub(crate) struct Spinner {
    task: std::thread::JoinHandle<io::Result<()>>,
    sender: mpsc::Sender<String>,
}

impl Spinner {
    pub(crate) fn new(mut message: String) -> Self {
        let (tx, rx) = mpsc::channel();

        let task = std::thread::spawn(move || {
            const SPINNER_CHARS: &str = "-\\|/";
            let mut stdout = std::io::stdout();
            for ch in SPINNER_CHARS.chars().cycle() {
                match rx.try_recv() {
                    Ok(new) => message = new,
                    Err(TryRecvError::Empty) => (),
                    Err(TryRecvError::Disconnected) => break,
                }
                write!(stdout, "{CLEAR_LINE}{ch} {message}")?;
                stdout.flush()?;
                std::thread::sleep(WRITE_INTERVAL);
            }

            write!(stdout, "{CLEAR_LINE}")
        });

        Self { task, sender: tx }
    }

    pub(crate) fn update_message(&self, message: String) {
        self.sender
            .send(message)
            .unwrap_or_else(|err| println!("{CLEAR_LINE}{}...", err.0))
    }

    pub(crate) fn finish(self) {
        finish!(self)
    }
}

pub(crate) struct ProgressBar {
    task: std::thread::JoinHandle<io::Result<()>>,
    sender: mpsc::Sender<()>,
}

impl ProgressBar {
    #[inline]
    fn tick_div(total: usize) -> f32 {
        total as f32 / P_BAR_LEN as f32
    }

    #[inline]
    fn ticks(count: usize, tick_div: f32) -> usize {
        (count as f32 / tick_div).round() as usize
    }

    pub(crate) fn new(label_a: &'static str, label_b: &'static str, total: NonZero<usize>) -> Self {
        let tick_div = Self::tick_div(usize::from(total));

        let (tx, rx) = mpsc::channel();

        let task = std::thread::spawn(move || {
            let mut count = 0;
            let mut stdout = std::io::stdout();
            'task: loop {
                let start = std::time::Instant::now();
                'update_ct: loop {
                    match rx.try_recv() {
                        Ok(()) => count += 1,
                        Err(TryRecvError::Empty) => {
                            if start.elapsed() > WRITE_INTERVAL {
                                break 'update_ct;
                            }
                            std::thread::sleep(Duration::from_millis(20));
                        }
                        Err(TryRecvError::Disconnected) => break 'task,
                    }
                }
                let ticks = Self::ticks(count, tick_div);

                write!(
                    stdout,
                    "{CLEAR_LINE}{label_a} [{LIGHT_BLUE}{}>{}{RESET}] {count}/{total} {label_b}",
                    Repeat('=', ticks),
                    Space(P_BAR_LEN - ticks)
                )?;
                stdout.flush()?;
            }

            write!(stdout, "{CLEAR_LINE}")
        });

        Self { task, sender: tx }
    }

    #[inline]
    pub(crate) fn tick(&self) {
        let _ = self.sender.send(());
    }

    pub(crate) fn finish(self) {
        finish!(self)
    }
}

#[cfg(test)]
mod test {
    use super::{P_BAR_LEN, ProgressBar};

    #[test]
    fn progress_bar_lens() {
        let half = P_BAR_LEN / 2;
        let half = half..=(half + 1);

        for total in 1..10_000 {
            for ct in 0..=total {
                let tick_div = ProgressBar::tick_div(total);
                let ticks = ProgressBar::ticks(ct, tick_div);

                if ct == 0 {
                    assert_eq!(ticks, 0)
                } else if (ct as f32 - total as f32 / 2.0).abs() < 0.5 {
                    assert!(half.contains(&ticks))
                } else if ct == total {
                    assert_eq!(ticks, P_BAR_LEN)
                }

                assert!(tick_div > 0.0);
                assert!(ticks <= P_BAR_LEN)
            }
        }
    }
}
