use crate::utils::input::line::LineData;
use crossterm::style::{Color, Stylize};
use std::fmt::Display;

pub const PROMPT_END: &str = "> ";
pub const RED: &str = "\x1b[31m";
pub const YELLOW: &str = "\x1b[38;5;220m";
pub const GREEN: &str = "\x1b[92m";
pub const BLUE: &str = "\x1b[38;5;38m";
pub const MAGENTA: &str = "\x1b[35m";
pub const GREY: &str = "\x1b[38;5;238m";
pub const WHITE: &str = "\x1b[0m";

enum TextColor {
    Yellow,
    Blue,
    Grey,
    White,
}

impl TextColor {
    fn to_str(&self) -> &'static str {
        match self {
            TextColor::Yellow => YELLOW,
            TextColor::Blue => BLUE,
            TextColor::Grey => GREY,
            TextColor::White => WHITE,
        }
    }
}

struct FormatState {
    curr_color: TextColor,
    open_quote: Option<char>,
    white_space_start: usize,
    output: String,
}

impl FormatState {
    fn new() -> Self {
        FormatState {
            curr_color: TextColor::Yellow,
            output: String::from(TextColor::Yellow.to_str()),
            white_space_start: 0,
            open_quote: None,
        }
    }

    #[inline]
    fn set_color(&mut self, color: TextColor) {
        self.push(color.to_str());
        self.curr_color = color;
    }

    #[inline]
    fn color_token(&mut self, str: &str, color: TextColor) {
        self.push(color.to_str());
        self.push(str);
        self.push(self.curr_color.to_str());
    }

    #[inline]
    fn push(&mut self, str: &str) {
        self.output.push_str(str);
    }

    #[inline]
    fn open_quote(&mut self, str: &str, quote: Option<char>) {
        assert!(quote.is_some());
        self.set_color(TextColor::Blue);
        self.push(str);
        self.open_quote = quote;
        if str.chars().nth(1).is_some() && str.ends_with(quote.expect("color is blue")) {
            self.close_quote();
        }
    }

    #[inline]
    fn close_quote(&mut self) {
        self.open_quote = None;
        self.set_color(TextColor::White);
    }
}

impl Display for LineData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{WHITE}{}{}{}",
            self.prompt().bold(),
            self.prompt_separator()
                .bold()
                .stylize()
                .with(if self.completion_enabled() {
                    if self.err() {
                        Color::Red
                    } else {
                        Color::Reset
                    }
                } else {
                    Color::Reset
                }),
            if self.completion_enabled() {
                stylize_input(self.input())
            } else {
                self.input().to_string()
            }
        )
    }
}

fn stylize_input(input: &str) -> String {
    let mut ctx = FormatState::new();

    for token in input.split_whitespace() {
        let i = input[ctx.white_space_start..]
            .find(token)
            .expect("already found");

        ctx.push(&input[ctx.white_space_start..ctx.white_space_start + i]);
        ctx.white_space_start += i + token.len();

        match ctx.curr_color {
            TextColor::White => {
                if token.starts_with('-') {
                    ctx.color_token(token, TextColor::Grey);
                } else if token.starts_with('\'') || token.starts_with('\"') {
                    ctx.open_quote(token, token.chars().next());
                } else {
                    ctx.push(token);
                }
            }
            TextColor::Yellow => {
                ctx.push(token);
                ctx.set_color(TextColor::White);
            }
            TextColor::Blue => {
                ctx.push(token);
                if token.ends_with(ctx.open_quote.expect("color is blue")) {
                    ctx.close_quote();
                }
            }
            TextColor::Grey => unreachable!(),
        }
    }

    if !matches!(ctx.curr_color, TextColor::White) {
        ctx.set_color(TextColor::White);
    }
    ctx.output
}
