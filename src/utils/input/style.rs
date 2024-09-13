use crate::utils::input::line::LineData;
use crossterm::style::{Color, Stylize};
use std::fmt::Display;

pub const PROMPT_END: &str = "> ";
const YELLOW: &str = "\x1b[0;33m";
const BLUE: &str = "\x1b[38;5;38m";
const GREY: &str = "\x1b[38;5;238m";
const WHITE: &str = "\x1b[0m";

// MARK: TODO
// 1. Add error checks for
//   - arg before command
//   - arg with no value

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
    err: bool,
}

impl FormatState {
    fn new() -> Self {
        FormatState {
            curr_color: TextColor::Yellow,
            output: String::from(TextColor::Yellow.to_str()),
            white_space_start: 0,
            open_quote: None,
            err: false,
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
    }

    #[inline]
    fn close_quote(&mut self) {
        self.open_quote = None;
        self.set_color(TextColor::White);
    }
}

impl Display for LineData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (line_out, err) = stylize_input(self.input());
        write!(
            f,
            "{}{}{}",
            self.prompt().bold(),
            PROMPT_END
                .bold()
                .stylize()
                .with(if err { Color::Red } else { Color::Reset }),
            line_out
        )
    }
}

fn stylize_input(input: &str) -> (String, bool) {
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
        if let TextColor::Blue = ctx.curr_color {
            ctx.err = true;
        }
        ctx.set_color(TextColor::White);
    }
    (ctx.output, ctx.err)
}
