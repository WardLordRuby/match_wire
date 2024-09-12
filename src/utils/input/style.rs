use crate::utils::input::line::LineData;
use crossterm::style::{Color, Stylize};
use std::fmt::Display;

pub const PROMPT_END: &str = "> ";
const YELLOW: &str = "\x1b[0;33m";
const BLUE: &str = "\x1b[38;5;38m";
const GREY: &str = "\x1b[38;5;238m";
const WHITE: &str = "\x1b[0m";

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

// MARK: TODO
// 1. Add error checks for
//   - arg before command
//   - arg with no value

#[derive(Default)]
struct Ctx {
    curr: &'static str,
    open_quote: Option<char>,
    white_space_start: usize,
    output: String,
    err: bool,
}

impl Ctx {
    fn new() -> Self {
        Ctx {
            curr: YELLOW,
            output: String::from(YELLOW),
            ..Default::default()
        }
    }

    fn set_color(&mut self, color: &'static str) {
        self.curr = color;
        self.push(color);
    }

    #[inline]
    fn push(&mut self, str: &str) {
        self.output.push_str(str);
    }
}

fn stylize_input(input: &str) -> (String, bool) {
    let mut ctx = Ctx::new();

    for token in input.split_whitespace() {
        let i = input[ctx.white_space_start..]
            .find(token)
            .expect("already found");

        ctx.push(&input[ctx.white_space_start..ctx.white_space_start + i]);
        ctx.white_space_start += i + token.len();

        match ctx.curr {
            WHITE => {
                if token.starts_with('-') {
                    ctx.push(GREY);
                    ctx.push(token);
                    ctx.push(WHITE);
                } else if token.starts_with('\'') || token.starts_with('\"') {
                    ctx.set_color(BLUE);
                    ctx.push(token);
                    ctx.open_quote = Some(token.chars().next().expect("found above"));
                } else {
                    ctx.push(token);
                }
            }
            YELLOW => {
                ctx.push(token);
                ctx.set_color(WHITE);
            }
            BLUE => {
                ctx.push(token);
                if token.ends_with(ctx.open_quote.expect("color is blue")) {
                    ctx.open_quote = None;
                    ctx.set_color(WHITE);
                }
            }
            _ => unreachable!(),
        }
    }

    if ctx.curr != WHITE {
        if ctx.curr == BLUE {
            ctx.err = true;
        }
        ctx.set_color(WHITE);
    }
    (ctx.output, ctx.err)
}
