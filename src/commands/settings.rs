use crate::{
    LOG_ONLY,
    commands::{
        filter::{RegionContainer, continent},
        handler::{CommandContext, CommandHandle, ReplHandle},
    },
    utils::{
        caching::ContCode,
        display::{self, BoxBottom, BoxTop},
        main_thread_state,
    },
};

use std::{
    fmt::Display,
    io::{self, ErrorKind, Stdout, Write},
    path::Path,
};

use constcat::concat;
use crossterm::{
    QueueableCommand, cursor,
    event::{Event, KeyCode, KeyEvent},
    execute, queue,
    style::Print,
    terminal::{
        BeginSynchronizedUpdate, Clear,
        ClearType::{All, UntilNewLine},
        EndSynchronizedUpdate, EnterAlternateScreen, LeaveAlternateScreen,
    },
};
use repl_oxide::{
    ansi_code::{RESET, YELLOW},
    input_hook::{HookStates, HookedEvent, InputHook},
};
use serde::{Deserialize, Deserializer, Serialize};
use tracing::{error, info, warn};

const SETTINGS: &str = "settings.json";
pub(crate) const RETRIES_MAX: u8 = 10;

const TERM_SIZE_MIN: (u16, u16) = (
    SETTINGS_TEXT[0].len() as u16 + TOOLTIP_WIDTH as u16 + 2,
    SETTINGS_TEXT.len() as u16 + SETTING_ENTRIES.len() as u16 + 4,
);
const TOOLTIP_WIDTH: usize = 70;
const TOOLTIP_R: u16 = TOOLTIP_WIDTH as u16 + SETTINGS_TEXT[0].len() as u16 - 1;

// font: Ogre - patorjk.com
const SETTINGS_TEXT: [&str; 6] = [
    r#" __      _   _   _                 "#,
    r#"/ _\ ___| |_| |_(_)_ __   __ _ ___ "#,
    r#"\ \ / _ \ __| __| | '_ \ / _` / __|"#,
    r#"_\ \  __/ |_| |_| | | | | (_| \__ \"#,
    r#"\__/\___|\__|\__|_|_| |_|\__, |___/"#,
    r#"                         |___/     "#,
];

const KEYS: [&str; 3] = [
    "(up) / (w): Move cursor up | (down) / (s): Move cursor down",
    "(left) / (a): Decrement | (right) / (d): Increment",
    "(backspace) / (q): Save & leave",
];

const REGION_TIP: [&str; 2] = [
    "Whether or not this region is included in filter requests by",
    "default.",
];
const SETTING_ENTRIES: &[Entry] = &[
    Entry {
        field: Field::LaunchOnStartup,
        tip: [
            "Whether or not the game client launches when you launch MatchWire.",
            "",
        ],
        kind: Kind::Bool,
    },
    Entry {
        field: Field::ServerRetires,
        tip: [
            "The default number of retires that should be made for servers that",
            "fail to respond.",
        ],
        kind: Kind::U8,
    },
    Entry {
        field: Field::Na,
        tip: REGION_TIP,
        kind: Kind::SubSetBool,
    },
    Entry {
        field: Field::Sa,
        tip: REGION_TIP,
        kind: Kind::SubSetBool,
    },
    Entry {
        field: Field::Eu,
        tip: REGION_TIP,
        kind: Kind::SubSetBool,
    },
    Entry {
        field: Field::Apac,
        tip: REGION_TIP,
        kind: Kind::SubSetBool,
    },
];

const _: () = assert!(
    std::mem::size_of::<Settings>() <= std::mem::size_of::<&Settings>(),
    "functions should now take `Settings` by ref"
);

#[derive(Deserialize, Serialize, Debug, Clone, Copy)]
pub struct Settings {
    #[serde(deserialize_with = "retries_deserializer")]
    pub server_retires: u8,
    pub launch_on_startup: bool,
    #[serde(deserialize_with = "regions_deserializer")]
    default_regions: SelectedRegions,
}

fn retries_deserializer<'de, D>(deserializer: D) -> Result<u8, D::Error>
where
    D: Deserializer<'de>,
{
    let retries = i16::deserialize(deserializer)?;

    if retries.is_negative() {
        return Ok(0);
    } else if retries > RETRIES_MAX as i16 {
        return Ok(RETRIES_MAX);
    }

    Ok(retries as u8)
}

fn regions_deserializer<'de, D>(deserializer: D) -> Result<SelectedRegions, D::Error>
where
    D: Deserializer<'de>,
{
    let regions = SelectedRegions::deserialize(deserializer)?;

    if regions.is_none() {
        return Ok(SelectedRegions::default());
    }

    Ok(regions)
}

#[derive(Deserialize, Serialize, Debug, Clone, Copy, PartialEq, Eq)]
struct SelectedRegions {
    north_america: bool,
    south_america: bool,
    europe: bool,
    asia_pacific: bool,
}

impl SelectedRegions {
    const fn default() -> Self {
        Self {
            north_america: true,
            south_america: true,
            europe: true,
            asia_pacific: true,
        }
    }

    #[inline]
    fn all(self) -> bool {
        self == Self::default()
    }

    #[inline]
    fn is_none(self) -> bool {
        self == Self {
            north_america: false,
            south_america: false,
            europe: false,
            asia_pacific: false,
        }
    }

    fn flatten(self) -> [(bool, &'static [ContCode]); continent::COUNT] {
        [
            (self.north_america, &continent::NA_CODE),
            (self.south_america, &continent::SA_CODE),
            (self.europe, &continent::EU_CODES),
            (self.asia_pacific, &continent::APAC_CODES),
        ]
    }
}

impl Settings {
    const fn default() -> Self {
        Self {
            server_retires: 3,
            launch_on_startup: true,
            default_regions: SelectedRegions::default(),
        }
    }

    pub fn init(local_env_dir: Option<&Path>) -> Self {
        let settings = if let Some(settings_path) = local_env_dir.map(|dir| dir.join(SETTINGS)) {
            let mut not_found = false;
            let res = match std::fs::read(&settings_path) {
                Ok(data) => serde_json::from_slice::<Self>(&data).map_err(display::error),
                Err(err) => {
                    if err.kind() == ErrorKind::NotFound {
                        not_found = true;
                        error!(name: LOG_ONLY, "{}", concat!(SETTINGS, " not found"))
                    } else {
                        error!("Failed to read {SETTINGS}");
                        error!(name: LOG_ONLY, "{err}, reading file: {}", settings_path.display())
                    };
                    Err(())
                }
            };

            if res.is_err() {
                if let Err(err) = Self::write(&mut Self::default(), &settings_path) {
                    error!(name: LOG_ONLY, "{err}, failed to write file: {}", settings_path.display())
                } else if not_found {
                    info!(name: LOG_ONLY, "Settings file created at: {}", settings_path.display())
                } else {
                    info!("New {SETTINGS} created")
                }
            } else {
                info!(name: LOG_ONLY, "Settings loaded!")
            }

            res.ok()
        } else {
            warn!("Used default app settings");
            None
        };

        settings.unwrap_or(Self::default())
    }

    pub(crate) fn regions(self) -> Option<RegionContainer> {
        let selected_regions = self.default_regions;

        if selected_regions.all() {
            return None;
        }

        Some(RegionContainer::Vec(
            selected_regions
                .flatten()
                .into_iter()
                .filter(|(region_selected, _)| *region_selected)
                .flat_map(|(_, codes)| codes)
                .copied()
                .collect(),
        ))
    }

    fn write(&mut self, path: &Path) -> io::Result<()> {
        let file = std::fs::File::create(path)?;

        if self.default_regions.is_none() {
            self.default_regions = SelectedRegions::default();
        }

        serde_json::to_writer_pretty(file, &self).expect("`Self` is always ok to serialize");
        info!(name: LOG_ONLY, "{SETTINGS} saved!");
        Ok(())
    }

    fn write_field(&self, field: Field, term: &mut Stdout, selected: bool) -> io::Result<()> {
        const HIGHLIGHTED: &str = "\x1b[7m";
        let sel = if selected { HIGHLIGHTED } else { "" };

        write!(term, "{field}: {sel}")?;
        match field {
            Field::LaunchOnStartup => write!(term, "{}{RESET} ", self.launch_on_startup),
            Field::ServerRetires => write!(term, "{}{RESET} ", self.server_retires),
            Field::Na => write!(term, "{}{RESET} ", self.default_regions.north_america),
            Field::Sa => write!(term, "{}{RESET} ", self.default_regions.south_america),
            Field::Eu => write!(term, "{}{RESET} ", self.default_regions.europe),
            Field::Apac => write!(term, "{}{RESET} ", self.default_regions.asia_pacific),
        }
    }

    fn increment(&mut self, setting: Field) {
        match setting {
            Field::ServerRetires if self.server_retires == RETRIES_MAX => self.server_retires = 0,
            Field::ServerRetires => self.server_retires += 1,
            Field::LaunchOnStartup | Field::Na | Field::Sa | Field::Eu | Field::Apac => {
                panic!("Use `Self::flip`")
            }
        }
    }

    fn decrement(&mut self, setting: Field) {
        match setting {
            Field::ServerRetires if self.server_retires == 0 => self.server_retires = RETRIES_MAX,
            Field::ServerRetires => self.server_retires -= 1,
            Field::LaunchOnStartup | Field::Na | Field::Sa | Field::Eu | Field::Apac => {
                panic!("Use `Self::flip`")
            }
        }
    }

    fn flip(&mut self, setting: Field) {
        match setting {
            Field::LaunchOnStartup => self.launch_on_startup = !self.launch_on_startup,
            Field::Na => self.default_regions.north_america = !self.default_regions.north_america,
            Field::Sa => self.default_regions.south_america = !self.default_regions.south_america,
            Field::Eu => self.default_regions.europe = !self.default_regions.europe,
            Field::Apac => self.default_regions.asia_pacific = !self.default_regions.asia_pacific,
            Field::ServerRetires => panic!("Use `Self::increment` | `Self::decrement`"),
        }
    }
}

pub(crate) struct SettingsRenderer {
    selected: usize,
    entries: &'static [Entry],
    entry_loc: Vec<(u16, u16)>,
    tip_loc: (u16, u16),
}

struct Entry {
    field: Field,
    tip: [&'static str; 2],
    kind: Kind,
}

#[derive(Clone, Copy)]
enum Kind {
    Bool,
    SubSetBool,
    U8,
}

impl Kind {
    fn is_bool(self) -> bool {
        matches!(self, Self::Bool | Self::SubSetBool)
    }
}

#[derive(Clone, Copy)]
pub(crate) enum Field {
    LaunchOnStartup,
    ServerRetires,
    Na,
    Sa,
    Eu,
    Apac,
}

impl Display for Field {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Field::LaunchOnStartup => "Launch on startup",
                Field::ServerRetires => "Server retries",
                Field::Na => "North America",
                Field::Sa => "South America",
                Field::Eu => "Europe",
                Field::Apac => "Asia Pacific",
            }
        )
    }
}

impl SettingsRenderer {
    pub(crate) const fn new() -> Self {
        Self {
            selected: 0,
            entries: SETTING_ENTRIES,
            entry_loc: Vec::new(),
            tip_loc: (
                SETTINGS_TEXT[0].len() as u16 - 2,
                SETTINGS_TEXT.len() as u16 + 1,
            ),
        }
    }

    /// Will panic if called before `Self` has been drawn
    #[inline]
    fn selected(&self) -> (&Entry, (u16, u16)) {
        (&self.entries[self.selected], self.entry_loc[self.selected])
    }

    fn draw_change(
        disp: &mut Self,
        settings: Settings,
        term: &mut Stdout,
        bounds: usize,
        f: impl Fn(&mut usize),
    ) -> io::Result<()> {
        if disp.selected == bounds {
            return Ok(());
        }

        let mut selected = disp.selected();

        for state in [false, true] {
            term.queue(cursor::MoveTo(selected.1.0, selected.1.1))?;
            settings.write_field(selected.0.field, term, state)?;

            if state {
                break;
            }

            f(&mut disp.selected);
            selected = disp.selected();
        }

        for (i, tip) in selected.0.tip.into_iter().enumerate() {
            term.queue(cursor::MoveTo(disp.tip_loc.0, disp.tip_loc.1 + i as u16))?;
            write_table_content(term, tip)?;
        }

        Ok(())
    }

    #[inline]
    fn move_up(&mut self, settings: Settings, term: &mut Stdout) -> io::Result<()> {
        Self::draw_change(self, settings, term, 0, |i| *i -= 1)
    }

    #[inline]
    fn move_down(&mut self, settings: Settings, term: &mut Stdout) -> io::Result<()> {
        Self::draw_change(self, settings, term, self.entries.len() - 1, |i| *i += 1)
    }

    fn draw(&mut self, term: &mut Stdout, settings: Settings) -> io::Result<()> {
        let (mut curr_col, mut curr_row) = (2, 0);

        for line in SETTINGS_TEXT {
            term.queue(cursor::MoveTo(curr_col, curr_row))?
                .queue(Print(line))?;
            curr_row += 1;
        }

        let mut found_regions = false;
        self.entry_loc.clear();

        for (i, entry) in self.entries.iter().enumerate() {
            if !found_regions && matches!(entry.kind, Kind::SubSetBool) {
                found_regions = true;

                term.queue(cursor::MoveTo(curr_col, curr_row + 1))?
                    .queue(Print("Default regions:"))?;

                curr_col += 2;
                curr_row += 2;
            }

            term.queue(cursor::MoveTo(curr_col, curr_row))?;
            self.entry_loc.push((curr_col, curr_row));
            settings.write_field(entry.field, term, self.selected == i)?;

            curr_row += 1;
        }

        curr_col = self.tip_loc.0;
        curr_row = self.tip_loc.1;

        term.queue(cursor::MoveTo(curr_col, curr_row - 1))?
            .queue(Print(BoxTop(Some("ToolTip"), TOOLTIP_WIDTH)))?;

        for line in self.entries[self.selected].tip {
            term.queue(cursor::MoveTo(curr_col, curr_row))?;
            write_table_content(term, line)?;
            curr_row += 1;
        }

        term.queue(cursor::MoveTo(curr_col, curr_row))?
            .queue(Print(BoxBottom(TOOLTIP_WIDTH)))?;
        curr_row += 1;

        term.queue(cursor::MoveTo(curr_col, curr_row))?
            .queue(Print(BoxTop(Some("Keys"), TOOLTIP_WIDTH)))?;
        curr_row += 1;

        for key_tip in KEYS {
            term.queue(cursor::MoveTo(curr_col, curr_row))?;
            write_table_content(term, key_tip)?;
            curr_row += 1;
        }

        term.queue(cursor::MoveTo(curr_col, curr_row))?
            .queue(Print(BoxBottom(TOOLTIP_WIDTH)))?;

        Ok(())
    }
}

#[inline(always)]
fn write_table_content(term: &mut Stdout, content: &str) -> io::Result<()> {
    queue!(
        term,
        Print(format_args!("│ {content}")),
        Clear(UntilNewLine),
        cursor::MoveToColumn(TOOLTIP_R),
        Print('│')
    )
}

impl CommandContext {
    fn try_write_settings(&mut self) -> io::Result<()> {
        let local_dir = self.local_dir().ok_or_else(|| {
            io::Error::new(
                ErrorKind::NotFound,
                format!("No valid directory to write {SETTINGS}"),
            )
        })?;

        self.settings.write(&local_dir.join(SETTINGS))
    }

    pub(crate) fn settings(
        &mut self,
        handle: &ReplHandle,
        use_default: bool,
    ) -> io::Result<CommandHandle> {
        if use_default {
            self.settings = Settings::default();
            let _ = self.try_write_settings().map_err(display::error);
            return Ok(CommandHandle::Processed);
        }

        let term_size = handle.terminal_size();
        if term_size.0 < TERM_SIZE_MIN.0 || term_size.1 < TERM_SIZE_MIN.1 {
            println!("{YELLOW}Terminal to small. Resize window to edit settings{RESET}");
            return Ok(CommandHandle::Processed);
        }

        let state_changes = HookStates::<CommandContext, Stdout>::new(
            |handle, context| {
                handle.disable_render();

                let term = handle.writer();
                execute!(term, BeginSynchronizedUpdate)?;

                main_thread_state::AltScreen::enter();
                queue!(term, EnterAlternateScreen, cursor::Hide, Clear(All))?;
                context.settings_disp.draw(term, context.settings)?;

                execute!(term, EndSynchronizedUpdate)?;
                Ok(())
            },
            |handle, _context| {
                execute!(handle.writer(), LeaveAlternateScreen, cursor::Show)?;

                main_thread_state::AltScreen::leave();
                handle.enable_render();
                Ok(())
            },
        );

        let input_hook = InputHook::with_new_uid(state_changes, |handle, context, event| {
            let term = handle.writer();

            match event {
                Event::Key(KeyEvent {
                    code: KeyCode::Backspace | KeyCode::Esc | KeyCode::Char('q'),
                    ..
                }) => {
                    let _ = context.try_write_settings().map_err(display::error);
                    return HookedEvent::release_hook();
                }
                Event::Key(KeyEvent {
                    code: KeyCode::Up | KeyCode::Char('w'),
                    ..
                }) => context.settings_disp.move_up(context.settings, term)?,
                Event::Key(KeyEvent {
                    code: KeyCode::Down | KeyCode::Char('s'),
                    ..
                }) => context.settings_disp.move_down(context.settings, term)?,
                Event::Key(
                    event @ KeyEvent {
                        code:
                            KeyCode::Left
                            | KeyCode::Char('a')
                            | KeyCode::Right
                            | KeyCode::Char('d')
                            | KeyCode::Enter,
                        ..
                    },
                ) => {
                    let (selected, pos) = context.settings_disp.selected();

                    if selected.kind.is_bool() {
                        context.settings.flip(selected.field);
                    } else if matches!(event.code, KeyCode::Left | KeyCode::Char('a')) {
                        context.settings.decrement(selected.field);
                    } else {
                        context.settings.increment(selected.field);
                    }

                    term.queue(cursor::MoveTo(pos.0, pos.1))?;
                    context.settings.write_field(selected.field, term, true)?;
                }
                _ => return HookedEvent::continue_hook(),
            }

            term.flush()?;
            HookedEvent::continue_hook()
        });

        Ok(CommandHandle::InsertHook(input_hook))
    }
}
