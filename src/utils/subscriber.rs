use std::path::Path;

use tracing_appender::rolling::InitError;
use tracing_subscriber::{
    Layer, filter::EnvFilter, fmt, layer::SubscriberExt, util::SubscriberInitExt,
};

#[cfg(debug_assertions)]
pub use debug::init_subscriber;
#[cfg(not(debug_assertions))]
pub use release::init_subscriber;

#[cfg(not(debug_assertions))]
mod release {
    use super::*;
    use crate::{CRATE_NAME, LOG_ONLY, main_thread_state};

    use constcat::concat;
    use repl_oxide::ansi_code::{BLUE, CLEAR_LINE, GREEN, MAGENTA, RED, RESET, YELLOW};
    use tracing::{Event, Level, Subscriber};
    use tracing_appender::rolling::RollingFileAppender;
    use tracing_subscriber::{
        filter::FilterFn,
        fmt::{
            FmtContext,
            format::{FormatEvent, FormatFields, PrettyFields, Writer},
        },
        registry::LookupSpan,
    };

    const LOG_NAME: &str = concat!(CRATE_NAME, ".log");

    struct PanicFormatter<E> {
        inner: E,
    }

    impl<E> PanicFormatter<E> {
        fn new(inner: E) -> Self {
            Self { inner }
        }
    }

    impl<S, N, E> FormatEvent<S, N> for PanicFormatter<E>
    where
        S: Subscriber + for<'a> LookupSpan<'a>,
        N: for<'a> FormatFields<'a> + 'static,
        E: FormatEvent<S, N>,
    {
        fn format_event(
            &self,
            ctx: &FmtContext<'_, S, N>,
            mut writer: Writer<'_>,
            event: &Event<'_>,
        ) -> std::fmt::Result {
            let meta = event.metadata();

            if *meta.level() == Level::ERROR && meta.name() == "PANIC" {
                ctx.field_format().format_fields(writer.by_ref(), event)?;
                writeln!(writer)
            } else {
                self.inner.format_event(ctx, writer.by_ref(), event)
            }
        }
    }

    struct ColoredFormatter<E> {
        inner: E,
    }

    impl<E> ColoredFormatter<E> {
        fn new(inner: E) -> Self {
            Self { inner }
        }
    }

    impl<S, N, E> FormatEvent<S, N> for ColoredFormatter<E>
    where
        S: Subscriber + for<'a> LookupSpan<'a>,
        N: for<'a> FormatFields<'a> + 'static,
        E: FormatEvent<S, N>,
    {
        fn format_event(
            &self,
            ctx: &FmtContext<'_, S, N>,
            writer: Writer<'_>,
            event: &Event<'_>,
        ) -> std::fmt::Result {
            let line_color = match *event.metadata().level() {
                Level::ERROR => RED,
                Level::WARN => YELLOW,
                Level::INFO => GREEN,
                Level::DEBUG => BLUE,
                Level::TRACE => MAGENTA,
            };

            let print = |mut writer: Writer<'_>| {
                write!(writer, "{CLEAR_LINE}{line_color}")?;
                self.inner.format_event(ctx, writer.by_ref(), event)?;
                write!(writer, "{RESET}")
            };

            if main_thread_state::AltScreen::is_visible() {
                main_thread_state::AltScreen::push_formatter(print)
            } else {
                print(writer)
            }
        }
    }

    pub fn init_subscriber(local_env_dir: &Path) -> Result<(), InitError> {
        let file_appender = RollingFileAppender::builder()
            .filename_prefix(LOG_NAME)
            .max_log_files(5)
            .build(local_env_dir)?;

        let log_layer = fmt::layer()
            .event_format(PanicFormatter::new(
                fmt::format().with_target(false).with_ansi(false),
            ))
            .fmt_fields(PrettyFields::new())
            .with_writer(file_appender)
            .with_filter(EnvFilter::new(concat!(CRATE_NAME, "=info,reqwest=warn")));

        let exclude_log_only = FilterFn::new(|metadata| metadata.name() != LOG_ONLY);

        let stdout_layer = fmt::layer()
            .event_format(ColoredFormatter::new(
                fmt::format()
                    .with_target(false)
                    .without_time()
                    .with_level(false),
            ))
            .with_writer(std::io::stdout)
            .with_filter(EnvFilter::new(concat!(CRATE_NAME, "=info")))
            .with_filter(exclude_log_only);

        tracing_subscriber::registry()
            .with(log_layer)
            .with(stdout_layer)
            .init();

        Ok(())
    }
}

#[cfg(debug_assertions)]
mod debug {
    use super::*;
    use tracing_subscriber::filter::LevelFilter;

    pub fn init_subscriber(_local_env_dir: &Path) -> Result<(), InitError> {
        tracing_subscriber::registry()
            .with(
                fmt::layer().with_target(false).pretty().with_filter(
                    EnvFilter::builder()
                        .with_default_directive(LevelFilter::INFO.into())
                        .from_env_lossy(),
                ),
            )
            .init();

        Ok(())
    }
}
