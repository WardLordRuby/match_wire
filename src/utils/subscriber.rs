use tracing_subscriber::{filter::EnvFilter, fmt, layer::SubscriberExt, util::SubscriberInitExt};

#[cfg(not(debug_assertions))]
use tracing::{Event, Level, Subscriber};

#[cfg(not(debug_assertions))]
use tracing_subscriber::{
    fmt::{
        format::{FormatEvent, FormatFields, PrettyFields, Writer},
        FmtContext,
    },
    registry::LookupSpan,
};

#[cfg(not(debug_assertions))]
struct CustomFormatter<E> {
    inner: E,
}

#[cfg(not(debug_assertions))]
impl<E> CustomFormatter<E> {
    fn new(inner: E) -> Self {
        Self { inner }
    }
}

#[cfg(not(debug_assertions))]
impl<S, N, E> FormatEvent<S, N> for CustomFormatter<E>
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
        if meta.level() == &Level::ERROR && meta.name() == "PANIC" {
            ctx.field_format().format_fields(writer.by_ref(), event)?;
            writeln!(writer)
        } else {
            self.inner.format_event(ctx, writer.by_ref(), event)
        }
    }
}

#[cfg(not(debug_assertions))]
pub fn init_subscriber(local_env_dir: &std::path::Path) -> std::io::Result<()> {
    use tracing_subscriber::{filter::DynFilterFn, Layer};

    let file_appender = tracing_appender::rolling::RollingFileAppender::builder()
        .filename_prefix(crate::LOG_NAME)
        .max_log_files(5)
        .build(local_env_dir)
        .map_err(std::io::Error::other)?;

    let file_layer = fmt::layer()
        .event_format(CustomFormatter::new(
            fmt::format().with_target(false).with_ansi(false),
        ))
        .fmt_fields(PrettyFields::new())
        .with_writer(file_appender)
        .with_filter(EnvFilter::new("h2m_favorites=info,reqwest=warn"));

    let exclude_log_only = DynFilterFn::new(|metadata, _| metadata.name() != crate::LOG_ONLY);

    // let custom_writer = || -> std::io::Stdout {
    //     use crossterm::{
    //         cursor, execute,
    //         terminal::{Clear, ClearType::CurrentLine},
    //     };
    //     let mut stdout = std::io::stdout();
    //     execute!(stdout, Clear(CurrentLine)).unwrap(); // cursor::MoveToColumn(0),

    //     stdout
    // };

    let stdout_layer = fmt::layer()
        .with_target(false)
        .without_time()
        .with_ansi(false)
        .with_level(false)
        .with_writer(std::io::stdout)
        .with_filter(EnvFilter::new("h2m_favorites=info"))
        .with_filter(exclude_log_only);

    tracing_subscriber::registry()
        .with(file_layer)
        .with(stdout_layer)
        .init();

    Ok(())
}

#[cfg(debug_assertions)]
pub fn init_subscriber(_local_env_dir: &std::path::Path) -> std::io::Result<()> {
    use tracing_subscriber::filter::LevelFilter;

    tracing_subscriber::registry()
        .with(fmt::layer().with_target(false).pretty())
        .with(
            EnvFilter::builder()
                .with_default_directive(LevelFilter::INFO.into())
                .from_env_lossy(),
        )
        .init();
    Ok(())
}
