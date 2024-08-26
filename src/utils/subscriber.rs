use tracing_subscriber::{fmt, layer::SubscriberExt, util::SubscriberInitExt};

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
    let file_appender = tracing_appender::rolling::RollingFileAppender::builder()
        .filename_prefix(crate::LOG_NAME)
        .max_log_files(5)
        .build(local_env_dir)
        .map_err(std::io::Error::other)?;

    tracing_subscriber::registry()
        .with(
            fmt::layer()
                .event_format(CustomFormatter::new(
                    fmt::format()
                        .with_target(false)
                        .with_ansi(false)
                        .without_time(),
                ))
                // MARK: TODO
                // Need to set only info level and higher
                .fmt_fields(PrettyFields::new())
                .with_writer(file_appender),
        )
        .init();
    Ok(())
}

#[cfg(debug_assertions)]
pub fn init_subscriber(_local_env_dir: &std::path::Path) -> std::io::Result<()> {
    use tracing_subscriber::filter::{EnvFilter, LevelFilter};

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
