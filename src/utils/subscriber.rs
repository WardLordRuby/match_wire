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
struct PanicFormatter<E> {
    inner: E,
}

#[cfg(not(debug_assertions))]
impl<E> PanicFormatter<E> {
    fn new(inner: E) -> Self {
        Self { inner }
    }
}

#[cfg(not(debug_assertions))]
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
        if meta.level() == &Level::ERROR && meta.name() == "PANIC" {
            ctx.field_format().format_fields(writer.by_ref(), event)?;
            writeln!(writer)
        } else {
            self.inner.format_event(ctx, writer.by_ref(), event)
        }
    }
}

#[cfg(not(debug_assertions))]
struct ColoredFormatter<E> {
    inner: E,
}

#[cfg(not(debug_assertions))]
impl<E> ColoredFormatter<E> {
    fn new(inner: E) -> Self {
        Self { inner }
    }
}

#[cfg(not(debug_assertions))]
impl<S, N, E> FormatEvent<S, N> for ColoredFormatter<E>
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
        use crate::utils::input::style::{BLUE, GREEN, MAGENTA, RED, WHITE, YELLOW};

        let meta = event.metadata();

        write!(
            writer,
            "{}",
            match *meta.level() {
                Level::ERROR => RED,
                Level::WARN => YELLOW,
                Level::INFO => GREEN,
                Level::DEBUG => BLUE,
                Level::TRACE => MAGENTA,
            }
        )?;

        self.inner.format_event(ctx, writer.by_ref(), event)?;

        write!(writer, "{WHITE}")
    }
}

#[cfg(not(debug_assertions))]
pub fn init_subscriber(local_env_dir: &std::path::Path) -> std::io::Result<()> {
    use tracing_subscriber::{filter::DynFilterFn, Layer};
    let name = env!("CARGO_PKG_NAME");
    let log_name = format!("{name}.log");

    let file_appender = tracing_appender::rolling::RollingFileAppender::builder()
        .filename_prefix(log_name)
        .max_log_files(5)
        .build(local_env_dir)
        .map_err(std::io::Error::other)?;

    let log_layer = fmt::layer()
        .event_format(PanicFormatter::new(
            fmt::format().with_target(false).with_ansi(false),
        ))
        .fmt_fields(PrettyFields::new())
        .with_writer(file_appender)
        .with_filter(EnvFilter::new(format!("{name}=info,reqwest=warn")));

    let exclude_log_only = DynFilterFn::new(|metadata, _| metadata.name() != crate::LOG_ONLY);

    let stdout_layer = fmt::layer()
        .event_format(ColoredFormatter::new(
            fmt::format()
                .with_target(false)
                .without_time()
                .with_level(false),
        ))
        .with_writer(std::io::stdout)
        .with_filter(EnvFilter::new(format!("{name}=info")))
        .with_filter(exclude_log_only);

    tracing_subscriber::registry()
        .with(log_layer)
        .with(stdout_layer)
        .init();

    Ok(())
}

#[cfg(debug_assertions)]
pub fn init_subscriber(_local_env_dir: &std::path::Path) -> std::io::Result<()> {
    use tracing_subscriber::{filter::LevelFilter, Layer};

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
