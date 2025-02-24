use tracing_subscriber::{filter::EnvFilter, fmt, layer::SubscriberExt, util::SubscriberInitExt};

#[cfg(not(debug_assertions))]
use tracing::{Event, Level, Subscriber};

#[cfg(not(debug_assertions))]
use tracing_subscriber::{
    fmt::{
        FmtContext,
        format::{FormatEvent, FormatFields, PrettyFields, Writer},
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
fn print_during_splash<F>(print: F) -> std::fmt::Result
where
    F: FnOnce(Writer<'_>) -> std::fmt::Result,
{
    let mut buffer = String::new();
    print(Writer::new(&mut buffer))?;
    let mut msg_queue = crate::SPLASH_SCREEN_MSG_BUFFER
        .lock()
        .expect("no one will panic with lock & lock uncontested");
    msg_queue.push_str(&buffer);
    Ok(())
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
        writer: Writer<'_>,
        event: &Event<'_>,
    ) -> std::fmt::Result {
        use repl_oxide::ansi_code::{BLUE, GREEN, MAGENTA, RED, RESET, YELLOW};

        let line_color = match *event.metadata().level() {
            Level::ERROR => RED,
            Level::WARN => YELLOW,
            Level::INFO => GREEN,
            Level::DEBUG => BLUE,
            Level::TRACE => MAGENTA,
        };

        let print = |mut writer: Writer<'_>| {
            write!(writer, "{line_color}")?;
            self.inner.format_event(ctx, writer.by_ref(), event)?;
            write!(writer, "{RESET}")
        };

        if crate::SPLASH_SCREEN_VIS.load(std::sync::atomic::Ordering::Acquire) {
            return print_during_splash(print);
        }
        print(writer)
    }
}

#[cfg(not(debug_assertions))]
pub fn init_subscriber(local_env_dir: &std::path::Path) -> std::io::Result<()> {
    use constcat::concat;
    use tracing_subscriber::{Layer, filter::DynFilterFn};

    const NAME: &str = env!("CARGO_PKG_NAME");
    const LOG_NAME: &str = concat!(NAME, ".log");

    let file_appender = tracing_appender::rolling::RollingFileAppender::builder()
        .filename_prefix(LOG_NAME)
        .max_log_files(5)
        .build(local_env_dir)
        .map_err(std::io::Error::other)?;

    let log_layer = fmt::layer()
        .event_format(PanicFormatter::new(
            fmt::format().with_target(false).with_ansi(false),
        ))
        .fmt_fields(PrettyFields::new())
        .with_writer(file_appender)
        .with_filter(EnvFilter::new(concat!(NAME, "=info,reqwest=warn")));

    let exclude_log_only = DynFilterFn::new(|metadata, _| metadata.name() != crate::LOG_ONLY);

    let stdout_layer = fmt::layer()
        .event_format(ColoredFormatter::new(
            fmt::format()
                .with_target(false)
                .without_time()
                .with_level(false),
        ))
        .with_writer(std::io::stdout)
        .with_filter(EnvFilter::new(concat!(NAME, "=info")))
        .with_filter(exclude_log_only);

    tracing_subscriber::registry()
        .with(log_layer)
        .with(stdout_layer)
        .init();

    Ok(())
}

#[cfg(debug_assertions)]
pub fn init_subscriber(_local_env_dir: &std::path::Path) -> std::io::Result<()> {
    use tracing_subscriber::{Layer, filter::LevelFilter};

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
