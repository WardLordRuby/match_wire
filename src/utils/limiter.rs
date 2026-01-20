use std::{
    marker::PhantomData,
    num::NonZero,
    time::{Duration, Instant},
};

#[derive(Clone, Copy)]
pub struct RateLimiter<T> {
    ct: usize,
    start: Option<Instant>,
    limit: usize,
    interval: Duration,
    _marker: PhantomData<T>,
}

pub trait RateLimitConfig {
    const LIMIT: NonZero<usize>;
    const INTERVAL: NonZeroDuration;
    const DISP_NAME: &str;
}

#[derive(Clone, Copy)]
pub struct NonZeroDuration(Duration);

impl NonZeroDuration {
    pub const fn new(duration: Duration) -> Option<Self> {
        if duration.is_zero() { None } else { Some(Self(duration)) }
    }

    pub const fn get(&self) -> Duration {
        self.0
    }
}

/// `$ident` is the struct and display name of the generated `RateLimitConfig`\
/// `($ident:ident, $limit:usize, $interval:Duration)`
///
/// ```compile_fail
/// # use match_wire::impl_rate_limit_config;
/// # use std::time::Duration;
/// // Limit must be non-zero
/// impl_rate_limit_config!(LimitZero, 0, Duration::from_secs(60)); // doesn't compile!
/// ```
///
/// ```compile_fail
/// # use match_wire::impl_rate_limit_config;
/// # use std::time::Duration;
/// // Interval must be non-zero
/// impl_rate_limit_config!(IntervalZero, 60, Duration::from_secs(0)); // doesn't compile!
/// ```
#[macro_export]
macro_rules! impl_rate_limit_config {
    ($ident:ident, $limit:expr, $interval:expr) => {
        #[derive(Clone, Copy)]
        pub(crate) struct $ident;

        impl $crate::utils::limiter::RateLimitConfig for $ident {
            const LIMIT: std::num::NonZero<usize> = std::num::NonZero::new($limit).unwrap();
            const INTERVAL: $crate::utils::limiter::NonZeroDuration =
                $crate::utils::limiter::NonZeroDuration::new($interval).unwrap();
            const DISP_NAME: &str = stringify!($ident);
        }

        const _: () = assert!(!$interval.is_zero(), "Interval must be non-zero");
        const _: () = assert!($limit != 0, "Limit must be non-zero");
    };
}

impl<T: RateLimitConfig> RateLimiter<T> {
    #[expect(clippy::new_without_default, reason = "`Self` will only be const")]
    pub const fn new() -> Self {
        Self {
            ct: 0,
            start: None,
            limit: T::LIMIT.get(),
            interval: T::INTERVAL.get(),
            _marker: PhantomData,
        }
    }

    #[inline]
    fn elapsed(&self) -> Option<Duration> {
        self.start.map(|start| start.elapsed())
    }

    pub fn remainder(&self) -> Option<Duration> {
        self.start
            .and_then(|start| self.interval.checked_sub(start.elapsed()))
    }

    /// This method is used to advance the internal state and return if it is ok to send an API request. Do
    /// **NOT** use to find if the limit has been reached when not also attempting to send a request.
    /// Limited status can be found with [`Self::limited`].
    pub fn within_limit(&mut self) -> bool {
        if self.ct == 0 {
            self.start = Some(Instant::now())
        } else if self.ct >= self.limit {
            return if self.elapsed().unwrap() > self.interval {
                self.start = Some(Instant::now());
                self.ct = 1;
                true
            } else {
                false
            };
        }

        self.ct += 1;
        true
    }

    pub fn limited(&self) -> bool {
        !(self.ct < self.limit || self.elapsed().expect("limit can not be 0") > self.interval)
    }
}

impl<T: RateLimitConfig> std::fmt::Display for RateLimiter<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.limited() {
            write!(
                f,
                "{} service rate limited for another {} seconds",
                T::DISP_NAME,
                (self.remainder().unwrap_or_default()).as_secs()
            )
        } else {
            write!(f, "{} service not currently rate limited", T::DISP_NAME)
        }
    }
}
