#[cfg(test)]
mod test {
    use match_wire::{impl_rate_limit_config, utils::limiter::RateLimiter};
    use std::time::Duration;

    const TEST_LIMIT: usize = 10;
    const TEST_INTERVAL: Duration = Duration::from_millis(800);

    impl_rate_limit_config!(Test, TEST_LIMIT, TEST_INTERVAL);

    #[test]
    fn rate_limiter() {
        const TIME: Duration = Duration::from_millis(10);
        let mut limiter = RateLimiter::<Test>::new();

        for _ in 0..TEST_LIMIT {
            assert!(!limiter.limited());
            assert!(limiter.within_limit());
        }

        assert!(limiter.limited());
        assert!(!limiter.within_limit());

        std::thread::sleep(TEST_INTERVAL - TIME);

        assert!(limiter.limited());
        assert!(!limiter.within_limit());

        std::thread::sleep(TIME);

        assert!(!limiter.limited());
        assert!(limiter.within_limit());
    }
}
