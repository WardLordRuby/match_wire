#[cfg(test)]
mod tests {
    use match_wire::{
        H2M_MAX_CLIENT_NUM,
        commands::filter::{Server, Sourced, process_stats},
        models::json_data::GetInfo,
        utils::{
            display::{
                DisplayFilterStats, GAME_TYPE_IDS, Line, MAP_IDS, MIN_FILTER_COLS, count_digits,
            },
            global_state,
        },
    };

    use std::net::{IpAddr, Ipv4Addr, Ipv6Addr, SocketAddr};

    use rand::{Rng, distr::Alphanumeric, seq::IndexedRandom};

    #[test]
    fn line_len() {
        for i in 0..512 {
            let line = Line(i).to_string();
            assert_eq!(i, line.chars().count());
        }
    }

    #[test]
    fn num_len() {
        let mut rng = rand::rng();

        for _ in 0..512 {
            let num = rng.random::<u32>() as usize;
            assert_eq!(count_digits(num), num.to_string().chars().count());
        }
    }

    fn random_socket_addr<R: Rng>(rng: &mut R) -> SocketAddr {
        let port = rng.random_range(27016..=27030);

        let ip = if rng.random_bool(0.75) {
            IpAddr::V4(Ipv4Addr::new(
                rng.random::<u8>(),
                rng.random::<u8>(),
                rng.random::<u8>(),
                rng.random::<u8>(),
            ))
        } else {
            let (d, e) = if rng.random_bool(0.5) {
                (rng.random::<u16>(), rng.random::<u16>())
            } else {
                (0, 0)
            };

            IpAddr::V6(Ipv6Addr::new(
                rng.random::<u16>(),
                rng.random::<u16>(),
                rng.random::<u16>(),
                d,
                e,
                rng.random::<u16>(),
                rng.random::<u16>(),
                rng.random::<u16>(),
            ))
        };

        SocketAddr::new(ip, port)
    }

    fn random_string<R: Rng>(rng: &mut R) -> String {
        (1..rng.random_range(10..=100))
            .map(|_| rng.sample(Alphanumeric) as char)
            .collect()
    }

    fn random_version<R: Rng>(rng: &mut R) -> String {
        let mut random_num = || rng.random_range(0..=10_u8);
        format!("{}.{}.{}", random_num(), random_num(), random_num())
    }

    fn gen_test_servers() -> Vec<Server> {
        let map_names = MAP_IDS.iter().map(|&(_, v)| v).collect::<Vec<_>>();
        let game_types = GAME_TYPE_IDS.iter().map(|&(_, v)| v).collect::<Vec<_>>();
        let rng = &mut rand::rng();

        let mut servers = Vec::with_capacity(60);

        for _ in 0..60 {
            let max_clients = rng.random_range(2..=H2M_MAX_CLIENT_NUM as u8);
            let bots = rng
                .random_bool(0.5)
                .then(|| max_clients - rng.random_range(1..=max_clients))
                .unwrap_or_default();

            servers.push(Server {
                source: Sourced::Hmw(random_socket_addr(rng)),
                info: GetInfo {
                    clients: rng.random_range(bots..=max_clients),
                    max_clients,
                    game_type: if rng.random_bool(0.7) {
                        game_types.choose(rng).unwrap().to_string()
                    } else {
                        random_string(rng)
                    }
                    .into(),
                    host_name: random_string(rng),
                    map_name: if rng.random_bool(0.7) {
                        map_names.choose(rng).unwrap().to_string()
                    } else {
                        random_string(rng)
                    }
                    .into(),
                    bots,
                    private: rng.random_bool(0.10),
                    private_clients: -1,
                    game_version: if rng.random_bool(0.8) {
                        random_version(rng)
                    } else {
                        random_string(rng)
                    },
                },
            });
        }

        servers
    }

    #[test]
    fn display_servers() {
        global_state::Cache::set(global_state::Cache::default());
        let mut servers = gen_test_servers();
        process_stats(&mut servers);

        for i in 0..21 {
            println!("{}", DisplayFilterStats(&servers, MIN_FILTER_COLS + i * 3))
        }
    }
}
