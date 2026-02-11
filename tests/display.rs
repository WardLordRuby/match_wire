#[cfg(test)]
mod test {
    use match_wire::{
        MAX_H2M_CLIENT_NUM,
        commands::filter::{FilterPreProcess, GameStats, Server, Sourced},
        models::{cli::Source, json_data::GetInfo},
        utils::display::{
            GAME_DISPLAY_NAMES, GAME_MODE_IDS, Line, MAP_IDS,
            table::{
                DisplayFilterStats, DisplaySourceStats, DisplaySourceStatsInner, MIN_FILTER_COLS,
                count_digits,
            },
        },
    };

    use std::{
        collections::HashMap,
        net::{IpAddr, Ipv4Addr, Ipv6Addr, SocketAddr},
    };

    use rand::{Rng, RngExt, distr::Alphanumeric, seq::IndexedRandom};

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

    fn gen_test_servers<R: Rng>(rng: &mut R) -> Vec<Server> {
        let map_names = MAP_IDS.iter().map(|&(_, v)| v).collect::<Vec<_>>();
        let game_types = GAME_MODE_IDS.iter().map(|&(_, v)| v).collect::<Vec<_>>();

        let mut servers = Vec::with_capacity(60);

        for _ in 0..60 {
            let max_clients = rng.random_range(2..=MAX_H2M_CLIENT_NUM);
            let bots = if rng.random_bool(0.5) {
                max_clients - rng.random_range(1..=max_clients)
            } else {
                0
            };

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
        let rng = &mut rand::rng();
        let mut servers = gen_test_servers(rng);
        let pre_process = FilterPreProcess::compute(&mut servers);

        for i in 0..21 {
            let width = MIN_FILTER_COLS + i * 3;
            println!(
                "{}",
                DisplayFilterStats(&servers, &pre_process, &HashMap::new(), width)
            )
        }
    }

    fn random_game_strings<R: Rng, B: IntoIterator<Item = usize>>(
        rng: &mut R,
        range: B,
    ) -> Vec<String> {
        let mut choose = || {
            if rng.random_bool(0.8) {
                GAME_DISPLAY_NAMES.choose(rng).unwrap().0.to_string()
            } else {
                random_string(rng)
            }
        };

        range.into_iter().map(|_| choose()).collect()
    }

    fn gen_test_sources<R: Rng>(rng: &mut R) -> Vec<DisplaySourceStatsInner> {
        let len = 0..14;
        let mut hmw = GameStats::new(&random_game_strings(rng, len.clone()));
        let mut hmw_total = GameStats::default();

        let test_servers = len.map(|_| gen_test_servers(rng)).collect::<Vec<_>>();

        for (i, servers) in test_servers.iter().map(Vec::as_slice).enumerate() {
            for sourced_server in servers {
                let entry = &mut hmw[i];
                let (is_server, is_unresponsive, player_ct) = if rng.random_bool(0.9) {
                    let player_ct = sourced_server.info.player_ct() as usize;

                    entry.servers += 1;
                    entry.players += player_ct;
                    (1, 0, player_ct)
                } else {
                    entry.unresponsive += 1;
                    (0, 1, 0)
                };
                hmw_total.add((is_server, is_unresponsive, player_ct));
            }
        }

        vec![(
            Source::HmwMaster,
            hmw_total,
            hmw,
            Some(rng.random_range(0..200)),
        )]
    }

    #[test]
    fn display_sources() {
        let rng = &mut rand::rng();
        let sources = gen_test_sources(rng);

        for _ in 0..10 {
            println!("{}", DisplaySourceStats(&sources))
        }
    }
}
