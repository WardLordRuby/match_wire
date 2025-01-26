#[cfg(test)]
mod tests {
    use std::net::{IpAddr, Ipv4Addr, SocketAddr};

    use match_wire::{commands::launch_h2m::HostName, strip_ansi_private_modes};
    use repl_oxide::strip_ansi;

    #[test]
    fn parse_hostnames_h2m() {
        const INPUT: [&str; 3] = [
            "\u{1b}[m\u{1b}[38;5;3m\u{1b}[mJoining [US] ^5CWS ^7| ^1Best Maps ^:TDM...",
            "\u{1b}[m\u{1b}[38;5;3m\u{1b}[mJoining ^3:: OP GOLD ::^7 | ^1Mosh Pit^7 | ^:MW2/COD4 MAPS^7 | ^1Double XP^7 | ^:Map Vote^7 | ^1#2...",
            "\u{1b}[m\u{1b}[38;5;3m\u{1b}[mJoining ^1[^2F^3r^4e^5a^5k ^1o^2f ^3D^4u^5t^6y^1] ^7 24/7 DOM | ^: [2XP] ^7 | ^1 [LA-2]..."
        ];

        const OUTPUT: [(&str, &str); 3] = [
            ("[us] cws | best maps tdm", "[US] ^5CWS ^7| ^1Best Maps ^:TDM"),
            (":: op gold :: | mosh pit | mw2/cod4 maps | double xp | map vote | #2", "^3:: OP GOLD ::^7 | ^1Mosh Pit^7 | ^:MW2/COD4 MAPS^7 | ^1Double XP^7 | ^:Map Vote^7 | ^1#2"),
            ("[freak of duty]  24/7 dom |  [2xp]  |  [la-2]", "^1[^2F^3r^4e^5a^5k ^1o^2f ^3D^4u^5t^6y^1] ^7 24/7 DOM | ^: [2XP] ^7 | ^1 [LA-2]")
        ];

        for (i, host) in INPUT.iter().enumerate() {
            let wide_bytes = host.encode_utf16().collect::<Vec<_>>();
            let data = HostName::from_browser(&wide_bytes[..], 0.4).unwrap();
            assert!(data.socket_addr.is_none());
            assert_eq!(data.host_name.parsed, OUTPUT[i].0);
            assert_eq!(data.host_name.raw, OUTPUT[i].1);
        }
    }

    #[test]
    fn parse_hostnames_hmw() {
        const INPUT: [&str; 4] = [
            "Connecting to server:[0] {152.53.39.127:27017} ^5[US-EAST] ^3Respex ^1| ^6Map Voting ^1| ^2COD4^7/^3MW2 ^4Maps ^1| ^9HARDCORE TDM ^1| 2XP",
            "Connecting to server:[49] {2.56.166.179:27017} ^1Crimson Tide ^7| ^524/7 ^3Shipment ^7& ^3Rust ^5US #6",
            "Connecting to server:[33] {99.41.89.109:27020} ^:[NA-S] ^7| ^2Ashes ^7SnD 6v6 ^62XP",
            "Connecting to server:[3] {103.195.100.207:29737} ^1~ M ~ ^2MANIACOS ^7SHIPMENT"
        ];

        const OUTPUT: [(&str, &str, SocketAddr); 4] = [
            ("[us-east] respex | map voting | cod4/mw2 maps | hardcore tdm | 2xp", "^5[US-EAST] ^3Respex ^1| ^6Map Voting ^1| ^2COD4^7/^3MW2 ^4Maps ^1| ^9HARDCORE TDM ^1| 2XP", SocketAddr::new(IpAddr::V4(Ipv4Addr::new(152, 53, 39, 127)), 27017)),
            ("crimson tide | 24/7 shipment & rust us #6", "^1Crimson Tide ^7| ^524/7 ^3Shipment ^7& ^3Rust ^5US #6", SocketAddr::new(IpAddr::V4(Ipv4Addr::new(2, 56, 166, 179)), 27017)),
            ("[na-s] | ashes snd 6v6 2xp", "^:[NA-S] ^7| ^2Ashes ^7SnD 6v6 ^62XP", SocketAddr::new(IpAddr::V4(Ipv4Addr::new(99, 41, 89, 109)), 27020)),
            ("~ m ~ maniacos shipment", "^1~ M ~ ^2MANIACOS ^7SHIPMENT", SocketAddr::new(IpAddr::V4(Ipv4Addr::new(103, 195,100, 207)), 29737))
        ];

        for (i, host) in INPUT.iter().enumerate() {
            let wide_bytes = host.encode_utf16().collect::<Vec<_>>();
            let data = HostName::from_browser(&wide_bytes[..], 1.1).unwrap();
            assert_eq!(data.host_name.parsed, OUTPUT[i].0);
            assert_eq!(data.host_name.raw, OUTPUT[i].1);
            assert_eq!(data.socket_addr.unwrap().unwrap(), OUTPUT[i].2);
        }
    }

    #[test]
    fn parse_ansi_sequences() {
        const INPUT: [&str; 5] = [
            "\u{1b}[?25hLoading fastfile mp_shirt_028_p_tr",
            "\u{1b}[?25h]",
            "\u{1b}[38;5;3mWaited 1100 msec for asset \"m/mtl_ret_h1_transparent_01\", of type \"material\"",
            "\u{1b}[mLoading fastfile imagefile1",
            "\u{1b}[38;5;3m\u{1b}[?25lLoading fastfile \u{1b}[mimagefile1\u{1b}[?25h",
        ];

        const OUTPUT: [&str; 5] = [
            "Loading fastfile mp_shirt_028_p_tr",
            "]",
            "Waited 1100 msec for asset \"m/mtl_ret_h1_transparent_01\", of type \"material\"",
            "Loading fastfile imagefile1",
            "Loading fastfile imagefile1",
        ];

        for (i, input) in INPUT.iter().enumerate() {
            let parsed = strip_ansi(input);
            assert_eq!(parsed, OUTPUT[i]);
        }
    }

    #[test]
    fn parse_ansi_private_modes() {
        const INPUT: [&str; 4] = [
            "\u{1b}[?25hLoading fastfile mp_shirt_028_p_tr",
            "\u{1b}[?25h]",
            "\u{1b}[38;5;3mWaited 1100 msec for asset \u{1b}[?47h\"m/mtl_ret_h1_transparent_01\", of type \u{1b}[?47l\"material\"\u{1b}[m",
            "\u{1b}[?25hLoading fastfile \u{1b}[?25lmp_shirt_028_p_tr",
        ];

        const OUTPUT: [&str; 4] = [
            "Loading fastfile mp_shirt_028_p_tr",
            "]",
            "\u{1b}[38;5;3mWaited 1100 msec for asset \"m/mtl_ret_h1_transparent_01\", of type \"material\"\u{1b}[m",
            "Loading fastfile mp_shirt_028_p_tr",
        ];

        for (i, input) in INPUT.iter().enumerate() {
            let parsed = strip_ansi_private_modes(input);
            assert_eq!(parsed, OUTPUT[i]);
        }
    }
}
