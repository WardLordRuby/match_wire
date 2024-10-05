#[cfg(test)]
mod tests {
    use match_wire::commands::launch_h2m::HostName;

    #[test]
    fn parse_hostnames() {
        const INPUT: [&str; 7] = [
            "\u{1b}[m\u{1b}[38;5;3m\u{1b}[mJoining [US] ^5CWS ^7| ^1Best Maps ^:TDM...",
            "\u{1b}[m Loading fastfile imagefile1",
            "\u{1b}[38;5;3m Waited 1100 msec for asset \"m/mtl_ret_h1_transparent_01\", of type \"material\"",
            "\u{1b}[m\u{1b}[38;5;3m\u{1b}[mJoining ^3:: OP GOLD ::^7 | ^1Mosh Pit^7 | ^:MW2/COD4 MAPS^7 | ^1Double XP^7 | ^:Map Vote^7 | ^1#2..",
            "[?25h Loading fastfile mp_shirt_028_p_tr",
            "[?25h]",
            "\u{1b}[m\u{1b}[38;5;3m\u{1b}[mJoining ^1[^2F^3r^4e^5a^5k ^1o^2f ^3D^4u^5t^6y^1] ^7 24/7 DOM | ^: [2XP] ^7 | ^1 [LA-2]..."
        ];

        const OUTPUT: [(&str, &str); 7] = [
            ("[us] cws | best maps tdm", "[US] ^5CWS ^7| ^1Best Maps ^:TDM"),
            ("loading fastfile imagefile1", "Loading fastfile imagefile1"),
            ("waited 1100 msec for asset \"m/mtl_ret_h1_transparent_01\", of type \"material\"", "Waited 1100 msec for asset \"m/mtl_ret_h1_transparent_01\", of type \"material\""),
            (":: op gold :: | mosh pit | mw2/cod4 maps | double xp | map vote | #2", "^3:: OP GOLD ::^7 | ^1Mosh Pit^7 | ^:MW2/COD4 MAPS^7 | ^1Double XP^7 | ^:Map Vote^7 | ^1#2"),
            ("loading fastfile mp_shirt_028_p_tr", "Loading fastfile mp_shirt_028_p_tr"),
            ("", ""),
            ("[freak of duty]  24/7 dom |  [2xp]  |  [la-2]", "^1[^2F^3r^4e^5a^5k ^1o^2f ^3D^4u^5t^6y^1] ^7 24/7 DOM | ^: [2XP] ^7 | ^1 [LA-2]")
        ];

        for (i, host) in INPUT.iter().enumerate() {
            let wide_bytes = host.encode_utf16().collect::<Vec<_>>();
            let data = HostName::from_browser(&wide_bytes[..], 0.4);
            assert_eq!(data.host_name.parsed, OUTPUT[i].0);
            assert_eq!(data.host_name.raw, OUTPUT[i].1);
        }
    }
}
