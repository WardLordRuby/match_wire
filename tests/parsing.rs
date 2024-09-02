#[cfg(test)]
mod tests {
    use h2m_favorites::commands::launch_h2m::HostName;

    #[test]
    fn parse_hostnames() {
        const INPUT: [&str; 4] = ["\u{1b}[m\u{1b}[38;5;3m\u{1b}[mJoining [US] ^5CWS ^7| ^1Best Maps ^:TDM...",
            "\u{1b}[mLoading fastfile imagefile1",
            "\u{1b}[38;5;3mWaited 1100 msec for asset \"m/mtl_ret_h1_transparent_01\", of type \"material\"",
            "\u{1b}[m\u{1b}[38;5;3m\u{1b}[mJoining ^3:: OP GOLD ::^7 | ^1Mosh Pit^7 | ^:MW2/COD4 MAPS^7 | ^1Double XP^7 | ^:Map Vote^7 | ^1#2.."
        ];

        const OUTPUT: [(&str, &str); 4] = [("[us] cws | best maps tdm", "[US] ^5CWS ^7| ^1Best Maps ^:TDM"),
            ("loading fastfile imagefile1", "Loading fastfile imagefile1"),
            ("waited 1100 msec for asset \"m/mtl_ret_h1_transparent_01\", of type \"material\"", "Waited 1100 msec for asset \"m/mtl_ret_h1_transparent_01\", of type \"material\""),
            (":: op gold :: | mosh pit | mw2/cod4 maps | double xp | map vote | #2", "^3:: OP GOLD ::^7 | ^1Mosh Pit^7 | ^:MW2/COD4 MAPS^7 | ^1Double XP^7 | ^:Map Vote^7 | ^1#2")
        ];

        for (i, host) in INPUT.iter().enumerate() {
            let wide_bytes = host.encode_utf16().collect::<Vec<_>>();
            let data = HostName::from(&wide_bytes[..]);
            assert_eq!(data.parsed, OUTPUT[i].0);
            assert_eq!(data.raw, OUTPUT[i].1);
        }
    }
}