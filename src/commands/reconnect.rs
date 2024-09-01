use crate::{
    cli::HistoryArgs,
    commands::{
        handler::{CommandContext, CommandHandle},
        launch_h2m::HostName,
    },
};
use std::{ffi::OsString, fmt::Display};
use tracing::error;
use winptyrs::PTY;

// MARK: TODO
// 1. find correct threadProcessId - Done
// 2. get rawHandles to stdout and stdin of that threadProcessId - Done
// 3. filter stdout reads - Done
// 4. create a stack of (color_coded_hostname, parsed_hostname) - Done
// 5. create a fn to display list of parsed_hostname -> connect ip:port
// 6. add feat to connect to specify what number in the stack to connect to
// 7. create launch h2m command
// 8. send commands into stdin psuedo terminal

struct DisplayHistory<'a>(&'a [HostName]);

impl<'a> Display for DisplayHistory<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, host_name) in self.0.iter().rev().enumerate() {
            writeln!(f, "{i}. {}", host_name.parsed)?;
            if i == 6 {
                break;
            }
        }
        Ok(())
    }
}

struct DisplayHistoryErr(usize);

impl Display for DisplayHistoryErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0 == 1 {
            writeln!(f, "History only contains 1 entry")?;
        } else {
            writeln!(f, "History only contains {} entries", self.0)?;
        }
        Ok(())
    }
}

pub fn reconnect(args: HistoryArgs, context: &CommandContext) -> CommandHandle {
    let h2m_handle_arc = context.h2m_handle();
    if h2m_handle_arc.is_none() {
        error!("Use the 'launch' command to open H2M-Mod");
        return CommandHandle::default();
    }
    let server_history_arc = context.h2m_server_connection_history();
    let server_history = server_history_arc.blocking_lock();
    if server_history.is_empty() {
        error!("No joined servers in history, connect to a server to add it to history");
        return CommandHandle::default();
    }
    if args.history {
        println!("{}", DisplayHistory(&server_history));
    }
    let history_len = server_history.len();
    let cache_arc = context.cache();
    let cache = cache_arc.blocking_lock();
    let connect = if let Some(num) = args.connect {
        if num > history_len {
            error!("{}", DisplayHistoryErr(history_len));
            return CommandHandle::default();
        }
        cache
            .host_to_connect
            .get(&server_history[history_len - num].raw)
    } else {
        cache
            .host_to_connect
            .get(&server_history.last().unwrap().raw)
    };
    if let Some(ip_port) = connect {
        connect_to(ip_port, &h2m_handle_arc.unwrap()).unwrap_or_else(|err| error!("{err}"));
    } else {
        error!("Could not find server in cache")
    }
    CommandHandle::default()
}

fn connect_to(ip_port: &str, handle: &PTY) -> Result<(), String> {
    let send_command =
        |command: &dyn AsRef<str>| match handle.write(OsString::from(command.as_ref())) {
            Ok(chars) => {
                if chars == 0 {
                    Err(String::from("Failed to send command to h2m console"))
                } else {
                    Ok(())
                }
            }
            Err(err) => Err(err.to_string_lossy().to_string()),
        };

    send_command(&"disconnect\r\n")?;
    std::thread::sleep(std::time::Duration::from_millis(5));
    send_command(&format!("connect {ip_port}\r\n"))
}
