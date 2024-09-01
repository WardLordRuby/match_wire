use crate::commands::handler::CommandHandle;

// MARK: TODO
// 1. find correct threadProcessId - Done
// 2. get rawHandles to stdout and stdin of that threadProcessId - Done
// 3. filter stdout reads - Done
// 4. create a stack of (color_coded_hostname, parsed_hostname) - Done
// 5. create a fn to display list of parsed_hostname -> connect ip:port
// 6. add feat to connect to specify what number in the stack to connect to
// 7. create launch h2m command
// 8. send commands into stdin psuedo terminal

pub fn reconnect(show_history: bool) -> CommandHandle {
    if show_history {
        todo!();
    }
    todo!()
}
