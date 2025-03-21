[iw4m-server-master]: https://master.iw4.zip/servers#
[latest-dl]: https://github.com/WardLordRuby/match_wire/releases/download/v0.6.0-pre/match_wire.exe
[nexus-link]: https://www.nexusmods.com/callofdutymodernwarfareremastered/mods/2
[hmw-discord]: https://discord.com/invite/HorizonMW
[hmw-launcher-dl]: https://docs.horizonmw.org/download/
[rust-dl]: https://www.rust-lang.org/tools/install
<div align="center">
    <img src="https://i.imgur.com/VAxzjQZ.png" width="15%" height="15%">
</div>

# MatchWire
[<picture>
    <source media="(prefers-color-scheme: dark)" srcset="https://img.shields.io/github/downloads/WardLordRuby/match_wire/total?label=GitHub%20Downloads&labelColor=%2323282e&color=%230e8726">
    <img src="https://img.shields.io/github/downloads/WardLordRuby/match_wire/total?label=GitHub%20Downloads&labelColor=%23F8F8FF&color=%230e8726" alt="GitHub Downloads">
</picture>][latest-dl]
[<picture>
    <source media="(prefers-color-scheme: dark)" srcset="https://img.shields.io/badge/dynamic/json?url=https%3A%2F%2Fgist.githubusercontent.com%2FWardLordRuby%2Fd6ef5e71d937c2310cc8058638ca17fe%2Fraw%2F&query=%24.23308787515394.mod_downloads&label=Nexus%20Downloads&labelColor=%2323282e">
    <img src="https://img.shields.io/badge/dynamic/json?url=https%3A%2F%2Fgist.githubusercontent.com%2FWardLordRuby%2Fd6ef5e71d937c2310cc8058638ca17fe%2Fraw%2F&query=%24.23308787515394.mod_downloads&label=Nexus%20Downloads&labelColor=%23F8F8FF" alt="Nexus Downloads">
</picture>][nexus-link]
[<picture>
    <source media="(prefers-color-scheme: dark)" srcset="https://img.shields.io/github/license/WardLordRuby/match_wire?label=License&labelColor=%2323282e">
    <img src="https://img.shields.io/github/license/WardLordRuby/match_wire?label=License&labelColor=%23F8F8FF" alt="GitHub License">
</picture>](LICENSE)

MatchWire is a game launcher that enhances the core H2M/HMW (Mw2 Remastered) experience by offering key quality-of-life
improvements. From its core MatchWire is designed to be extremely low overhead, providing no in game performance degradation
with even the lowest end pc's.  

It provides a server scraper that supports writing your own queries of Mw2 Remastered servers running [IW4Admin][iw4m-server-master]
and/or servers found on the HMW master server, as well as a reconnect command to connect you to your last joined server.  

MatchWire keeps track of the previous 6 joined servers allowing you to use the reconnect command to easily join back if you
happen to not make it into the server during a map change. The program also keeps track of frequently used commands to make
it ultra quick to execute your carefully crafted server filters.  

## Disclaimer
MatchWire is an independent project and is not affiliated with, endorsed by, or connected to the H2M development team or the
HMW development team. This repository does not distribute any game files. Users are responsible for obtaining the game through
official channels.  

MatchWire provides no competitive advantage and does not affect gameplay in any way. The program is designed solely to offer
quality-of-life improvements. MatchWire does not modify game mechanics, alter game balance, or provide any features that would
give users an unfair advantage over other players.  

## Compatibility
MatchWire is only _fully_ supported on Windows 10 version 1809 (October 2018) and later. The server scraper feature set will
work fine on older versions of windows, and _should_ work on linux when you compile from source (untested)

## Installation
1. Locate / Install Modern Warfare Remastered (2017)
2. Install Horizon MW via the launcher found on their [Discord][hmw-discord] | [direct-download][hmw-launcher-dl]
3. Download [match_wire.exe][latest-dl]
4. Place match_wire.exe in your MWR(2017) game directory and run

## Usage
Launch match_wire.exe once it is inside your game directory and it will automatically start Mw2 Remastered for you. The terminal
window will provide you a place to enter commands. MatchWire includes a command auto-complete feature, just use the tab key to
walk through available commands and command options. Pressing 'ctrl + c' will clear the current line or if line current line is
empty it will close MatchWire. Note that closing MatchWire will also close Mw2 Remastered.  

### Commands
| Commands                     | Alias     | Description                                                                     |
| ---------------------------- | --------- | ------------------------------------------------------------------------------- |
| [filter](#filter-help)       | Filter    | Create a new favourites.json using various filter options                       |
| [reconnect](#reconnect-help) | Reconnect | Reconnect to last server joined (or specified entry in history)                 |
| launch                       | Launch    | Launch Mw2 Remastered (reconnect only works if the game is spawned by this app) |
| cache                        | Cache     | Update / Reset cache (useful if reconnect can not find server name in cache)    |
| [console](#console-help)     | Logs      | Display and interact with the Mw2 Remastered console                            |
| game-dir                     | Gamedir   | Opens your game directory in explorer.exe                                       |
| local-env                    | Localenv  | Opens the local environment directory (where logs and cache are saved)          |
| quit                         | Quit      | Closes game and launcher                                                        |
| version                      | Version   | Displays version of MatchWire and Mw2 Remastered                                |
| help                         | -         | Displays helpful information                                                    |

A help page is available for every command, to access it use: `<COMMAND_NAME> --help`

## Filter help
| Options                        | Arguments      | Short | Description                                                          |
| ------------------------------ | -------------- | ----- | -------------------------------------------------------------------- |
| `--limit <LIMIT>`[^1]          | Number         | `-l`  | Specify the maximum number of servers added to favourites.json       |
| `--player-min <PLAYER_MIN>`    | Number         | `-p`  | Specify a minimum number of players a server must have [Default: 0]  |
| `--with-bots`                  | -              | -     | Server _must_ contain bot players                                    |
| `--without-bots`               | -              | -     | Server can _not_ contain bot players                                 |
| `--include-unresponsive`       | -              | -     | Include servers that do not respond to a 'getInfo' request           |
| `--region <REGION>...`         | [na, eu, apac] | `-r`  | Specify region(s) server _must_ be located in [Default: include all] |
| `--source <SOURCE>...`         | [iw4, hmw]     | `-s`  | Specify source(s) to populate from [Default: include all]            |
| `--includes <INCLUDES>...`[^2] | String(s)      | `-i`  | Server name _must_ contain any 1 of the given term(s)                |
| `--excludes <EXCLUDES>...`[^2] | String(s)      | `-e`  | Server name _must not_ contain any 1 of the given term(s)            |
| `--retry-max <RETRY_MAX>`      | Number         | -     | Specify a maximum number of 'getInfo' retries [Default: 3]           |
| `--help`                       | -              | `-h`  | Print help                                                           |

[^1]: The default server limit for **H2M users** is set to 100. Limiting favourites.json will fix the 'join random servers' bug
  when trying to connect via the in game server browser. On **Horizon MW** this issue is fixed and no limit is applied to the
  number of servers MatchWire will add to your favourites.json.
[^2]: Search terms are case-insensitive. Excludes has higher priority if include and exclude are used together.

### Custom queries
Filtering Mw2 Remastered servers is easy by adding your own arguments. Any combination of filters is valid.  

Examples:

  ```
  filter --team-size-max 6 --exclude trick shot 
  ```
  This query will filter all Mw2 Remastered servers to only include servers with a max team size of 6 and exclude all servers
  with "trick" or "shot" in their title.
  ```
  filter --region na --player-min 2
  ```
  This query will filter all Mw2 Remastered servers to only include servers hosted in North America and have a minimum number of
  connected players of 2, then save the filtered results to your favourites.json.  

#### Tips
- After running the filter command make sure to have Filter Servers set to Favorites and Refresh to load the new favorite list
  (bottom right of the server browser)
- Arguments can be shortened to a single character for example `--includes` can be shortened to `-i`
- To add spaces to your search term surround it in quotations e.g. `filter -i "long search term"`

## Reconnect help
<div align="center">

  | Options               | Arguments | Short | Description                            |
  | :-------------------- | :-------- | :---- | :------------------------------------- |
  | `--history`           | -         | `-H`  | Display previously connected servers   |
  | `--connect <CONNECT>` | Number    | `-c`  | Connect to a numbered entry in history |
  | `--help`              | -         | `-h`  | Print help                             |

</div>

Executing the reconnect command with no additional options will connect you to the most recently connected server.  

Examples:

  ```
  reconnect --history
  ```
  This will display a numbered list of recently connected servers, 1 being most recent.  
  ```
  reconnect --connect <NUM>
  ```
  Using reconnect with the connect argument or `-c` for short will connect you back to the specified entry in your history.  

## Console help
The console command is an easy way to interact and view Mw2 Remastered's console window. Sending commands to the console works
just as it normally would, simply type the command and press enter to send. Pressing 'backspace' or 'ctrl + c' when the input
line is empty will leave the game console and return back to MatchWire.  

## Build from source
If you desire to build from source the process is straight forward. Make sure you have [rust][rust-dl] installed. 
1. Download or clone the source code  
2. Follow the instructions in [location_api_key.rs.template](src/location_api_key.rs.template)
3. Compile the project with `cargo build --release`
