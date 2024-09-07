[iw4m-server-master]: https://master.iw4.zip/servers#
[filter-help]: https://i.imgur.com/13Ni21N.png "query arguments"
[reconnect-help]: https://i.imgur.com/bKSbsBL.png "history arguments"
[latest-dl]: https://github.com/WardLordRuby/H2M_favorites/releases/download/v0.4.2/h2m_favorites.exe
<div align="center">
    <img src="https://i.imgur.com/VAxzjQZ.png" width="15%" height="15%">
</div>

# H2M Favorites
[![GitHub Downloads](https://img.shields.io/github/downloads/WardLordRuby/H2M_favorites/total?label=Downloads&labelColor=%2323282e&color=%230e8726)][latest-dl]
[![GitHub License](https://img.shields.io/github/license/WardLordRuby/H2M_favorites?label=License&labelColor=%2323282e)](LICENSE)  

H2M Favorites (name-change-pending) is a launcher that aims to provide quality of life improvements to the base H2M experience.  

It provides a server scraper that supports writing your own queries of H2M servers running [IW4Admin][iw4m-server-master], and a reconnect command to connect you to your last joined server.  

The server scraper fixes the H2M server browser! By limiting favorites.json to 100 entries you will no longer join random servers when trying to connect using the in game browser.
The launcher keeps track of the previous 6 joined servers allowing you to use the reconnect command to easily join back if you happen to not make it into the server during a map change.  

## Installation
1. Download [h2m_favorites.exe][latest-dl]
2. Place h2m_favorites.exe in your MWR(2017) game directory

## Usage
Launch h2m_favorites.exe once it is inside your game directory and it will automatically start H2M-Mod for you. The terminal window will provide you a place to enter commands.

### Commands  
| Commands                     | Alias     | Description                                                                   |
| ---------------------------- | --------- | ----------------------------------------------------------------------------- |
| [filter](#query-help)        | Filter    | Create a new favorites.json using various filter options                      |
| [reconnect](#reconnect-help) | Reconnect | Reconnect to last server joined (or specified entry in history)               |
| launch                       | Launch    | Launch H2M-Mod (reconnect only works if H2M is spawned by this app)           |
| update-cache                 | Reset     | Force cache to reset (useful if reconnect can not find server name in cache)  |
| display-logs                 | Logs      | Display H2M console output                                                    |
| game-dir                     | Gamedir   | Opens your game directory in explorer.exe                                     |
| local-env                    | Localenv  | Opens the local environment directory (where logs and cache is saved)         |
| quit                         | Quit      | Closes game and launcher                                                      |
| help                         | -         | Displays helpful information                                                  |

A help page is available for every command, to access it use: `<COMMAND_NAME> --help`

## Query help
![help][filter-help]

### Custom queries
Filtering H2M servers is easy by adding your own arguments. Any combination of filters is valid  
The filter command with no added arguments will give you the top 100 most populated servers  

Examples:
     
  ```
  filter --team-size-max 6 --exclude trick shot 
  ```
  This query will filter all H2M servers to only include servers with a max team size of 6 and exclude all servers with trick or shot in their title.
  ```
  filter --region na --player-min 2
  ```
  This query will filter all H2M servers to only include servers hosted in North America and have a minimum number of connected players of 2, then save the filtered results to your favorites.json.

#### Tips:
- After running the filter command make sure to have Filter Servers set to Favorites and Refresh to load the new favorite list (bottom right of the H2M server browser)
- Argmuents can be shortened to a single character for example `--includes` can be shortened to `-i`
- To add spaces to your search term surround it in quotations e.g. `filter -i "long search term"`

## Reconnect help
![help][reconnect-help]

By default the reconnect command will connect you to the most recently connected server.  

Arguments:  

  ```
  reconnect --history
  ```
  This will display a numbered list of recently connected servers, 1 being most recent.  
  ```
  reconnect --connect <NUM>
  ```
  Using reconnect with the connect argument or `-c` for short will connect you back to the specified entry in your history.  
