[iw4m-server-master]: https://master.iw4.zip/servers#
[help-img]: https://i.imgur.com/vjiBSp1.png "query arguments"
[step-2]: https://i.imgur.com/HRvBykK.png "open command prompt in MWR directory"
[latest-dl]: https://github.com/WardLordRuby/H2M_favorites/releases/download/v0.2.6/h2m_favorites.exe

# H2M Favorites
A command line interface for writing your own queries of servers running [IW4Admin][iw4m-server-master]

This tool fixes the H2M server browser! It fixes that annoying bug where the browser connects you to a random server, by limiting the amount of servers listed in your favorites.json.

## Installation
1. Download [h2m_favorites.exe][latest-dl]
2. Place h2m_favorites.exe in your MWR(2017) game directory

## Usage
Double clicking h2m_favorites.exe will compile your favorites list with the top 100 servers that have the highest player-counts.

#### Custom queries:
Filtering H2M servers is easy by adding your own arguments. We can do this with a few simple steps.
1. Navagate to your installation of MWR(2017)
2. Right click inside the folder (or shift right click) and select 'Open in Terminal' ![tutorial-1][step-2]
3. In the terminal window we will be using an example query for deminstration pupropses  
   Examples:
     
   WindowsPowerShell:  
   ```
   .\h2m_favorites.exe --region na --player-min 2
   ```
   Cmd:
   ```
   h2m_favorites.exe --region na --player-min 2
   ```
   This query will filter all H2M servers to only include servers hosted in North America and have a minimum number of connected players of 2, then save the filtered results to your favorites.json.

#### Tips:
- For help with available filters you can run the command: `h2m_favorites.exe --help`
- To add spaces to your search term surround it in quotations e.g. `h2m_favorites.exe -i "long search term"`
- Commonly used queries can be automated by creating a new txt file and entering your desired query formatted for Cmd. Then save-as "your-custom-query.bat" and place inside your game directory.

## Query help
![help][help-img]


