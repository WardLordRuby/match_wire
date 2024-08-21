[iw4m-server-master]: https://master.iw4.zip/servers#
[help-img]: https://i.imgur.com/Jcupr8A.png "query arguments"
[latest-dl]: https://github.com/WardLordRuby/H2M_favorites/releases/download/v0.1.0/h2m_favorites.exe

# H2M Favorites
A comand line interface for writing your own queries of servers running [IW4Admin][iw4m-server-master]

This tool fixes the H2M server browser! It fixes that annoying bug where the browser connects you to a random server, by limiting the amount of servers listed in your favorites.json.

## Installation
1. Download [h2m_favorites.exe][latest-dl]
2. Place h2m_favorites.exe in your MWR(2017) game directory

#### Notes:
- The default behavior of runing the exe without any command line arguments is to fill favorites.json with the top 100 servers sorted by player-count
- Filtering servers by region is currently slow, still have to work on making the geo-location requests async

## Queries
![help][help-img]


