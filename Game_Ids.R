library(XML)
library(MASS)
library(rvest)

full_games = data.frame(games=(character()))
days = seq(as.Date('2019-05-24'),as.Date('2019-10-10'),by='week')
for(i in seq_along(days)){
  date=format(days[i],format="%Y%m%d")
  games = read_html(paste0("http://www.espn.com/wnba/schedule/_/date/",date))
  games = games %>% html_nodes(xpath="//td/a") %>% html_attr("href")
  games = as.data.frame(games)
  games[,1] = as.character(games[,1])
  games = games[grep("gameId",games[,1]),]
  games = gsub("\\/wnba\\/game\\?gameId=","",games)
  games = as.data.frame(games)
  games[,1] = as.character(games[,1])
  full_games = rbind(games,full_games)
}
full_games
full_games_clean = full_games[!duplicated(full_games[,1]),]
full_games_clean = as.data.frame(full_games_clean)
full_games_clean = full_games_clean[-grep("http",full_games_clean[,1]),]
full_games_clean = as.data.frame(full_games_clean)
names(full_games_clean) = "game_id"
full_games_clean$game_id = as.character(full_games_clean$game_id)
length(unique(full_games_clean$game_id))
full_games_clean
