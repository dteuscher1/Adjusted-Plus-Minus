test <- games %>% html_elements('h2') %>% html_text2()
test2 <- paste0(test, ", 2019")
as.Date(test2, "%A, %B %d, %Y")
try <- as.Date(test2, "%A, %B %d, %Y")
try
format(try, "%Y%m%d")

games %>% html_elements('td') %>% html_elements('a') %>% html_text()
info <- data.frame(games %>% html_nodes(xpath="//*[@id=\"sched-container\"]") %>% html_table())

k <- 1
game_day <- character(length = nrow(info))
for(i in 1:nrow(info)){
    if(info$matchup[i] != 'matchup' && info$matchup[i] != "No games scheduled"){
        game_day[i] <- test2[k]
    } else{
        k <- k + 1
        game_day[i] <- test2[k]
    }
}

info2 <- info %>% mutate(game_day = game_day) %>% filter(matchup != 'matchup', matchup != "No games scheduled")

game_id <- games %>% html_nodes(xpath="//td/a") %>% html_attr("href")
game_id = as.data.frame(game_id)
game_id[,1] = as.character(game_id[,1])
game_id = game_id[grep("gameId",game_id[,1]),]
game_ids <- str_extract(game_id, "[0-9]+$")
game_id = gsub("\\/wnba\\/game\\?gameId=","",game_id)
info3 <- info2 %>% mutate(game_day = as.Date(game_day, "%A, %B %d, %Y"),
                          game_id = game_ids, 
                          away_abr = str_extract(matchup, "[A-Z]+$"),
                          home_abr = str_extract(Var.2, "[A-Z]+$"), 
                          away_team = str_extract(matchup, ".+?(?= [A-Z]+$)"),
                          home_team = str_extract(Var.2, ".+?(?= [A-Z]+$)")) %>% 
    separate(result, into = c("Winner", "Loser"), sep = ",") %>%
    mutate(Winner = str_extract(Winner, "[A-Z]+"),
           Loser = str_extract(Loser, "[A-Z]+"))

info3
