library(XML)
library(MASS)
library(rvest)

game_info <-  data.frame(Winner=character(), Loser = character(), game_day = character(), game_id = character(),
                         away_abr = character(), home_abr = character(), away_team = character(), 
                         home_team = character())
playoff_characters <- c("FIRST ROUND", "SECOND ROUND")
days <- seq(as.Date('2019-05-24'),as.Date('2019-10-10'),by='week')
for(i in seq_along(days)){
    date <- format(days[i],format="%Y%m%d")
    games <- read_html(paste0("http://www.espn.com/wnba/schedule/_/date/",date))
    game_days <- games %>% html_elements('h2') %>% html_text2()
    info <- data.frame(games %>% html_nodes(xpath="//*[@id=\"sched-container\"]") %>% html_table())[,1:5]
    colnames(info) <- c('matchup', "Var.2", "result", "winner.high", "loser.high")
    k <- 1
    game_date <- character(length = nrow(info))
    for(j in 1:nrow(info)){
        if(info$matchup[j] != 'matchup' && info$matchup[j] != "No games scheduled"){
            game_date[j] <- game_days[k]
        } else if(j == 1){
            game_date[j] <- game_days[k]
        } else{
            k <- k + 1
            game_date[j] <- game_days[k]
        }
    }
    info <- info %>% mutate(game_date = game_date) %>% 
        filter(matchup != 'matchup', matchup != "No games scheduled", matchup != "FIRST ROUND", !(matchup %in% playoff_characters), 
               !str_detect(matchup, "WNBA SEMIFINALS"), !str_detect(matchup, "WNBA FINALS"))
    game_id <- games %>% html_nodes(xpath="//td/a") %>% html_attr("href")
    game_id = as.data.frame(game_id)
    game_id[,1] = as.character(game_id[,1])
    game_id = game_id[grep("gameId",game_id[,1]),]
    game_ids <- str_extract(game_id, "[0-9]+$")
    info_all <- info %>% mutate(game_day = game_date,
                              game_id = game_ids, 
                              away_abr = str_extract(matchup, "[A-Z]+$"),
                              home_abr = str_extract(Var.2, "[A-Z]+$"), 
                              away_team = str_extract(matchup, ".+?(?= [A-Z]+$)"),
                              home_team = str_extract(Var.2, ".+?(?= [A-Z]+$)")) %>% 
        separate(result, into = c("Winner", "Loser"), sep = ",") %>%
        mutate(Winner = str_extract(Winner, "[A-Z]+"),
               Loser = str_extract(Loser, "[A-Z]+")) %>% 
        dplyr::select(-matchup, -Var.2, -winner.high, -loser.high)
    game_info <- game_info %>% bind_rows(info_all)
    Sys.sleep(10)
}

game_info <- game_info %>% filter(Winner != "WIL") %>% 
    mutate(game_day = paste0(game_day, ", 2019"),
           game_day = as.Date(game_day, "%A, %B %d, %Y"),
           game_day = format(game_day, "%Y%m%d"))

# test <- games %>% html_elements('h2') %>% html_text2()
# test2 <- paste0(test, ", 2019")
# as.Date(test2, "%A, %B %d, %Y")
# try <- as.Date(test2, "%A, %B %d, %Y")
# try
# format(try, "%Y%m%d")
# 
# games %>% html_elements('td') %>% html_elements('a') %>% html_text()
# info <- data.frame(games %>% html_nodes(xpath="//*[@id=\"sched-container\"]") %>% html_table())
# 
# k <- 1
# game_day <- character(length = nrow(info))
# for(i in 1:nrow(info)){
#     if(info$matchup[i] != 'matchup' && info$matchup[i] != "No games scheduled"){
#         game_day[i] <- test2[k]
#     } else{
#         k <- k + 1
#         game_day[i] <- test2[k]
#     }
# }
# game_info <- game_info %>% filter(Winner != "WIL")
# info2 <- info %>% mutate(game_day = game_day) %>% filter(matchup != 'matchup', matchup != "No games scheduled")
# 
# game_id <- games %>% html_nodes(xpath="//td/a") %>% html_attr("href")
# game_id = as.data.frame(game_id)
# game_id[,1] = as.character(game_id[,1])
# game_id = game_id[grep("gameId",game_id[,1]),]
# game_ids <- str_extract(game_id, "[0-9]+$")
# game_id = gsub("\\/wnba\\/game\\?gameId=","",game_id)
# info3 <- info2 %>% mutate(game_day = as.Date(game_day, "%A, %B %d, %Y"),
#                           game_id = game_ids, 
#                           away_abr = str_extract(matchup, "[A-Z]+$"),
#                           home_abr = str_extract(Var.2, "[A-Z]+$"), 
#                           away_team = str_extract(matchup, ".+?(?= [A-Z]+$)"),
#                           home_team = str_extract(Var.2, ".+?(?= [A-Z]+$)")) %>% 
#     separate(result, into = c("Winner", "Loser"), sep = ",") %>%
#     mutate(Winner = str_extract(Winner, "[A-Z]+"),
#            Loser = str_extract(Loser, "[A-Z]+")) %>% dplyr::select(-matchup, -Var.2, -winner.high, -loser.high, -Var.6, -Var.7)
# 
# info3
