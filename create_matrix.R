library(wehoop)
library(tidyverse)
source("wnba_pbp_scrape.R")

pbp_2019 <- load_wnba_pbp(2019)
game_ids <- unique(pbp_2019$game_id)

empty_data_frame <- data.frame(home_points = 0, away_points = 0 , point_diff = 0, Player = "First Row")
# Games 4, 7, 11, 12, 18, 22, 25, 26, 28, 32
for(i in 227){
    game_id_string <- paste(game_ids[i])
    pbp <- pbp_2019 %>% filter(game_id == game_ids[i])
    data <- possession_data(game_id_string, pbp)
    empty_data_frame <- empty_data_frame %>% bind_rows(data)
}

game_ids[18]
