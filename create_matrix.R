library(wehoop)
library(tidyverse)
source("wnba_pbp_scrape.R")

pbp_2019 <- load_wnba_pbp(2019)
preseason <- 401129933:401129947
pbp_2019 <- pbp_2019 %>% filter(!(game_id %in% preseason))
pbp_2019$text[pbp_2019$game_id == 401104916 & pbp_2019$sequence_number == 424] <- "Bria Holmes enters the game for Shekinna Stricklen"
pbp_2019$text[pbp_2019$game_id == 401104919 & pbp_2019$game_play_number == 275] <- "Shenise Johnson enters the game for Betnijah Laney"
pbp_2019$text[pbp_2019$game_id == 401104924 & pbp_2019$game_play_number == 113] <- "Tina Charles enters the game for Nayo Raincock-Ekunwe"
pbp_2019$text[pbp_2019$game_id == 401104937 & pbp_2019$game_play_number == 108] <- "Nneka Ogwumike enters the game for Tierra Ruffin-Pratt"
pbp_2019$text[pbp_2019$game_id == 401104944 & pbp_2019$game_play_number == 89] <- "Crystal Langhorne enters the game for Jewell Loyd"
pbp_2019$text[pbp_2019$game_id == 401104957 & pbp_2019$game_play_number == 97] <- "LaToya Sanders enters the game for Kristi Toliver"
pbp_2019$text[pbp_2019$game_id == 401104962 & pbp_2019$game_play_number == 89] <- "Marie Gulich enters the game for Elizabeth Williams"
pbp_2019$text[pbp_2019$game_id == 401104965 & pbp_2019$game_play_number == 86] <- "Han Xu enters the game for Tina Charles"
pbp_2019$text[pbp_2019$game_id == 401104982 & pbp_2019$game_play_number == 289] <- "Odyssey Sims enters the game for Danielle Robinson"
pbp_2019$text[pbp_2019$game_id == 401104983 & pbp_2019$game_play_number == 290] <- "Kalani Brown enters the game for Riquna Williams"
pbp_2019$text[pbp_2019$game_id == 401104994 & pbp_2019$game_play_number == 95] <- "Natasha Cloud enters the game for Kristi Toliver"
pbp_2019$text[pbp_2019$game_id == 401104999 & pbp_2019$game_play_number == 79] <- "Myisha Hines-Allen enters the game for Kristi Toliver"
pbp_2019$text[pbp_2019$game_id == 401105001 & pbp_2019$game_play_number == 287] <- "Odyssey Sims enters the game for Danielle Robinson"
pbp_2019$text[pbp_2019$game_id == 401105009 & pbp_2019$game_play_number == 95] <- "Kristine Anigwe enters the game for Jonquel Jones"
pbp_2019$text[pbp_2019$game_id == 401105018 & pbp_2019$game_play_number == 286] <- "Morgan Tuck enters the game for Alyssa Thomas" 
pbp_2019$text[pbp_2019$game_id == 401105034 & pbp_2019$game_play_number == 86] <- "Kristine Anigwe enters the game for Jonquel Jones"
pbp_2019$text[pbp_2019$game_id == 401105045 & pbp_2019$game_play_number == 194] <- "Asia Durr enters the game for Bria Hartley"
pbp_2019$text[pbp_2019$game_id == 401105077 & pbp_2019$game_play_number == 101] <- "Riquna Williams enters the game for Chelsea Gray"
pbp_2019$text[pbp_2019$game_id == 401105087 & pbp_2019$game_play_number == 209] <- "Jackie Young enters the game for Dearica Hamby "
pbp_2019$text[pbp_2019$game_id == 401105102 & pbp_2019$game_play_number == 80] <- "Shavonte Zellous enters the game for Jordin Canada"
pbp_2019$text[pbp_2019$game_id == 401165796 & pbp_2019$game_play_number == 104] <- "Kayla McBride enters the game for Epiphanny Prince" 
pbp_2019$text[pbp_2019$game_id == 401165888 & pbp_2019$game_play_number == 83] <- "Tianna Hawkins enters the game for Elena Delle Donne"
pbp_2019$text[pbp_2019$game_id == 401165892 & pbp_2019$game_play_number == 204] <- "Candace Parker enters the game for Chiney Ogwumike"
pbp_2019 <- pbp_2019[-which(pbp_2019$game_id == 401104919 & pbp_2019$game_play_number == 276), ]
pbp_2019 <- pbp_2019[-which(pbp_2019$game_id == 401104924 & pbp_2019$game_play_number == 111), ]
pbp_2019 <- pbp_2019[-which(pbp_2019$game_id == 401104937 & pbp_2019$game_play_number == 109), ]
pbp_2019 <- pbp_2019[-which(pbp_2019$game_id == 401104944 & pbp_2019$game_play_number == 90), ]
pbp_2019 <- pbp_2019[-which(pbp_2019$game_id == 401104982 & pbp_2019$game_play_number == 290), ]
pbp_2019 <- pbp_2019[-which(pbp_2019$game_id == 401104983 & pbp_2019$game_play_number == 291), ]
pbp_2019 <- pbp_2019[-which(pbp_2019$game_id == 401104983 & pbp_2019$game_play_number == 292), ]
pbp_2019 <- pbp_2019[-which(pbp_2019$game_id == 401104994 & pbp_2019$game_play_number == 96), ]
pbp_2019 <- pbp_2019[-which(pbp_2019$game_id == 401104999 & pbp_2019$game_play_number == 80), ]
pbp_2019 <- pbp_2019[-which(pbp_2019$game_id == 401105018 & pbp_2019$game_play_number == 287), ]
pbp_2019 <- pbp_2019[-which(pbp_2019$game_id == 401105001 & pbp_2019$game_play_number == 288), ]
pbp_2019 <- pbp_2019[-which(pbp_2019$game_id == 401105045 & pbp_2019$game_play_number == 192), ]
pbp_2019 <- pbp_2019[-which(pbp_2019$game_id == 401105082 & pbp_2019$game_play_number == 211), ]
pbp_2019 <- pbp_2019[-which(pbp_2019$game_id == 401105082 & pbp_2019$game_play_number == 212), ]
pbp_2019 <- pbp_2019[-which(pbp_2019$game_id == 401105087 & pbp_2019$game_play_number == 207), ]
pbp_2019 <- pbp_2019[-which(pbp_2019$game_id == 401105100 & pbp_2019$game_play_number == 83), ]
pbp_2019 <- pbp_2019[-which(pbp_2019$game_id == 401165888 & pbp_2019$game_play_number == 84), ]
pbp_2019 <- pbp_2019[-which(pbp_2019$game_id == 401165892 & pbp_2019$game_play_number == 205), ]
game_ids <- unique(pbp_2019$game_id)

empty_data_frame <- data.frame(home_points = 0, away_points = 0 , point_diff = 0, Player = "First Row", game_id = "")
# Games 4, 7, 11, 12, 18, 22, 25, 26, 28, 32
counter <- 0
for(i in 1:4){
    counter <- counter + 1
    game_id_string <- paste(game_ids[i])
    pbp <- pbp_2019 %>% filter(game_id == game_ids[i])
    data <- possession_data(game_id_string, pbp) %>% bind_cols(game_id = game_id_string)
    empty_data_frame <- empty_data_frame %>% bind_rows(data)
}

length(unique(empty_data_frame$game_id))
game_ids[18]
401129933

game_ids[11]
game_ids[4]
