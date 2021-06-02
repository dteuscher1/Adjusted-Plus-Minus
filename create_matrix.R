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
pbp_2019$text[pbp_2019$game_id == 401104923 & pbp_2019$game_play_number == 303] <- "Chiney Ogwumike enters the game for Nneka Ogwumike"
pbp_2019$text[pbp_2019$game_id == 401104934 & pbp_2019$game_play_number == 171] <- "Alanna Smith enters the game for Leilani Mitchell"
pbp_2019$text[pbp_2019$game_id == 401104973 & pbp_2019$game_play_number == 122] <- "Natasha Cloud enters the game for Aerial Powers"
pbp_2019$text[pbp_2019$game_id == 401104975 & pbp_2019$game_play_number == 52] <- "Chiney Ogwumike enters the game for Candace Parker"
pbp_2019$text[pbp_2019$game_id == 401104983 & pbp_2019$game_play_number == 250] <- "Victoria Macaulay enters the game for Stefanie Dolson"
pbp_2019$text[pbp_2019$game_id == 401104985 & pbp_2019$game_play_number == 303] <- "Kahleah Copper enters the game for Allie Quigley"
pbp_2019$text[pbp_2019$game_id == 401104994 & pbp_2019$game_play_number == 60] <- "Myisha Hines-Allen enters the game for LaToya Sanders"
pbp_2019$text[pbp_2019$game_id == 401105002 & pbp_2019$game_play_number == 37] <- "Teaira McCowan enters the game for Candice Dupree"
pbp_2019$text[pbp_2019$game_id == 401105024 & pbp_2019$game_play_number == 193] <- "Sugar Rodgers enters the game for Jackie Young"
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
pbp_2019 <- pbp_2019[-which(pbp_2019$game_id == 401104923 & pbp_2019$game_play_number == 300), ]
pbp_2019 <- pbp_2019[-which(pbp_2019$game_id == 401104934 & pbp_2019$game_play_number == 172), ]
pbp_2019 <- pbp_2019[-which(pbp_2019$game_id == 401104973 & pbp_2019$game_play_number == 120), ]
pbp_2019 <- pbp_2019[-which(pbp_2019$game_id == 401104985 & pbp_2019$game_play_number == 301), ]
pbp_2019 <- pbp_2019[-which(pbp_2019$game_id == 401104994 & pbp_2019$game_play_number == 61), ]
pbp_2019 <- pbp_2019[-which(pbp_2019$game_id == 401105002 & pbp_2019$game_play_number == 38), ]
pbp_2019 <- pbp_2019[-which(pbp_2019$game_id == 401105010 & pbp_2019$game_play_number == 180), ]
pbp_2019 <- pbp_2019[-which(pbp_2019$game_id == 401105024 & pbp_2019$game_play_number == 190), ]
text_cols <- c('Odyssey Sims enters the game for Lexie Brown', "Stephanie Talbot enters the game for Napheesa Collier")
clock_display_value_cols <- c("6:47", "6:47")
shooting_play_cols <- c(FALSE, FALSE)
score_value_cols <- c(0, 0)
season_cols <- c(2019, 2019)
home_score_cols <- c(15, 15)
away_score_cols <- c(25, 25)
scoring_play_cols <- c(FALSE, FALSE)
game_id_cols <- c(401105001, 401105001)
type_text_cols <- c("Substitution", "Substitution")

pbp_2019 <- pbp_2019 %>% add_row(text = text_cols, clock_display_value = clock_display_value_cols, shooting_play = shooting_play_cols,
                                score_value = score_value_cols, season = season_cols, home_score = home_score_cols,
                                away_score = away_score_cols, scoring_play = scoring_play_cols, game_id = game_id_cols,
                                type_text = type_text_cols, .after = which(pbp_2019$game_id == "401105001" & pbp_2019$game_play_number == 127))
View(pbp_2019 %>% filter(game_id == 401105024) %>% dplyr::select(text, game_play_number, clock_display_value))
#pbp <- pbp_2019 %>% filter(game_id == 401105010)
game_ids <- unique(pbp_2019$game_id)

empty_data_frame <- data.frame(home_points = 0, away_points = 0 , point_diff = 0, Player = "First Row", game_id = "")
# Games 11, 38, 82, 105, 115, 173, 204, 215, 216
#games_to_use <- game_ids[-c(38, 56, 115, 173, 204, 215, 216)]
games_to_use <- game_ids
counter <- 1
# games 22*, 54*, 61*, 63*, 71*, 73*, 81*, 82*, 88*, 89*, 97*, 111*, 151, 156, 174, 185, 191, 204, 214, 215
for(i in 151){
    counter <- counter + 1
    game_id_string <- paste(games_to_use[i])
    pbp <- pbp_2019 %>% filter(game_id == games_to_use[i])
    data <- possession_data(game_id_string, pbp) %>% bind_cols(game_id = game_id_string)
    empty_data_frame <- empty_data_frame %>% bind_rows(data)
}
head(empty_data_frame)
empty_data_frame <- empty_data_frame[-1,]
empty_data_frame <- empty_data_frame %>% mutate(Player = factor(Player))
X <- model.matrix(point_diff ~ -1 + Player, data = empty_data_frame)
ids <- seq(10, nrow(X), by = 10)
X_small <- matrix(0, nrow = nrow(X)/10, ncol = ncol(X))
colnames(X_small) <- colnames(X)
for(i in ids){
    k <- i/10
    for(j in 1:ncol(X)){
        X_small[k, j] <- sum(X[(i-9):i, j]) 
    }
}


points <- seq(1, nrow(empty_data_frame), by = 10)
y <- empty_data_frame$point_diff[points]
point_diff_2019 <- data.frame(X_small, point_diff = y)
write_csv(point_diff_2019, "point_diff_2019.csv")

glimpse(point_diff_2019)
