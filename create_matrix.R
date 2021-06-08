## David Teuscher
## Latest changes: 07.06.2021
## This script creates the data used to fit adjusted plus minus and 
## regularized adjusted plus minus
########################################################

# Load functions to parse the data and clean the data
source("wnba_pbp_scrape.R")
source("clean_data.R")
pbp_2019 <- clean_data(2019)
# Extract the unique game ids for 2019
game_ids <- unique(pbp_2019$game_id)

# Create an empty data frame to hold data
empty_data_frame <- data.frame(home_points = numeric(), away_points = numeric() , point_diff = numeric(), home_possession = numeric(), Player = character(), game_id = character())
# Remove the All-star game
games_to_use <- game_ids[c(-204)]

# Loop through each game and parse the data and append it to the data frame
for(i in 1:length(games_to_use)){
    game_id_string <- paste(games_to_use[i])
    pbp <- pbp_2019 %>% filter(game_id == games_to_use[i])
    data <- possession_data(game_id_string, pbp) %>% bind_cols(game_id = game_id_string)
    empty_data_frame <- empty_data_frame %>% bind_rows(data)
}

# Make player a factor
empty_data_frame <- empty_data_frame %>% mutate(Player = factor(Player))
# Create an X matrix for each player
X <- model.matrix(point_diff ~ -1 + Player, data = empty_data_frame)

# Since the matrix makes 10 rows for each lineup, the matrix needs to be compressed by 
# combining each 10 rows
ids <- seq(10, nrow(X), by = 10)
X_small <- matrix(0, nrow = nrow(X)/10, ncol = ncol(X))
colnames(X_small) <- colnames(X)
for(i in ids){
    k <- i/10
    for(j in 1:ncol(X)){
        X_small[k, j] <- sum(X[(i-9):i, j]) 
    }
}

# Extract the point differential and team possession for each possession
points <- seq(1, nrow(empty_data_frame), by = 10)
y <- empty_data_frame$point_diff[points]
home_possession <- empty_data_frame$home_possession[points]
game_id <- empty_data_frame$game_id[points]

# Create a data frame and save the data 
point_diff_2019 <- data.frame(game_id, home_possession, X_small, point_diff = y)
write_csv(point_diff_2019, "point_diff_2019_updated.csv")

