## David Teuscher
## Latest changes: 12.05.2021
## This script reads in play by play data for WNBA games and determines that 
## players on the court at the time
########################################################

# Install the wehoop package
# devtools::install_github(repo = "saiemgilani/wehoop")
library(wehoop)
library(tidyverse)

# Read in the play by play data for the Dallas Wings vs. Atlanta Dream on May 24, 2019
data <- espn_wnba_pbp(game_id = "401104913")

# Bring in the box score data and filter for only the starters to get the lineup at the 
# beginning of the game
box_score <- wehoop::espn_wnba_player_box(game_id = "401104913") %>%
  filter(starter == TRUE)

# Take the play by play data and combine it with the starters
# Combine the starters into a single column for the current lineup so it is
# easier to replace players as the game goes on using regular expressions
pbp <- cbind(data, Player = t(box_score$athlete.displayName)) %>% 
  unite(col = "Lineup1", 26:30, sep = ",") %>%
  unite(col = "Lineup2", 21:25, sep = ",")

# Filters the play by play data for only the substitutions 
subs <- pbp %>% filter(type.text == "Substitution") 

# Determine the player that is subbed into the game and subbed out of the game
player_in <- str_extract(subs$text, "^^[A-Z][-a-zA-Z]+ [A-Z][-a-zA-Z]+")
player_out <- str_extract(subs$text, "[A-Z][-a-zA-Z]+ [A-Z][-a-zA-Z]+$")

# Initialize vectors for the different lineups during substitutions
Lineup1 <- numeric(nrow(subs))
Lineup2 <- numeric(nrow(subs))

# Set the first lineup as the starters
Lineup1[1] <- pbp$Lineup1[1]
Lineup2[1] <- pbp$Lineup2[1]

# Loop through all the substitutions in the game and change the lineup depending on 
# whether the substitution is for team 1 or team 2
for(i in 2:(length(Lineup1)+1)){
  if(str_detect(Lineup1[i - 1], player_out[i - 1])){
    Lineup1[i] <- str_replace(Lineup1[i - 1], player_out[i - 1], player_in[i -1])
    Lineup2[i] <- Lineup2[i - 1]
  } else{
    Lineup2[i] <- str_replace(Lineup2[i - 1], player_out[i - 1], player_in[i - 1])
    Lineup1[i] <- Lineup1[i - 1]
  }
}

# This is a data set to test that the substitions are replaced correctly. 
testlineup <- cbind(c("starter", subs$text), Lineup1, Lineup2)
