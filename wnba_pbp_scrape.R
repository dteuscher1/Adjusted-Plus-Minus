## David Teuscher
## Latest changes: 14.05.2021
## This script reads in play by play data for WNBA games and determines that 
## players on the court at the time
########################################################

# Install the wehoop package
# devtools::install_github(repo = "saiemgilani/wehoop")
library(wehoop)
library(tidyverse)

# Read in the play by play data for the Dallas Wings vs. Atlanta Dream on May 24, 2019
pbp <- espn_wnba_pbp(game_id = "401104913")

# Bring in the box score data and filter for only the starters to get the lineup at the 
# beginning of the game
box_score <- wehoop::espn_wnba_player_box(game_id = "401104913") %>%
  filter(starter == TRUE)

# Create empty vectors for the lineups for both teams
# Length is the same length as the play by play 
LineupAway <- character(nrow(pbp))
LineupHome <- character(nrow(pbp))

# Set the starting lineup for both teams; 
# Away team is always the first 5 players
# Home team is the second 5 players
LineupAway[1] <- paste(box_score$athlete_display_name[1:5], collapse = ",")
LineupHome[1] <- paste(box_score$athlete_display_name[6:10], collapse = ",")

# Loop through every row on the play by play data 
for(i in 2:nrow(pbp)){
  # If the play is a substitution, change the lineup
  if(pbp$type_text[i] == "Substitution"){
    # Determine the player coming in and the player coming out of the game
    player_in <- str_extract(pbp$text[i], "^^[A-Z][-a-zA-Z]+ [A-Z][-a-zA-Z]+")
    player_out <- str_extract(pbp$text[i], "[A-Z][-a-zA-Z]+ [A-Z][-a-zA-Z]+$")
    
    # If the player going out is on the away team, substitute the player in on the 
    # away team. If they aren't on the away team, then sub them for the home team
    if(str_detect(LineupAway[i -1], player_out)){
      LineupAway[i] <- str_replace(LineupAway[i-1], player_out, player_in)
      LineupHome[i] <- LineupHome[i-1]
    } else{
      LineupHome[i] <- str_replace(LineupHome[i-1], player_out, player_in)
      LineupAway[i] <- LineupAway[i-1]
    }
  }
  
  # If it isn't a substitution, keep the lineup the same as the previous play
  else{
    LineupAway[i] <- LineupAway[i-1]
    LineupHome[i] <- LineupHome[i-1]
  }
}

# Combine the lineup with the play by play data
test <- pbp %>% bind_cols(LineupAway = LineupAway, LineupHome = LineupHome)

unique(test$type_text)
head(test)
View(test)
# Select variables that are needed to pull out possession information
possesion <- test %>% select(shooting_play, home_score, scoring_play, away_score,
                             text, score_value, team_id, type_text, LineupAway,
                             LineupHome)

small <- possesion[1:20, ] %>% 
  mutate(change_of_possession = ifelse((shooting_play == TRUE & scoring_play == TRUE) | type_text == "Defensive Rebound", 1, 0))
View(small)
