## David Teuscher
## Latest changes: 19.05.2021
## This script reads in play by play data for WNBA games and determines that 
## players on the court at the time
########################################################

# Install the wehoop package
# devtools::install_github(repo = "saiemgilani/wehoop")
library(wehoop)
library(tidyverse)
library(rvest)
game_info <- read.csv("game_info_2019.csv")

# Read in the play by play data for the Dallas Wings vs. Atlanta Dream on May 24, 2019
#pbp <- espn_wnba_pbp("401105001")
#gameid <- "401105001"
possession_data <- function(gameid, data){
  pbp <- data
  box_score <- wehoop::espn_wnba_player_box(game_id = gameid)
  starters <- box_score %>% filter(starter == TRUE)
  
  # Create empty vectors for the lineups for both teams
  # Length is the same length as the play by play 
  LineupAway <- character(nrow(pbp))
  LineupHome <- character(nrow(pbp))
  
  # Set the starting lineup for both teams; 
  # Away team is always the first 5 players
  # Home team is the second 5 players
  LineupAway[1] <- paste(starters$athlete_display_name[1:5], collapse = ",")
  LineupHome[1] <- paste(starters$athlete_display_name[6:10], collapse = ",")
  
  # Loop through every row on the play by play data 
  for(i in 2:nrow(pbp)){
    # If the play is a substitution, change the lineup
    if(pbp$type_text[i] == "Substitution"){
      filter_pbp <- pbp %>% filter(type_text == "Substitution", clock_display_value == clock_display_value[i], period_display_value == period_display_value[i])
      player_in <- str_trim(str_extract(filter_pbp$text, "^(.*)(?=enters)"))
      player_out <- str_extract(filter_pbp$text, "(?<=for )(.*)$")
      sub_counter <- nrow(filter_pbp)
      if(all(!stringi::stri_isempty(player_in)) & all(!stringi::stri_isempty(player_out))){
        player_in <- str_trim(str_extract(pbp$text[i], "^(.*)(?=enters)"))
        player_out <- str_extract(pbp$text[i], "(?<=for )(.*)$")
        players_out <- player_out
        sub_counter <- 1
        if(is.na(player_in)){
          player_in <- ""
        }
        if(is.na(player_out)){
          player_out <- ""
        }
        
      }
      if((any(player_in == "") | any(player_out == "")) & (length(player_in) > 1 | length(player_out) > 1)){
        game <- game_info %>% filter(game_id == gameid)
        url <- paste0("https://www.basketball-reference.com/wnba/boxscores/pbp/", game$game_day, "0",game$bref_home, ".html")
        table <- url %>% read_html() %>% html_table()
        table[[1]] <- table[[1]][-1,]
        table <- url %>% read_html() %>% html_table()
        colnames(table[[1]]) <- table[[1]][1,]
        bref_pbp <- table[[1]]
        colnames(bref_pbp) <- c("Time", "Away", "Away_Points", "Score", "Home_Points", "Home")
        possible_vals <- bref_pbp %>% filter(Time == ifelse(str_detect(pbp$clock_display_value[i], ":"), paste0(pbp$clock_display_value[i], ".0"), ifelse(nchar(str_extract(pbp$clock_display_value[i], "^(.*)(?=.)")) == 3, paste0("0:", str_extract(pbp$clock_display_value[i], "^(.*)(?=.)"), "0"), paste0("0:0", str_extract(pbp$clock_display_value[i], "^(.*)(?=.)"), "0"))))
        subs <- possible_vals[str_detect(possible_vals$Home, "enters the game") | str_detect(possible_vals$Away, "enters the game"), ]
        Awaysubs <- subs$Away[subs$Away != ""]
        Homesubs <- subs$Home[subs$Home != ""]
        LineupAway[i] <- LineupAway[i-1]
        LineupHome[i] <- LineupHome[i-1]
        players_out_away <- character(length(Awaysubs))
        if(!identical(Awaysubs, character(0))){
          players_out_away <- str_trim(str_extract(Awaysubs, "(?<=for )(.*)$"))
          players_in_away <- str_trim(str_extract(Awaysubs, "^(.*)(?=enters)"))
          for(s in 1:length(Awaysubs)){
            players_in_away[s] <- box_score$athlete_display_name[str_detect(box_score$athlete_short_name, players_in_away[s])]
            players_out_away[s] <- box_score$athlete_display_name[str_detect(box_score$athlete_short_name, players_out_away[s])]
            if(str_detect(LineupAway[i], players_out_away[s])){
              LineupAway[i] <- str_replace(LineupAway[i], players_out_away[s], players_in_away[s])
              LineupHome[i] <- LineupHome[i]
            } else{
              LineupHome[i] <- str_replace(LineupHome[i], players_out_away[s], players_in_away[s])
              LineupAway[i] <- LineupAway[i]
            }
          }
        }
        players_out_home <- character(length(Homesubs))
        if(!identical(Homesubs, character(0))){
          players_out_home <- str_trim(str_extract(Homesubs, "(?<=for)(.*)$"))
          players_in_home <- str_trim(str_extract(Homesubs, "^(.*)(?=enters)"))
         for(s in 1:length(Homesubs)){
           players_in_home[s] <- box_score$athlete_display_name[str_detect(box_score$athlete_short_name, players_in_home[s])]
           players_out_home[s] <- box_score$athlete_display_name[str_detect(box_score$athlete_short_name, players_out_home[s])]
           if(str_detect(LineupAway[i], players_out_home[s])){
             LineupAway[i] <- str_replace(LineupAway[i], players_out_home[s], players_in_home[s])
             LineupHome[i] <- LineupHome[i]
           } else{
             LineupHome[i] <- str_replace(LineupHome[i], players_out_home[s], players_in_home[s])
             LineupAway[i] <- LineupAway[i]
           }
         }
        }
        players_out <- c(players_out_away, players_out_home) 
      } else {
      # Determine the player coming in and the player coming out of the game
        if(str_detect(LineupHome[i-1], player_out) | str_detect(LineupAway[i-1], player_out)){
          if(player_in == "" || player_out == ""){
            game <- game_info %>% filter(game_id == gameid)
            url <- paste0("https://www.basketball-reference.com/wnba/boxscores/pbp/", game$game_day, "0",game$bref_home, ".html")
            table <- url %>% read_html() %>% html_table()
            table[[1]] <- table[[1]][-1,]
            table <- url %>% read_html() %>% html_table()
            colnames(table[[1]]) <- table[[1]][1,]
            bref_pbp <- table[[1]]
            colnames(bref_pbp) <- c("Time", "Away", "Away_Points", "Score", "Home_Points", "Home")
            possible_vals <- bref_pbp %>% filter(Time == paste0(pbp$clock_display_value[i], ".0"))
            if(player_out == ""){
              first_inital <- str_extract(player_in, "^[A-Z]")
              last_name <- str_extract(player_in, "[A-Za-z]+$")
              player_name <- paste0(first_inital, ". ", last_name)
            } else {
              first_inital <- str_extract(player_out, "^[A-Z]")
              last_name <- str_extract(player_out, "[A-Za-z]+$") 
              player_name <- paste0(first_inital, ". ", last_name)
            }
            if(sum(str_detect(possible_vals$Away, player_name)) == 1){
              sub <- possible_vals$Away[str_detect(possible_vals$Away, player_name)]
              if(length(sub) > 1){
                sub_play <- str_detect(sub, "enters the game")
                sub <- sub[sub_play]
              }
              if(player_out == ""){
                player <- str_extract(sub, "(?<=for)(.*)$")
              } else {
                player <- str_extract(sub, "^(.*)(?= enters)")
              }
              if(!identical(player, character(0)) && is.na(player)){
                player <- ""
              }
            } else {
              sub <- possible_vals$Home[str_detect(possible_vals$Home, player_name)]
              if(length(sub) > 1){
                sub_play <- str_detect(sub, "enters the game")
                sub <- sub[sub_play]
              }
              if(player_out == ""){
                player <- str_extract(sub, "(?<=for)(.*)$")
              } else {
                player <- str_extract(sub, "^(.*)(?= enters)")
              }
              player <- str_extract(sub, "[A-Z.]+ [A-Za-z]+$")
              if(!identical(player, character(0)) && is.na(player)){
                player <- ""
              }
            }
            if(identical(player, character(0))){
              player <- ""
              player_sub <- player
            } else {
              player_sub <- box_score$athlete_display_name[str_detect(box_score$athlete_short_name, player)]
            }
            if(player_in == ""){
              player_in <- player_sub
              players_out <- player_out
            } else{
              player_out <- player_sub
              players_out <- player_sub
            }
          }
        }
      }
      
      if(any(str_detect(LineupAway[i-1], players_out)) | any(str_detect(LineupHome[i-1], players_out))){
      # If the player going out is on the away team, substitute the player in on the 
      # away team. If they aren't on the away team, then sub them for the home team
        if(sub_counter == 1){
          if(player_in != "" && player_out != ""){
            if(str_detect(LineupAway[i -1], player_out)){
              LineupAway[i] <- str_replace(LineupAway[i-1], player_out, player_in)
              LineupHome[i] <- LineupHome[i-1]
            } else{
              LineupHome[i] <- str_replace(LineupHome[i-1], player_out, player_in)
              LineupAway[i] <- LineupAway[i-1]
            }
          } else{
            LineupAway[i] <- LineupAway[i-1]
            LineupHome[i] <- LineupHome[i-1]
          }  
        }
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
  #View(test %>% dplyr::select(text, LineupAway, LineupHome, clock_display_value))
  # Select variables that are needed to pull out possession information
  possession <- test %>% dplyr::select(shooting_play, home_score, scoring_play, away_score,
                                text, score_value, team_id, type_text, LineupAway,
                                LineupHome, clock_display_value)
  
  turnover_types <- c("Out of Bounds - Lost Ball Turnover", "Offensive Foul Turnover", "Shot Clock Turnover", "Bad Pass\nTurnover",
                      "Lost Ball Turnover", "Out of Bounds - Bad Pass Turnover", "Traveling")
  
  change_possession <- numeric(nrow(possession))
  cond <- (possession$shooting_play == TRUE & possession$scoring_play == TRUE) | 
    possession$type_text == "Defensive Rebound" | possession$type_text %in% turnover_types
  
  free_throws <- c("Free Throw - 1 of 2", "Free Throw - 1 of 3", "Free Throw - 2 of 3", "Free Throw - Technical")
  
  for(i in 2:nrow(possession)){
    if(cond[i] == TRUE){
      change_possession[i] <- 1
    }
    if(possession$type_text[i] == "Shooting Foul" && (possession$clock_display_value[i] == possession$clock_display_value[i-1])){
      change_possession[i-1] <- 0
    }
    if((possession$type_text[i] == "Free Throw - 1 of 1" || 
        possession$type_text[i] == "Free Throw - 2 of 2" || 
        possession$type_text[i] == "Free Throw - 3 of 3") && 
       (possession$type_text[i-1] == "Substitution")){
      change_possession[i] <- 0
      change_possession[i-1] <- 1
    }
    if(possession$type_text[i] %in% free_throws){
      change_possession[i] <- 0
    }
    
  }
  
  possession <- possession %>% mutate(change_possession = change_possession)

  
  point_diff <- numeric(sum(possession$change_possession == 1) + 1)
  LineupAway <- numeric(length(point_diff))
  LineupHome <- numeric(length(point_diff))
  home_points <- numeric(length(point_diff))
  away_points <- numeric(length(point_diff))
  LineupAway[1] <- paste(box_score$athlete_display_name[1:5], collapse = ",")
  LineupHome[1] <- paste(box_score$athlete_display_name[6:10], collapse = ",")
  home_points[1] <- 0
  away_points[1] <- 0
  k <- 2
  for(i in 1:nrow(possession)){
    if(possession$change_possession[i] == 1){
      home_points[k] <-  possession$home_score[i]
      away_points[k] <- possession$away_score[i]
      LineupAway[k] <- possession$LineupAway[i]
      LineupHome[k] <- possession$LineupHome[i]
      point_diff[k] <- (home_points[k] - home_points[k-1]) - (away_points[k] - away_points[k-1])
      k <- k + 1
    }
  }
  another <- data.frame(home_points, away_points, point_diff, LineupAway, LineupHome)
  test2 <- another %>% 
    separate(LineupAway, into = c("P1", "P2", "P3", "P4", "P5"), sep = ",") %>%
    separate(LineupHome, into = c("P6", "P7", "P8", "P9", "P10"), sep = ",") %>% 
    pivot_longer(cols = P1:P10, values_to = "Player", names_to =  NULL) %>%
    mutate(Player = factor(Player))
  return(test2)
}

# # Bring in the box score data and filter for only the starters to get the lineup at the 
# # beginning of the game
# box_score <- wehoop::espn_wnba_player_box(game_id = "401104913") %>%
#   filter(starter == TRUE)
# 
# # Create empty vectors for the lineups for both teams
# # Length is the same length as the play by play 
# LineupAway <- character(nrow(pbp))
# LineupHome <- character(nrow(pbp))
# 
# # Set the starting lineup for both teams; 
# # Away team is always the first 5 players
# # Home team is the second 5 players
# LineupAway[1] <- paste(box_score$athlete_display_name[1:5], collapse = ",")
# LineupHome[1] <- paste(box_score$athlete_display_name[6:10], collapse = ",")
# 
# # Loop through every row on the play by play data 
# for(i in 2:nrow(pbp)){
#   # If the play is a substitution, change the lineup
#   if(pbp$type_text[i] == "Substitution"){
#     # Determine the player coming in and the player coming out of the game
#     player_in <- str_extract(pbp$text[i], "^^[A-Z][-a-zA-Z]+ [A-Z][-a-zA-Z]+")
#     player_out <- str_extract(pbp$text[i], "[A-Z][-a-zA-Z]+ [A-Z][-a-zA-Z]+$")
#     
#     # If the player going out is on the away team, substitute the player in on the 
#     # away team. If they aren't on the away team, then sub them for the home team
#     if(str_detect(LineupAway[i -1], player_out)){
#       LineupAway[i] <- str_replace(LineupAway[i-1], player_out, player_in)
#       LineupHome[i] <- LineupHome[i-1]
#     } else{
#       LineupHome[i] <- str_replace(LineupHome[i-1], player_out, player_in)
#       LineupAway[i] <- LineupAway[i-1]
#     }
#   }
#   
#   # If it isn't a substitution, keep the lineup the same as the previous play
#   else{
#     LineupAway[i] <- LineupAway[i-1]
#     LineupHome[i] <- LineupHome[i-1]
#   }
# }
# 
# # Combine the lineup with the play by play data
# test <- pbp %>% bind_cols(LineupAway = LineupAway, LineupHome = LineupHome)
# 
# # Select variables that are needed to pull out possession information
# possession <- test %>% select(shooting_play, home_score, scoring_play, away_score,
#                              text, score_value, team_id, type_text, LineupAway,
#                              LineupHome, clock_display_value)
# 
# turnover_types <- c("Out of Bounds - Lost Ball Turnover", "Offensive Foul Turnover", "Shot Clock Turnover", "Bad Pass\nTurnover",
#                     "Lost Ball Turnover", "Out of Bounds - Bad Pass Turnover", "Traveling")
# 
# change_possession <- numeric(nrow(possession))
# cond <- (possession$shooting_play == TRUE & possession$scoring_play == TRUE) | 
#   possession$type_text == "Defensive Rebound" | possession$type_text %in% turnover_types
# 
# free_throws <- c("Free Throw - 1 of 2", "Free Throw - 1 of 3", "Free Throw - 2 of 3", "Free Throw - Technical")
# 
# for(i in 2:nrow(possession)){
#   if(cond[i] == TRUE){
#     change_possession[i] <- 1
#   }
#   if(possession$type_text[i] == "Shooting Foul" && (possession$clock_display_value[i] == possession$clock_display_value[i-1])){
#     change_possession[i-1] <- 0
#   }
#   if((possession$type_text[i] == "Free Throw - 1 of 1" || 
#      possession$type_text[i] == "Free Throw - 2 of 2" || 
#      possession$type_text[i] == "Free Throw - 3 of 3") && 
#      (possession$type_text[i-1] == "Substitution")){
#     change_possession[i] <- 0
#     change_possession[i-1] <- 1
#   }
#   if(possession$type_text[i] %in% free_throws){
#     change_possession[i] <- 0
#   }
#   
# }
# 
# possession <- possession %>% mutate(change_possession = change_possession)
# 
# point_diff <- numeric(sum(possession$change_possession == 1) + 1)
# LineupAway <- numeric(length(point_diff))
# LineupHome <- numeric(length(point_diff))
# home_points <- numeric(length(point_diff))
# away_points <- numeric(length(point_diff))
# LineupAway[1] <- paste(box_score$athlete_display_name[1:5], collapse = ",")
# LineupHome[1] <- paste(box_score$athlete_display_name[6:10], collapse = ",")
# home_points[1] <- 0
# away_points[1] <- 0
# k <- 2
# for(i in 1:nrow(possession)){
#   if(possession$change_possession[i] == 1){
#     home_points[k] <-  possession$home_score[i]
#     away_points[k] <- possession$away_score[i]
#     LineupAway[k] <- possession$LineupAway[i]
#     LineupHome[k] <- possession$LineupHome[i]
#     point_diff[k] <- (home_points[k] - home_points[k-1]) - (away_points[k] - away_points[k-1])
#     k <- k + 1
#   }
# }
# another <- data.frame(home_points, away_points, point_diff, LineupAway, LineupHome)
# test2 <- another %>% 
#   separate(LineupAway, into = c("P1", "P2", "P3", "P4", "P5"), sep = ",") %>%
#   separate(LineupHome, into = c("P6", "P7", "P8", "P9", "P10"), sep = ",") %>% 
#   pivot_longer(cols = P1:P10, values_to = "Player", names_to =  NULL) %>%
#   mutate(Player = factor(Player))
# 
# X <- model.matrix(point_diff ~ -1 + Player, data = test2)
# ids <- seq(10, nrow(X), by = 10)
# X_small <- matrix(0, nrow = nrow(X)/10, ncol = ncol(X))
# colnames(X_small) <- colnames(X)
# for(i in ids){
#   k <- i/10
#   for(j in 1:ncol(X)){
#     X_small[k, j] <- sum(X[(i-9):i, j]) 
#   }
# }
# 
# X_small

# # Potential code to fix stuff
# url <- "https://www.basketball-reference.com/wnba/boxscores/pbp/201905250CON.html"
# 
# table <- url %>% read_html() %>% html_table()
# table[[1]] <- table[[1]][-1,]
# table <- url %>% read_html() %>% html_table()
# colnames(table[[1]]) <- table[[1]][1,]
# bref_pbp <- table[[1]]
# colnames(bref_pbp) <- c("Time", "Away", "Away_Points", "Score", "Home_Points", "Home")
# possible_vals <- bref_pbp %>% filter(Time == paste0(pbp$clock_display_value[i], ".0"))
# str_detect(possible_vals$Away, last_name)
# last_name <- str_extract(player_in, "[A-Za-z]+$")
# str_detect(possible_vals$Away, last_name)
# str_detect(possible_vals$Home, last_name)
# sub <- possible_vals$Home[str_detect(possible_vals$Home, last_name)]
# player <- str_extract(sub, "[A-Z.]+ [A-Za-z]+$")
# 
# box_score
# str_detect(box_score$athlete_short_name, player)
# box_score$athlete_display_name[str_detect(box_score$athlete_short_name, player)]
# 
# test_url <- paste0("https://www.basketball-reference.com/wnba/boxscores/pbp/", game$game_day, "0",game$bref_home, ".html") 
# table <- test_url %>% read_html() %>% html_table()
# table
