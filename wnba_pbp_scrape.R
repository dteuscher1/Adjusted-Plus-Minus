## David Teuscher
## Latest changes: 08.06.2021
## This script creates a function that takes the play by play data for 
## a game and determines the players on the court and when possessions change
########################################################

# Install the wehoop package
# devtools::install_github(repo = "saiemgilani/wehoop")
# Required packages
library(wehoop)
library(tidyverse)
library(rvest)
# Read in the game info file
game_info <- read.csv("game_info_2019.csv")

# A single game game be examined using this code
#pbp <- espn_wnba_pbp("401104913")
#gameid <- "401104913"

# Function to parse the play by play data into each possession
# Inputs: Game id for the game from ESPN; If the game_info file is scraped correctly, the
# game id can be extracted from there
# Data: play by play data for the game associated with the game id
possession_data <- function(gameid, data){
  pbp <- data
  # Pull out the box score information to get full player names
  box_score <- wehoop::espn_wnba_player_box(game_id = gameid)
  # Extract the starters from the box score data
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
  
  # A few players have the full name with a hyphen, but the play by play data only has
  # their first name included, so this are changed to remove the hyphenated portion of the name
  if(str_detect(LineupHome[1], "Brittany Boyd-Jones")){
    LineupHome[1] <- str_replace(LineupHome[1], "Brittany Boyd-Jones", "Brittany Boyd")
  } else if(str_detect(LineupAway[1], "Brittany Boyd-Jones")){
    LineupAway[1] <- str_replace(LineupAway[1], "Brittany Boyd-Jones", "Brittany Boyd")
  }
  if(str_detect(LineupHome[1], "Astou Ndour-Fall")){
    LineupHome[1] <- str_replace(LineupHome[1], "Astou Ndour-Fall", "Astou Ndour")
  } else if(str_detect(LineupAway[1], "Astou Ndour-Fall")){
    LineupAway[1] <- str_replace(LineupAway[1], "Astou Ndour-Fall", "Astou Ndour")
  }
  
  # Loop through every row on the play by play data 
  for(i in 2:nrow(pbp)){
    # If the play is a substitution, change the lineup
    if(pbp$type_text[i] == "Substitution"){
      # Determine how many substitutions occur at the time
      filter_pbp <- pbp %>% filter(type_text == "Substitution", clock_display_value == clock_display_value[i], period_display_value == period_display_value[i])
      # Determine the number of players in and out
      player_in <- str_trim(str_extract(filter_pbp$text, "^(.*)(?=enters)"))
      player_out <- str_extract(filter_pbp$text, "(?<=for )(.*)$")
      # Set the number of substitutions to occur
      sub_counter <- nrow(filter_pbp)
      
      # If none of the substitutions have a missing player, then do all of the substitutions one
      # by one
      if(all(!stringi::stri_isempty(player_in)) & all(!stringi::stri_isempty(player_out))){
        # Extract the player in and the player out
        player_in <- str_trim(str_extract(pbp$text[i], "^(.*)(?=enters)"))
        player_out <- str_extract(pbp$text[i], "(?<=for )(.*)$")
        
        # There is occassionally a typo where Natasha is spelled Natisha, so 
        # this corrects that if it occurs
        if(str_detect(player_in, "Natisha Hiedeman")){
          player_in <- "Natasha Hiedeman"
        } else if(str_detect(player_out, "Natisha Hiedeman")){
          player_out <- "Natasha Hiedeman"
        }
        
        # Set the substitute counter to 1
        players_out <- player_out
        sub_counter <- 1
        
        # If there is an NA value, it sets the player to a missing string 
        # (This may be able to be removed based on other changes to the code)
        if(is.na(player_in)){
          player_in <- ""
        }
        if(is.na(player_out)){
          player_out <- ""
        }
        
      }
      # If any of the players are missing and there are more than one substitutions occurred at the time
      # then go to Basketball Reference and pull out the substitution
      if((any(player_in == "") | any(player_out == "")) & (length(player_in) > 1 | length(player_out) > 1)){
        # Filter the game info for the information about the specific game
        game <- game_info %>% filter(game_id == gameid)
        # Create the needed url for basketball reference play by play data
        url <- paste0("https://www.basketball-reference.com/wnba/boxscores/pbp/", game$game_day, "0",game$bref_home, ".html")
        
        # Extract the play by play data from the url
        table <- url %>% read_html() %>% html_table()
        table[[1]] <- table[[1]][-1,]
        table <- url %>% read_html() %>% html_table()
        colnames(table[[1]]) <- table[[1]][1,]
        bref_pbp <- table[[1]]
        # Rename the columns of the play by play data
        colnames(bref_pbp) <- c("Time", "Away", "Away_Points", "Score", "Home_Points", "Home")
        # Filter the play by play data for plays that occurred at the time of the substitution
        possible_vals <- bref_pbp %>% filter(Time == ifelse(str_detect(pbp$clock_display_value[i], ":"), paste0(pbp$clock_display_value[i], ".0"), ifelse(nchar(str_extract(pbp$clock_display_value[i], "^(.*)(?=.)")) == 3, paste0("0:", str_extract(pbp$clock_display_value[i], "^(.*)(?=.)"), "0"), paste0("0:0", str_extract(pbp$clock_display_value[i], "^(.*)(?=.)"), "0"))))
        # Take the plays at the time that are substitutions
        subs <- possible_vals[str_detect(possible_vals$Home, "enters the game") | str_detect(possible_vals$Away, "enters the game"), ]
        # Extract the home and away substitutions
        Awaysubs <- subs$Away[subs$Away != ""]
        Homesubs <- subs$Home[subs$Home != ""]
        LineupAway[i] <- LineupAway[i-1]
        LineupHome[i] <- LineupHome[i-1]
        
        # Create an empty vector for the players going out 
        players_out_away <- character(length(Awaysubs))
        
        # If there are any away substitutions determine who they are
        if(!identical(Awaysubs, character(0))){
          # Extract players coming in and coming out
          players_out_away <- str_trim(str_extract(Awaysubs, "(?<=for )(.*)$"))
          players_in_away <- str_trim(str_extract(Awaysubs, "^(.*)(?=enters)"))
          
          # Pull out the first and last name for the player 
          # (Basketball Reference lists players as first initial last name i.e. D. Teuscher)
          for(s in 1:length(Awaysubs)){
            # From the box score data, the first and last name is extracted 
            players_in_away[s] <- box_score$athlete_display_name[str_detect(box_score$athlete_short_name, players_in_away[s])]
            players_out_away[s] <- box_score$athlete_display_name[str_detect(box_score$athlete_short_name, players_out_away[s])]
            
            # Fixing a few of the names that throw errors
            if(str_detect(players_out_away[s], "Brittany Boyd-Jones")){
              players_out_away[s] <- str_replace(players_out_away[s], "Brittany Boyd-Jones", "Brittany Boyd")
            }
            if(str_detect(players_in_away[s], "Brittany Boyd-Jones")){
              players_in_away[s] <- str_replace(players_in_away[s], "Brittany Boyd-Jones", "Brittany Boyd")
            }
            if(str_detect(players_out_away[s], "Astou Ndour-Fall")){
              players_out_away[s] <- str_replace(players_out_away[s], "Astou Ndour-Fall", "Astou Ndour")
            }
            if(str_detect(players_in_away[s], "Astou Ndour-Fall")){
              players_in_away[s] <- str_replace(players_in_away[s], "Astou Ndour-Fall", "Astou Ndour")
            }
            if(str_detect(players_out_away[s], "Natisha Hiedeman")){
              players_out_away[s] <- str_replace(players_out_away[s], "Natisha Hiedeman", "Natasha Hiedeman")
            }
            if(str_detect(players_in_away[s], "Natisha Hiedeman")){
              players_in_away[s] <- str_replace(players_in_away[s], "Natisha Hiedeman", "Natasha Hiedeman")
            }
            
            # Replacing players in the away lineup (Might need to adust some of this)
            if(str_detect(LineupAway[i], players_out_away[s])){
              LineupAway[i] <- str_replace(LineupAway[i], players_out_away[s], players_in_away[s])
              LineupHome[i] <- LineupHome[i]
            } else{
              LineupHome[i] <- str_replace(LineupHome[i], players_out_away[s], players_in_away[s])
              LineupAway[i] <- LineupAway[i]
            }
          }
        }
        
        # Create empty vector for home substitutions
        players_out_home <- character(length(Homesubs))
        
        # If there are substitutions for the home team, figure out who they are
        if(!identical(Homesubs, character(0))){
          # Extract the players coming in and out
          players_out_home <- str_trim(str_extract(Homesubs, "(?<= for)(.*)$"))
          players_in_home <- str_trim(str_extract(Homesubs, "^(.*)(?=enters)"))
          
          # Extract the first and last name for the play
         for(s in 1:length(Homesubs)){
           players_in_home[s] <- box_score$athlete_display_name[str_detect(box_score$athlete_short_name, players_in_home[s])]
           players_out_home[s] <- box_score$athlete_display_name[str_detect(box_score$athlete_short_name, players_out_home[s])]
           
           # Fix names that cause errors
           if(str_detect(players_out_home[s], "Brittany Boyd-Jones")){
             players_out_home[s] <- str_replace(players_out_home[s], "Brittany Boyd-Jones", "Brittany Boyd")
           }
           if(str_detect(players_in_home[s], "Brittany Boyd-Jones")){
             players_in_home[s] <- str_replace(players_out_home[s], "Brittany Boyd-Jones", "Brittany Boyd")
           }
           if(str_detect(players_out_home[s], "Astou Ndour-Fall")){
             players_out_home[s] <- str_replace(players_out_home[s], "Astou Ndour-Fall", "Astou Ndour")
           }
           if(str_detect(players_in_home[s], "Astou Ndour-Fall")){
             players_in_home[s] <- str_replace(players_out_home[s], "Astou Ndour-Fall", "Astou Ndour")
           }
           if(str_detect(players_out_home[s], "Natisha Hiedeman")){
             players_out_home[s] <- str_replace(players_out_home[s], "Natisha Hiedeman", "Natasha Hiedeman")
           }
           if(str_detect(players_in_home[s], "Natisha Hiedeman")){
             players_in_home[s] <- str_replace(players_in_home[s], "Natisha Hiedeman", "Natasha Hiedeman")
           }
           
           # Replace the player in the home lineup (This section needs to be changed also)
           if(str_detect(LineupAway[i], players_out_home[s])){
             LineupAway[i] <- str_replace(LineupAway[i], players_out_home[s], players_in_home[s])
             LineupHome[i] <- LineupHome[i]
           } else{
             LineupHome[i] <- str_replace(LineupHome[i], players_out_home[s], players_in_home[s])
             LineupAway[i] <- LineupAway[i]
           }
         }
        }
        # All of the players being subbed out
        players_out <- c(players_out_away, players_out_home) 
        
        # Substitution if there is only one substitution being done
      } else {
      # Determine the player coming in and the player coming out of the game
        # If the player coming out is in the either the home or away lineup, then make the 
        # substitution
        if(str_detect(LineupHome[i-1], player_out) | str_detect(LineupAway[i-1], player_out)){
          # If one of them is missing extract information from Basketball Reference
          if(player_in == "" || player_out == ""){
            # Filter for the specific game
            game <- game_info %>% filter(game_id == gameid)
            # Create a string for the url
            url <- paste0("https://www.basketball-reference.com/wnba/boxscores/pbp/", game$game_day, "0",game$bref_home, ".html")
            # Extract the play by play data using rvest
            table <- url %>% read_html() %>% html_table()
            table[[1]] <- table[[1]][-1,]
            table <- url %>% read_html() %>% html_table()
            colnames(table[[1]]) <- table[[1]][1,]
            bref_pbp <- table[[1]]
            # Rename the play by play columns
            colnames(bref_pbp) <- c("Time", "Away", "Away_Points", "Score", "Home_Points", "Home")
            # Filter for all plays at the time of the occurance
            possible_vals <- bref_pbp %>% filter(Time == paste0(pbp$clock_display_value[i], ".0"))
            # If the player coming out is unknown, pull the first initial and last name 
            # for the player coming in; else pull the first initial and last name for the player
            # coming out
            if(player_out == ""){
              first_inital <- str_extract(player_in, "^[A-Z]")
              last_name <- str_extract(player_in, "[A-Za-z]+$")
              player_name <- paste0(first_inital, ". ", last_name)
            } else {
              first_inital <- str_extract(player_out, "^[A-Z]")
              last_name <- str_extract(player_out, "[A-Za-z]+$") 
              player_name <- paste0(first_inital, ". ", last_name)
            }
            # If there is only one entry at the time of the substitution that contains the players
            # name, pull that row from the play by play. If there are multiple plays containing a 
            # player at the time, only pull the rows that includes the substitution
            if(sum(str_detect(possible_vals$Away, player_name)) == 1){
              sub <- possible_vals$Away[str_detect(possible_vals$Away, player_name)]
              if(length(sub) > 1){
                sub_play <- str_detect(sub, "enters the game")
                sub <- sub[sub_play]
              }
              # Extract either the player coming in or coming out depending on which is missing
              if(player_out == ""){
                player <- str_extract(sub, "(?<=for)(.*)$")
              } else {
                player <- str_extract(sub, "^(.*)(?= enters)")
              } 
              # Set to an empty string if the substitution doesn't occur correctly 
              if(!identical(player, character(0)) && is.na(player)){
                player <- ""
              }
              # If the player isn't on the away team, do the same process for the home team
            } else {
              # Find the rows where the player is included and find the row where a substitution 
              # occurs
              sub <- possible_vals$Home[str_detect(possible_vals$Home, player_name)]
              if(length(sub) > 1){
                sub_play <- str_detect(sub, "enters the game")
                sub <- sub[sub_play]
              }
              # Extract either the player coming in or coming out
              if(player_out == ""){
                player <- str_extract(sub, "(?<=for)(.*)$")
              } else {
                player <- str_extract(sub, "^(.*)(?= enters)")
              }
              
              # This may need to be adjusted as well 
              player <- str_extract(sub, "[A-Z.]+ [A-Za-z]+$")
              if(!identical(player, character(0)) && is.na(player)){
                player <- ""
              }
            }
            # Set the player to be empty if there is no sub extracted; Else extract the first and 
            # last name from the box score data
            if(identical(player, character(0))){
              player <- ""
              player_sub <- player
            } else {
              player_sub <- box_score$athlete_display_name[str_detect(box_score$athlete_short_name, player)]
              # Control for names that throw errors
              if(str_detect(player_sub, "Brittany Boyd-Jones")){
                player_sub <- str_replace(player_sub, "Brittany Boyd-Jones", "Brittany Boyd")
              }
              if(str_detect(player_sub, "Astou Ndour-Fall")){
                player_sub <- str_replace(player_sub, "Astou Ndour-Fall", "Astou Ndour")
              }
              if(str_detect(player_sub, "Natisha Hiedeman")){
                player_sub <- str_replace(player_sub, "Natisha Hiedeman", "Natasha Hiedeman")
              }
              
            }
            # Set player in or player out as the player that was extracted from 
            # Basketball reference
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
      
      # If any of the players coming out are in the lineup, sub them out
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
            # If one of the players are still missing for some reason, keep the lineup the same 
            # as previously
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
  
  # Select variables that are needed to pull out possession information
  possession <- test %>% dplyr::select(shooting_play, home_score, scoring_play, away_score,
                                text, score_value, team_id, type_text, LineupAway,
                                LineupHome, clock_display_value)
  # Vector with the different type of turnovers
  turnover_types <- c("Out of Bounds - Lost Ball Turnover", "Offensive Foul Turnover", "Shot Clock Turnover", "Bad Pass\nTurnover",
                      "Lost Ball Turnover", "Out of Bounds - Bad Pass Turnover", "Traveling")
  
  # Create a vector for change of possession
  change_possession <- numeric(nrow(possession))
  
  # Conditions that cause change of possession
  cond <- (possession$shooting_play == TRUE & possession$scoring_play == TRUE) | 
    possession$type_text == "Defensive Rebound" | possession$type_text %in% turnover_types
  
  # Free throws that don't change possession
  free_throws <- c("Free Throw - 1 of 2", "Free Throw - 1 of 3", "Free Throw - 2 of 3", "Free Throw - Technical")
  
  for(i in 2:nrow(possession)){
    # If any of those conditions are met, possession changes
    if(cond[i] == TRUE){
      change_possession[i] <- 1
    }
    # If a shooting foul occurs at the same time as a made basket, don't change possession
    if(possession$type_text[i] == "Shooting Foul" && (possession$clock_display_value[i] == possession$clock_display_value[i-1])){
      change_possession[i-1] <- 0
    }
    # If the last free throw is made but a substitution was made before the last free throw
    # Make the possession change before the free throw
    if((possession$type_text[i] == "Free Throw - 1 of 1" || 
        possession$type_text[i] == "Free Throw - 2 of 2" || 
        possession$type_text[i] == "Free Throw - 3 of 3") && 
       (possession$type_text[i-1] == "Substitution")){
      change_possession[i] <- 0
      change_possession[i-1] <- 1
    }
    # The possession doesn't change if the free throw isn't the last one
    if(possession$type_text[i] %in% free_throws){
      change_possession[i] <- 0
    }
    
  }
  
  # Bind on the change possession variable
  possession <- possession %>% mutate(change_possession = change_possession)

  # Create vectors the length of the number of possesions for the point differential 
  # and the lineups
  point_diff <- numeric(sum(possession$change_possession == 1) + 1)
  LineupAway <- numeric(length(point_diff))
  LineupHome <- numeric(length(point_diff))
  home_points <- numeric(length(point_diff))
  away_points <- numeric(length(point_diff))
  home_possession <- numeric(length(point_diff))
  # Pull the starting lineups again
  LineupAway[1] <- paste(starters$athlete_display_name[1:5], collapse = ",")
  LineupHome[1] <- paste(starters$athlete_display_name[6:10], collapse = ",")
  # Fix edges cases (Can probably adjust this code as well )
  if(str_detect(LineupHome[1], "Brittany Boyd-Jones")){
    LineupHome[1] <- str_replace(LineupHome[1], "Brittany Boyd-Jones", "Brittany Boyd")
  } else if(str_detect(LineupAway[1], "Brittany Boyd-Jones")){
    LineupAway[1] <- str_replace(LineupAway[1], "Brittany Boyd-Jones", "Brittany Boyd")
  }
  if(str_detect(LineupHome[1], "Astou Ndour-Fall")){
    LineupHome[1] <- str_replace(LineupHome[1], "Astou Ndour-Fall", "Astou Ndour")
  } else if(str_detect(LineupAway[1], "Astou Ndour-Fall")){
    LineupAway[1] <- str_replace(LineupAway[1], "Astou Ndour-Fall", "Astou Ndour")
  }
  
  home_points[1] <- 0
  away_points[1] <- 0
  # Determine who has possession at the beginning of the game
  player_possession <- str_extract(possession$text[1], "(?<=\\()(.*)(?= gains)")
  home_possession[1] <- ifelse(str_detect(LineupAway[1], player_possession), 0, 1)
  k <- 2
  # Filter through each row in the play by play data and when the possession changes
  # calculate the point differential for that possession; Also change which team has possession,
  # each time the possession changes
  for(i in 1:nrow(possession)){
    if(possession$change_possession[i] == 1){
      home_points[k] <-  possession$home_score[i]
      away_points[k] <- possession$away_score[i]
      LineupAway[k] <- possession$LineupAway[i]
      LineupHome[k] <- possession$LineupHome[i]
      point_diff[k] <- (home_points[k] - home_points[k-1]) - (away_points[k] - away_points[k-1])
      home_possession[k] <- ifelse(home_possession[k-1] == 1, 0, 1)
      k <- k + 1
    }
  }
  # Create a data frame with the points for both teams at each possession as well the point
  # differential for the possession and the lineups
  another <- data.frame(home_points, away_points, point_diff, LineupAway, LineupHome, home_possession)
  # Reshaping from wide to long format to make it easy to create X matrix for modeling 
  test2 <- another %>% 
    separate(LineupAway, into = c("P1", "P2", "P3", "P4", "P5"), sep = ",") %>%
    separate(LineupHome, into = c("P6", "P7", "P8", "P9", "P10"), sep = ",") %>% 
    pivot_longer(cols = P1:P10, values_to = "Player", names_to =  NULL) %>%
    mutate(Player = factor(Player))
  # Return the data in long format
  return(test2)
}

