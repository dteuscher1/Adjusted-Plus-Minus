## David Teuscher
## Last updated: 29.05.2021
## This script creates a dataset that includes the winner and loser for each game, 
## the game date and the ESPN game id as well as the abbreviations for both the home 
# away teams for ESPN and Basketball reference
###############################################################

# Load packages
library(rvest) # Used for webscraping; getting data from ESPN
library(tidyverse) # Used to manipulate the data set

# Create empty data frame to store data in
game_info <-  data.frame(Winner=character(), Loser = character(), game_day = character(), game_id = character(),
                         away_abr = character(), home_abr = character(), away_team = character(), 
                         home_team = character())

# There are row in the table that are empty except to indicate what round the game is
# part of. First and second round were always the same, while the semifinals and finals
# were dependent on the team, so this vector only includes the first two rounds
playoff_characters <- c("FIRST ROUND", "SECOND ROUND")

# Make a sequence of days from the start of the season to the end of the season
## NOTE: You can do the whole year, but then you will also include preseason games 
# and there is nothing to differentiate the preseason from the regular season, so I think 
# it is better to require the start and end date of the season. 

days <- seq(as.Date('2019-05-24'),as.Date('2019-10-10'),by='week')

# Have for loop that gets the games for each week
for(i in seq_along(days)){
    # Formats the date in the form needed for ESPN url
    date <- format(days[i],format="%Y%m%d")
    # Pull the html code from the schedule for a week of games
    games <- read_html(paste0("http://www.espn.com/wnba/schedule/_/date/",date))
    # Pull all of the h2 elements from the html code and then pull the text
    game_days <- games %>% html_elements('h2') %>% html_text2()
    
    # Find the html element with the class of "sched-container" and then pull the 
    # first five columns from that table
    info <- data.frame(games %>% html_nodes(xpath="//*[@id=\"sched-container\"]") %>% html_table())[,1:5]
    
    # Rename the columns so they are consistent throughout
    colnames(info) <- c('matchup', "Var.2", "result", "winner.high", "loser.high")
    
    # Set a counter for the day (ranges from 1 to 7)
    k <- 1
    game_date <- character(length = nrow(info))
    # If there are no games scheduled or come across a row that stars as matchup,
    # then the game day counter should be increased by one so that any following games
    # are given the correct game day
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
    
    # Bind the column with the game dates and then filter out the rows that 
    # have no game date
    info <- info %>% mutate(game_date = game_date) %>% 
        filter(matchup != 'matchup', matchup != "No games scheduled", matchup != "FIRST ROUND", !(matchup %in% playoff_characters), 
               !str_detect(matchup, "WNBA SEMIFINALS"), !str_detect(matchup, "WNBA FINALS"))
    
    # Pull out the href attributes that have the game id in them
    game_id <- games %>% html_nodes(xpath="//td/a") %>% html_attr("href")
    game_id <- as.data.frame(game_id)
    
    # Change the game id to a character and extract each gameid
    game_id[,1] <- as.character(game_id[,1])
    game_id <- game_id[grep("gameId",game_id[,1]),]
    game_ids <- str_extract(game_id, "[0-9]+$")
    
    # Bind the game id with the other data and extract the home and away team names
    # and abbreviations
    info_all <- info %>% mutate(game_id = game_ids, 
                              away_abr = str_extract(matchup, "[A-Z]+$"),
                              home_abr = str_extract(Var.2, "[A-Z]+$"), 
                              away_team = str_extract(matchup, ".+?(?= [A-Z]+$)"),
                              home_team = str_extract(Var.2, ".+?(?= [A-Z]+$)")) %>% 
        # Extract the winner and loser from the score
        separate(result, into = c("Winner", "Loser"), sep = ",") %>%
        mutate(Winner = str_extract(Winner, "[A-Z]+"),
               Loser = str_extract(Loser, "[A-Z]+")) %>% 
        # Get rid of variables that aren't needed anymore
        dplyr::select(-matchup, -Var.2, -winner.high, -loser.high)
    # Bind the data for the specific week to the rest of the data for previous weeks
    game_info <- game_info %>% bind_rows(info_all)
    Sys.sleep(15)
}

# Format the game date into the necessary format for the Basketball Reference 
# play by play urls
game_info <- game_info %>% filter(Winner != "WIL") %>% 
    mutate(game_day = paste0(game_date, ", 2019"),
           game_day = as.Date(game_day, "%A, %B %d, %Y"),
           game_day = format(game_day, "%Y%m%d"))

# Read in a file that has the team abbreviations that ESPN uses and the 
# abbreviations that Basketball Reference uses and join them together
teams <- read.csv("team_abbreviations.csv")
game_info <- game_info %>% 
    inner_join(teams, by = c('away_abr' = 'espn')) %>% 
    inner_join(teams, by = c('home_abr' = 'espn')) %>%
    rename(bref_away = bref.x,
           bref_home = bref.y)
