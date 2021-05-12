# Install the wehoop package
# devtools::install_github(repo = "saiemgilani/wehoop")
library(wehoop)
library(tidyverse)
data <- espn_wnba_pbp(game_id = "401104913")
box_score <- wehoop::espn_wnba_player_box(game_id = "401104913") %>%
  filter(starter == TRUE)

pbp <- cbind(data, Player = t(box_score$athlete.displayName)) %>% 
  unite(col = "Lineup1", 26:30, sep = ",") %>%
  unite(col = "Lineup2", 21:25, sep = ",")
subs <- pbp %>% filter(type.text == "Substitution") 

# Correctly replaces players in the lineup, but it uses the original lineup from the beginning
player_in <- str_extract(subs$text, "^^[A-Z][-a-zA-Z]+ [A-Z][-a-zA-Z]+")
player_out <- str_extract(subs$text, "[A-Z][-a-zA-Z]+ [A-Z][-a-zA-Z]+$")
for(i in 1:nrow(subs)){
  if(str_detect(subs$Lineup1[i], player_out[i])){
    subs$Lineup1[i] <- str_replace(subs$Lineup1[i], player_out[i], player_in[i])
  } else{
    subs$Lineup2[i] <- str_replace(subs$Lineup2[i], player_out[i], player_in[i])
  }
}
