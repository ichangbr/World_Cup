setwd("~/Master/Programación_avanzada/Proyecto/")
library(tidyverse)

player_data <- read_csv("./Data/players_22.csv")

teams <- c('Qatar', 'Ecuador', 'Senegal', 'Netherlands', 'England', 'Iran', 'United States', 'Ukraine', 'Argentina',
           'Saudi Arabia', 'Mexico', 'Poland', 'France', 'Australia', 'Denmark', 'Tunisia', 'Spain',
           'Costa Rica', 'Germany', 'Japan', 'Belgium', 'Canada', 'Morocco', 'Croatia', 'Brazil',
           'Serbia', 'Switzerland', 'Cameroon', 'Portugal', 'Ghana', 'Uruguay', 'Korea Republic'
)

nationalities <- unique(as.vector(player_data[["nationality_name"]]))

teams.indata <- intersect(teams, nationalities)

player_data <- player_data %>% select(short_name, long_name, overall,
                                      nationality_name, club_position) %>% 
              filter(nationality_name %in% teams.indata)

player_data$club_position[player_data$club_position %in% c("LCB", "RCB")] <- "CB"

Team_formation <- list()
for (team in teams.indata) {
  Team_formation[[team]] <- list()
  Team_formation[[team]][["GK"]] <- 0
  Team_formation[[team]][["DEF"]] <- vector(mode = "numerical")
  Team_formation[[team]][["MID"]] <- vector(mode = "numerical")
  Team_formation[[team]][["OFF"]] <- vector(mode = "numerical")
}

for (team in teams.indata[2]) {
  for (pos in names(Team_formation[[team]])) {
    print(pos)
    if (pos != "GK") {
      num_players <- readline("Number of players in the position: ")
    } else {num_players = 1}
    for (i in num_players) {
      Team_formation[[team]][[pos]] <- readline("Add position value:")
    }
  }
}



