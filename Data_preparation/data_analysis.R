library(tidyverse)
setwd("~/Master/Programación_avanzada/Proyecto")

TeamFormation <- readRDS("Data/team_formation.rds")

offense_values <- vector(mode = "numeric")
midfield_values <- vector(mode = "numeric")
defense_values <- vector(mode = "numeric")
goalkeep_values <- vector(mode = "numeric")
for (team in names(TeamFormation)) {
  for (pos in names(TeamFormation[[team]])) {
    TeamFormation[[team]][[pos]] <- as.numeric(TeamFormation[[team]][[pos]])
    if (pos == "GK"){
      goalkeep_values <- c(goalkeep_values, TeamFormation[[team]][[pos]])
    } else if (pos == "DEF") {
      defense_values <- c(defense_values, TeamFormation[[team]][[pos]])
    } else if (pos == "MID") {
      midfield_values <- c(midfield_values, TeamFormation[[team]][[pos]])
    } else {
      offense_values <- c(offense_values, TeamFormation[[team]][[pos]])
    }
  }
}

qatar_offense <- quantile(offense_values, 0.07)
qatar_midfield <- quantile(midfield_values, 0.07)
qatar_defense <- quantile(defense_values, 0.07)
qatar_goalkeep <- quantile(goalkeep_values, 0.07)

# LATER
Qatar <- list()
Qatar[["GK"]] <- qatar_goalkeep
Qatar[["DEF"]] <- qatar_defense*4
Qatar[["MID"]] <- qatar_midfield*4
Qatar[["OFF"]] <- qatar_offense*2

team_names <- names(TeamFormation)
TeamFormation <- c(list(Qatar), TeamFormation)
names(TeamFormation) <- c("Qatar", team_names)

for (team in names(TeamFormation)) {
  for (pos in names(TeamFormation[[team]])) {
    TeamFormation[[team]][[pos]] <- sum(TeamFormation[[team]][[pos]]/10)
  }
}

df <- t(as.data.frame(TeamFormation))
df <- cbind(df, rownames(df))
rownames(df) <- c()
colnames(df) <- c("values", "names")
df <- transform(df, values = as.numeric(values))
df[25:28, "names"] <- c("United_States.GK", "United_States.DEF",
                        "United_States.MID", "United_States.OFF")
df[37:40, "names"] <- c("Saudi_Arabia.GK", "Saudi_Arabia.DEF",
                        "Saudi_Arabia.MID", "Saudi_Arabia.OFF")
df[69:72, "names"] <- c("Costa_Rica.GK", "Costa_Rica.DEF",
                        "Costa_Rica.MID", "Costa_Rica.OFF")
df[125:128 , "names"] <- c("Korea_Republic.GK", "Korea_Republic.DEF",
                        "Korea_Republic.MID", "Korea_Republic.OFF")

df[,c("country", "position")] <- str_split_fixed(df$names, '\\.', 2)
df <- df[c("country", "position", "values")]

df <- df %>% pivot_wider(names_from = position,
                   values_from = values)

write.csv(df[,2:5], file = "./Data/data_teams.dat", row.names = F)

