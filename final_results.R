setwd("~/Master/Programación_avanzada/Proyecto")
results <- read.delim("./main/results", header = F)[[1]]
teams <- c('Qatar', 'Ecuador', 'Senegal', 'Netherlands', 'England', 'Iran', 'United States', 'Ukraine', 'Argentina',
           'Saudi Arabia', 'Mexico', 'Poland', 'France', 'Australia', 'Denmark', 'Tunisia', 'Spain',
           'Costa Rica', 'Germany', 'Japan', 'Belgium', 'Canada', 'Morocco', 'Croatia', 'Brazil',
           'Serbia', 'Switzerland', 'Cameroon', 'Portugal', 'Ghana', 'Uruguay', 'Korea Republic')

winner_names <- c()
count <- 1
for (value in results){
  winner_names[count] <- teams[value]
  count = count + 1
}
table(winner_names)
