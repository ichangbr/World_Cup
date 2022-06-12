library(tidyverse)
setwd("~/Master/Programación_avanzada/Proyecto")

options(digits = 5)
datos <- read.csv("./Data/data_teams.dat")
datos$DEF <- datos$DEF + (datos$MID*.36)
datos$OFF <- datos$OFF + (datos$MID*.4)
datos$GK <- datos$GK + (datos$DEF*.36)
datos$MID <- datos$MID
datos <- datos[, -5]

write.table(format(datos, nsmall = 3), file = "./Data/datos.dat",
            quote = F, sep = ",", row.names = F, col.names = F)

