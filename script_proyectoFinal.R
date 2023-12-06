library(readxl)
library(tidyverse)
library(writexl)

estudiantesDesplazados <- read_excel("ESTUDIANTES_EN_SITUACI_N_DE_DESPLAZAMIENTO_EN_SANTANDER.xlsx")

head(estudiantesDesplazados) # para ver mas valores: head(aux, 10)
tail(estudiantesDesplazados)
View(estudiantesDesplazados)
names(estudiantesDesplazados)
class(estudiantesDesplazados$d_muni)
length(estudiantesDesplazados$d_ano)
unique(estudiantesDesplazados$d_provincia)

is.na(estudiantesDesplazados)
sum(is.na(estudiantesDesplazados))

summary(estudiantesDesplazados)

library(skimr)
skim(estudiantesDesplazados)

estudiantesDesplazados %>%
  dplyr::group_by(d_provincia) %>%
  skim()
  
glimpse(estudiantesDesplazados)
dim(estudiantesDesplazados)

#frecuencia de aparición
tabledata <- table(estudiantesDesplazados$d_provincia)
barplot(tabledata, horiz = FALSE, las = 2)
pie(tabledata)

hist(estudiantesDesplazados$edad)

plot(estudiantesDesplazados$edad, estudiantesDesplazados$d_grado, xlim = c(1,70), ylim = c(0,30))

boxplot(tabledata)
which.max(tabledata)
