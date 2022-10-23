# sprobuje tenisistowy graf zrobic

getwd()
options(stringsAsFactors = FALSE)

data_set_names <- c("circuits", "constructor_results", "constructor_standings", 
                    "constructors", "driver_standings", "drivers", 
                    "lap_times", "pit_stops", "qualifying", "races",
                    "results", "seasons", "sprint_results", "status")

for (el in data_set_names ){
  path <- paste("data/", el,".csv", sep = "")
  assign(el, read.csv(path))
}

library(dplyr)
library(ggplot2)

# hamilton niebieski
# schumacher czerwony
# verstappen granatowy
# vettel zielony

# filtruje z resoults tylko tych kierowców co nas interesuj¹ i tylko pierwsze 
# miejsca

# merge z races po raceid zeby miec rok

# grupuje sezonami i kierowcami, zliczam


