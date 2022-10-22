# co to za kod jezeli nie jest podjebany

getwd()
options(stringsAsFactors = FALSE)

data_set_names <- c("circuits.csv", "constructor_results.csv", "constructor_standings.csv", 
                    "constructors.csv", "driver_standings.csv", "drivers.csv", 
                    "lap_times.csv", "pit_stops.csv", "qualifying.csv", "races.csv",
                    "results.csv", "seasons.csv", "sprint_results.csv", "status.csv")

for (el in data_set_names ){
  path <- paste("data/", el, sep = "")
  assign(el, read.csv(path))
}

library(dplyr)
library(ggplot2)



