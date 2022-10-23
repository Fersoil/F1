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

najlepsi_id <- drivers %>%
  filter(surname %in% c("Hamilton", "Schumacher", "Vettel", "Verstappen"))

# hamilton niebieski id=1
# schumacher czerwony id=30
# verstappen granatowy id=830
# vettel zielony id=20
head(df)

# filtruje z results tylko tych kierowców co nas interesuj¹ i tylko pierwsze 
# miejsca
results %>%
  filter(driverId %in% c(1, 30, 830, 20) & position == 1) %>%
  select(driverId, raceId) -> df

# merge z races po raceid zeby miec rok
races %>%
  select(raceId, year) %>%
  merge(df) -> df

# grupuje sezonami i kierowcami, zliczam
df %>%
  group_by(year) %>%
  count(driverId) -> df
  
df %>%
  group_by(driverId) %>%
  mutate(cumsum(n)) -> df

# okazuje sie ze numeryczne driverId sie nie spisuje X,D 
df %>%
  ggplot(aes(x = year, y = `cumsum(n)`, colour = driverId)) + 
  geom_line() + 
  geom_point() 

#popraweczka
df$driverId <- as.character(df$driverId)

#goodgame plot
df %>%
  ggplot(aes(x = year, y = `cumsum(n)`, colour = driverId)) + 
  geom_line() + 
  geom_point() +
  labs(title = "liczba zwyciêzonych wyœcigów na przestrzeni kariery", 
       x = "sezony",
       y = "sumaryczna liczba zwyciêstw") +
  xlim(1990, 2022)
