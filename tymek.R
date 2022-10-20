# siemanko witam w mojej kuchni

# tutaj robimy triki jakies no

getwd()


# pobieranie zbioróW
options(stringsAsFactors = FALSE)

data_set_names <- c("drivers", "driver_standings", "races", "results",
                    "seasons", "sprint_results", "status")

el <- data_set_names[1]

for (el in data_set_names ){
  path <- paste("data/", el, ".csv", sep = "")
  assign(el, read.csv(path))
}


# pierwsza analiza testowa
# zrobimy sobie dla 4 wybranych zawodników porównanie œrednich prêdkosci w ka¿dym sezonie


library(dplyr)
library(ggplot2)

df <- results %>% 
  select(resultId, raceId, driverId, milliseconds)

# czyscimy te dziwne \N
df <- df[!is.na(as.numeric(as.character(df$milliseconds))),]
df$milliseconds <- as.numeric(df$milliseconds)

df <- df %>%  merge(drivers) %>% 
  filter(surname %in% c("Hamilton", "Kubica", "Verstappen")) %>% 
  merge(races, by.x = "raceId", by.y="raceId")

df <- df %>% 
  group_by(surname, year) %>% 
  summarize(avg_time = mean(milliseconds/(1000))) 

df %>% 
  ggplot() +
  aes(y=avg_time, x=year, color=surname) %>% 
  geom_line(size = 1) +
  labs(title="porównanie srednich predkosci zawodnikow",
        y = "œredni czas w sekundach",
        x = "rok") +
  xlim(2005, 2022)


head(df)
