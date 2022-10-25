# siemanko witam w mojej kuchni

# tutaj robimy triki jakies no

getwd()
source("mega_wazny_plik.R")


# pobieranie zbioróW
options(stringsAsFactors = FALSE)

data_set_names <- c("drivers", "driver_standings", "races", "results",
                    "seasons", "sprint_results", "status", "earnings_2021")

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
  select(resultId, raceId, driverId, milliseconds, position)

# czyscimy te dziwne \N
df <- df[!is.na(as.numeric(as.character(df$milliseconds))),]
df <- df[!is.na(as.numeric(as.character(df$position))),]
#df %>% transform(position = ifelse(position == "\N", ))
df$position <- as.numeric(df$position)
df$milliseconds <- as.numeric(df$milliseconds)

df <- df %>%  merge(drivers) %>% 
  filter(driverRef %in% 
           c("hamilton", "vettel", "michael_schumacher", "max_verstappen")) %>% 
  merge(races, by.x = "raceId", by.y="raceId")

df <- df %>% 
  group_by(surname, year) %>% 
  summarize(avg_time = mean(milliseconds/(1000)), avg_position=mean(position))

df %>% 
  ggplot() +
  aes(y=avg_time, x=year, color=surname) %>% 
  geom_line(size = 1) +
  labs(title="porównanie srednich czasów zawodnikow",
        y = "œredni czas w sekundach",
        x = "rok") +
  xlim(2005, 2022)




head(df)

# porownanie srednich pozycji zawodników


df %>% 
  ggplot() +
  aes(y=avg_position, x=year, color=surname) %>% 
  geom_line(size = 1) +
  labs(title="porównanie srednich pozycji zawodnikow",
       y = "œrednie pozycje",
       x = "rok") +
  xlim(2005, 2022) 


head(earnings_2021) # te dane sa z https://www.statista.com/statistics/1255926/formula-one-salaries/


earnings_2021 %>% 
  ggplot() +
  aes(x=earnings, y=reorder(name, earnings), fill=name) +
  geom_bar(stat="identity") +
  labs(title="porównanie zarobków najlepiej zarabiaj¹cych kierowców F1",
       subtitle = "w 2021 roku",
       y = "zawodnik",
       x = "zarobki (w mln $)") +
  scale_fill_manual(values = c("Lewis Hamilton" = Hamilton, 
                               "Max Verstappen" = Verstappen,
                               "Sebastian Vettel" = Vettel))
