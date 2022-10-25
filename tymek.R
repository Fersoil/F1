# siemanko witam w mojej kuchni

# tutaj robimy triki jakies no

getwd()
source("mega_wazny_plik.R")


# pobieranie zbioróW
options(stringsAsFactors = FALSE)

data_set_names <- c("drivers", "driver_standings", "races", "results",
                    "seasons", "sprint_results", "status", "lap_times", "earnings_2021")

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
  theme_minimal() +
  labs(title="porównanie zarobków najlepiej zarabiaj¹cych kierowców F1",
       subtitle = "w 2021 roku",
       y = "zawodnik",
       x = "zarobki (w mln $)") +
  scale_fill_manual(values = c("Lewis Hamilton" = Hamilton, 
                               "Max Verstappen" = Verstappen,
                               "Sebastian Vettel" = Vettel)) +
  theme(legend.position = "none")



# wyprzedzanie
races %>% 
  filter(year == 2021) %>% 
  rename(circuit = name) -> races

drivers <- drivers %>% 
  filter(driverId %in% driver_standings$driverId) %>% 
  mutate(driver.name = paste(forename, surname, sep = " ")) %>% 
  select(driverId, code, number, driver.name, dob, nationality, url) %>% 
  rename(driver.number = number)

gp <- races %>% select(raceId, round, circuit)
dr <- drivers %>% select(driverId, driver.name, code, driver.number)

lap_times %>%  left_join(gp, by = "raceId") %>%
  left_join(dr, by = "driverId")%>% 
  select(-raceId, -driverId) -> lp

lp %>% 
  filter(circuit == "Italian Grand Prix") %>% # wybieramy sobie grand prix
  mutate(colours = case_when( # dodajemy kolorki
    driver.name == "Lewis Hamilton" ~ Hamilton,
    driver.name == "Max Verstappen" ~ Verstappen,
    driver.name == "Sebastian Vettel" ~ Vettel,
    TRUE ~ "#f0f0f0"
  )) %>% 
  ggplot(aes(x=lap, y=position, group=driver.name, color=colours)) +
  geom_line(size=2) +
  labs(title = "Pozycje zawodników na Italian Grand Prix 2021", 
       y = "Pozycja",
       x = "Okr¹¿enie") 
  
