# siemanko witam w mojej kuchni

# tutaj robimy triki jakies no

getwd()
source("mega_wazny_plik.R")

library(dplyr)
library(ggplot2)
library(scales)
library(tidyquant)


# pobieranie zbioróW
options(stringsAsFactors = FALSE)

data_set_names <- c("drivers", "driver_standings", "races", "results",
                    "seasons", "sprint_results", "status", "lap_times", 
                    "earnings_2021", "total_earnings")

el <- data_set_names[1]

for (el in data_set_names ){
  path <- paste("data/", el, ".csv", sep = "")
  assign(el, read.csv(path))
}


drivers <- drivers %>% 
  filter(driverId %in% driver_standings$driverId) %>% 
  mutate(driver.name = paste(forename, surname, sep = " ")) %>% 
  select(driverId, code, number, driver.name, dob, nationality, url) %>% 
  rename(driver.number = number)

results <- results %>% 
  select(resultId, raceId, driverId, milliseconds, position)

# czyscimy te dziwne \N
results <- results[!is.na(as.numeric(as.character(results$milliseconds))),]
results <- results[!is.na(as.numeric(as.character(results$position))),]
#df %>% transform(position = ifelse(position == "\N", ))
results$position <- as.numeric(results$position)
results$milliseconds <- as.numeric(results$milliseconds)



# pierwsza analiza testowa
# zrobimy sobie dla 4 wybranych zawodników porównanie œrednich prêdkosci w ka¿dym sezonie

# SREDNIE PREDKOSCI SEZONOWE


df <- results %>%  merge(drivers) %>% 
  filter(driver.name %in% our_drivers) %>% 
  merge(races, by.x = "raceId", by.y="raceId") %>% 
  mutate(date = lubridate::floor_date(as.Date(date), "month"))

str(df$date)

df <- df %>% 
  group_by(driver.name, year) %>% 
  summarize(avg_time = mean(milliseconds/(1000)), avg_position=mean(position))

df %>% 
  ggplot() +
  aes(y=avg_time, x=date, color=driver.name) %>% 
  geom_smooth(size = 1) +
  labs(title="porównanie srednich czasów zawodnikow",
        y = "œredni czas w sekundach",
        x = "rok") +
  theme_minimal() +
  scale_color_manual(values = driver_colors) +
  # xlim(2005, 2022) +
  scale_y_reverse()




head(df)

# porownanie srednich pozycji zawodników

brk <- function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)

df %>% 
  ggplot() +
  aes(y=avg_position, x=year, color=driver.name, group = driver.name) %>% 
  geom_ma(size = 1, se=FALSE) +
  labs(title="Porównanie œrednich pozycji zawodnikow",
       subtitle="w zawodach Grand Prix na przestrzeni lat",
       y = "Œrednia pozycja",
       x = "Rok",
       color = "Kierowca") +
  theme_minimal() +
  scale_color_manual(values = driver_colors) +
  #xlim(2005, 2022) +
  scale_y_reverse(breaks = c(10, 7, 4, 1)) 


# ZAROBKI


head(earnings_2021) # te dane sa z https://www.statista.com/statistics/1255926/formula-one-salaries/
head(total_earnings) # te dane s¹ z https://www.scmp.com/magazines/style/celebrity/article/3182964/10-richest-f1-drivers-all-time-net-worths-ranked-lewis
# oraz dla verstappena https://www.spotrac.com/formula1/oracle-red-bull-racing/max-verstappen-47373/cash-earnings/

total_earnings %>% 
  merge(drivers, by.x="name", by.y="driver.name") %>% 
  ggplot() +
  aes(x=earnings, y=reorder(name, earnings), fill=name) +
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(title="Porównanie maj¹tków najlepiej zarabiaj¹cych kierowców F1",
       subtitle = "na rok 2022",
       y = "zawodnik",
       x = "maj¹tki (w mln $)") +
  scale_fill_manual(values = driver_colors) +
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

lp %>% filter(driver.name == "Robert Kubica", circuit != "<NA>")

lp %>% 
  filter(circuit == "Abu Dhabi Grand Prix") %>% # wybieramy sobie grand prix
  mutate(colours = case_when( # dodajemy kolorki
    driver.name == "Lewis Hamilton" ~ Hamilton,
    driver.name == "Max Verstappen" ~ Verstappen,
    driver.name == "Sebastian Vettel" ~ Vettel,
    TRUE ~ "#f0f0f0"
  )) %>% 
  ggplot(aes(x=lap, y=position, group=driver.name, color=colours, alpha)) +
  geom_line(size=2) +
  labs(title = "Pozycje zawodników na Abu Dhabi Grand Prix 2021", 
       y = "Pozycja",
       x = "Okr¹¿enie") 
  
