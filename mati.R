# co to za kod jezeli nie jest podjebany

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


#  jakie wykresy chce zrobic

# ilosc wygranych wyscigow unormowane do rozpoczecia kariery
# srednie predkosci bolidow ferrari redbulla mclaren i mercedesa na przestrzeni lat (kolorem zaznaczone sezony w ktorych dany bolid byl najszybszy)
# najbogatsi kierowcy (slupkowy obrócony)
# jak duze znaczenie ma poczatkowa pozycja (brak pomyslu na wykres) (moze finalne miejsce w zaleznosci od poczatkowego miejca)
# kto ma rekord na danym torze i kiedy zostal ustanowiony (fajnie sie laczy z srednie predkosci bolidow)
# jak wyglada statystycznie miejsce wygranego podczas wysciugu np od polowy malo juz sie dzieje bo pierwszy prawie zawsze zostaje pierwszy
# jak wygl¹daja patterny pitstopow w sezonie 2021 ferrari redbulla mclaren i mercedesa

View(results)

#miejsce startowe a koncowa pozycja od 1950 do teraz
results %>%
  select(raceId, driverId, positionOrder, grid) %>%
  group_by(grid) %>%
  summarise(mean(positionOrder)) -> tmp

plot(tmp,xlab="pozycja startowa", ylab="finalnie miejsce", xlim=c(0, 27), ylim=c(0, 25))
abline(a=0, b=1, color = "r")


#miejsce startowe a koncowa pozycja od 2000 do teraz

races %>%
  select(raceId, year) -> lata

results %>%
  left_join(lata, by = "raceId") %>%
  filter(year < 2000) %>%
  select(raceId, driverId, positionOrder, grid) %>%
  group_by(grid) %>%
  summarise(mean(positionOrder)) -> miejsca_przed_2000

results %>%
  left_join(lata, by = "raceId") %>%
  filter(year > 2000) %>%
  select(raceId, driverId, positionOrder, grid) %>%
  group_by(grid) %>%
  summarise(mean(positionOrder)) -> miejsca_po_2000


plot(miejsca_przed_2000,xlab="pozycja startowa", ylab="finalnie miejsce", xlim=c(1, 27), 
     ylim=c(1, 21),col="blue", pch = 19)
points(miejsca_po_2000,col="red", pch = 19)
abline(a=0, b=1)
