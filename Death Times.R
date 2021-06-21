OldDeaths <- read.csv("Total Deaths14.csv")
OldDeaths[,1] = as.Date(OldDeaths[,1])
NewDeaths <- read.csv("Total Deaths16.csv")
NewDeaths[,1] = as.Date(NewDeaths[,1])


NDeaths = as.data.frame(merge(NewDeaths, OldDeaths, by ="EventDateOfDeath"))
NDeaths = NDeaths %>% mutate(change = Total.Deaths...Daily.Total.x
                             -Total.Deaths...Daily.Total.y)
NDeaths[NDeaths$change != 0,1]
