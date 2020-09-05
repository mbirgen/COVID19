suppressPackageStartupMessages(require(plotly))
suppressPackageStartupMessages(require(openintro))
suppressPackageStartupMessages(require(shiny))
suppressPackageStartupMessages(require(tigris))
suppressPackageStartupMessages(require(shinydashboard))
suppressPackageStartupMessages(require(tidyverse))
suppressPackageStartupMessages(require(stringr))
suppressPackageStartupMessages(require(lubridate))
suppressPackageStartupMessages(require(dplyr))
suppressPackageStartupMessages(require(zoo))
suppressPackageStartupMessages(require(leaflet))
suppressPackageStartupMessages(require(RColorBrewer))
suppressPackageStartupMessages(require(DT))


JHdata<- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/07-06-2020.csv"

url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
countiesRaw <- read.csv(url)

countiesRaw <- data.frame(countiesRaw)
countiesRaw$date <- as.Date(countiesRaw$date)
countiesRaw <- filter(countiesRaw, county != "Unknown")
countiesRaw$fips <- as.character(countiesRaw$fips)
countiesRaw$fips <- str_pad(countiesRaw$fips, 5, side="left", pad =0)

url2 <- "https://covidtracking.com/api/v1/states/daily.csv"
states <- read.csv(url2)

statesRaw <- data.frame(states)
statesRaw <- subset(statesRaw, statesRaw$state %in% state.abb)
statesRaw$state <- abbr2state(statesRaw$state)
year <- str_sub(statesRaw$date, 1,4)
month <- str_sub(statesRaw$date, 5,6)
day <- str_sub(statesRaw$date, -2)
statesRaw$date <- paste(year, month, day, sep = "-")
statesRaw$date <- as.Date(statesRaw$date)


# creates improved column names
names(statesRaw)[6] <- "Hospitalized"
names(statesRaw)[8] <- "ICU"
names(statesRaw)[25] <- "Daily_Positive"
names(statesRaw)[26] <- "Daily_Negative"
names(statesRaw)[28] <- "Total_Tests"
names(statesRaw)[29] <- "Daily_Tests"
names(statesRaw)[31] <- "Daily_Deaths"

# strips columns  and rows from statesRaw that are needed 
statesRawN <- statesRaw[,c(1,2,3)]

# adding population Data
censusCounty <- read.csv("censusCounties.csv")
censusCounty <- data.frame(censusCounty)
censusCounty <- filter(censusCounty, STATE %in% state.name)

censusState <- read.csv("censusState.csv")
censusState <- data.frame(censusState[1:56,])

# national map 
statesRawNN <- statesRawN[,c(1,2,3)]
statesRawN1 <- filter(statesRawNN, date==max(statesRawNN$date))
statesRawN2 <- left_join(statesRawN1, censusState, by=c("state"="STATE"))
selectedUS <- statesRawN2 %>% mutate(perCap= positive/POPULATION)

USmerged <- geo_join(states(cb=T), selectedUS, "NAME", "state")
