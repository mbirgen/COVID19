mydate <- Sys.Date()
# filename <- "C:\\Users\\mariah.birgen\\Downloads\\Summary.csv"
filename <- "Summary.csv"
CountyData <- read.csv(filename, 
                       stringsAsFactors = FALSE)

CountyData <- as.data.frame(CountyData) %>%
    arrange(EventResidentCounty) %>%
    mutate(Active = Individuals.Positive - Total.Recovered - Total.Deaths,
           positivity = Individuals.Positive/Individuals.Tested,
           date = as.Date(mydate),
           )
rownames(CountyData) <- CountyData[,1]
temp <-c( EventResidentCounty = "Totals",
          colSums(CountyData[,2:6]),
          positivity = NA,
          date = as.character(mydate))
temp["positivity"] = as.integer(temp["Individuals.Positive"])/as.integer(temp["Individuals.Tested"])
CountyData["Total",] = temp
# CountyData <- rbind(CountyData, Total = temp)
# CountyData <-cbind(date = as.Date(mydate), CountyData)
write.csv(CountyData, file = paste(
    'CountyData/',mydate,'CountyData.csv'))

# hospital <- as.data.frame(read.csv("CountyHospitilizations.csv"))
# hospital <- hospital[,-1]
# h_county <-  t(hosp_by_county[,2])
d_county <- t(CountyData[,-1])
county_names <- CountyData[,1]
colnames(d_county) <- CountyData[,1]
# d_county <- as.data.frame(d_county)%>%
#     mutate(Total = Reduce(`+`, .))
d_county <- cbind(date =as.character(mydate),d_county)

Tested <- read.csv( "CountyData/CountyTests.csv", stringsAsFactors=FALSE)
Tested <- Tested[,-1]
Positive <- read.csv( "CountyData/CountyPositive.csv", stringsAsFactors=FALSE)
Positive <- Positive[,-1]
Recovered <- read.csv( "CountyData/CountyRecovered.csv", stringsAsFactors=FALSE)
Recovered <- Recovered[,-1]
Deaths <- read.csv( "CountyData/CountyDeaths.csv", stringsAsFactors=FALSE)
Deaths <- Deaths[,-1]
Active <- read.csv( "CountyData/CountyActive.csv", stringsAsFactors=FALSE)
Active <- Active[,-1]


if( as.character(Tested[nrow(Tested), 1]) != mydate){
Tested <- rbind(Tested, d_county[1,])
Positive <- rbind(Positive, d_county[2,])
Recovered <- rbind(Recovered, d_county[3,])
Deaths <- rbind(Deaths, d_county[4,])
Active <- rbind(Active, d_county[5,])
}

Tested[,-1] = sapply(Tested[,-1], as.integer)
Positive[,-1] = sapply(Positive[,-1], as.integer)
Recovered[,-1] = sapply(Recovered[,-1], as.integer)
Deaths[,-1] = sapply(Deaths[,-1], as.integer)
Active[,-1] = sapply(Active[,-1], as.integer)

# Tested <- Tested[-nrow(Tested),]
# Positive <- Positive[-nrow(Positive),]
# Recovered <- Recovered[-nrow(Recovered),]
# Deaths <- Deaths[-nrow(Deaths),]

trows <- nrow(Tested)
newTest <- Tested[trows,-1]-Tested[trows-1,-1]
newPos <- Positive[trows,-1]-Positive[trows-1,-1]
newRec <- Recovered[trows,-1]-Recovered[trows-1,-1]
newDeath <- Deaths[trows,-1]-Deaths[trows-1,-1]
PerPos7 <- (Positive[trows,-1]-
              Positive[trows-7,-1])/
  (Tested[trows,-1]-
     Tested[trows-7,-1])

NewToday <- rbind(newTest, newPos, newRec, newDeath)
NewToday <- rbind(Active = Positive[trows,-1] - 
                    Recovered[trows,-1] -
                    Deaths[trows,-1],
                  NewToday, PerPos = newPos/newTest,
                  PerPos7 = PerPos7)

colnames(NewToday) <- county_names[1:101]

names_list <- CountyData[,1]
names_list <- names_list[-75]
names_list <- names_list[-100]

per100 <- Tested[,1]
for(i in names_list){
  pop[i] <- county[
    county$STATE=="Iowa"&county$COUNTY==i,][,3]
  tempname <- gsub(" ", "." , i)
  tempname = gsub("'",".", tempname)
  per100 <- cbind(per100,Active[tempname]/pop[i]*100000)
}

county_names <- colnames(Tested)
county_names <- county_names[-76]
county_names <- gsub(" ", ".", county_names)
for(i in 2:(length(county_names))){
    test <- Tested[,i]
    temp = length(test)
    pos <- as.numeric(Positive[,i])
    rec <- as.numeric(Recovered[,i])
    hosp <- as.integer(hospital[(nrow(hospital)-temp+1):nrow(hospital),i])
    death <- as.numeric(Deaths[,i])
    dates <- as.Date(Tested[,1]    )
    temp <- data.frame(
        date = dates,
        Tested = test,
        Positive = pos,
        Recovered = rec,
        Hospitalized = hosp,
        Deaths = death)
    
    temp <- temp %>% 
        mutate(
            Active = Positive - Recovered - Deaths,
            New.Pos = Positive - lag(Positive),
            New.Test = Tested - lag(Tested),
            New.Rec = Recovered - lag(Recovered),
            New.Deaths = Deaths - lag(Deaths),
            Frac.Pos = New.Pos/New.Test,
            Day7.Test = Tested - lag(Tested, n=7),
            Day7.Pos = Positive - lag(Positive, n=7),
            Day7.Rec = Recovered - lag(Recovered, n=7),
            Day7.Death = Deaths - lag(Deaths, n=7),
            Day7.Active = Active - lag(Active, n=7),
            Day7.Perc = Day7.Pos/Day7.Test,
            Day14.Test = Tested - lag(Tested, n=14),
            Day14.Pos = Positive - lag(Positive, n=14),
            Day14.Rec = Recovered - lag(Recovered, n=14),
            Day14.Death = Deaths - lag(Deaths, n=14),
            Day14.Active = Active - lag(Active, n=14),
            Day14.Perc = Day14.Pos/Day14.Test
        )
    
    assign(paste(county_names[i],"Data", sep=""), temp)
    # write.csv(temp, paste("CountyData/",county_names[i],
                          # "Data.csv", sep=""))
}

# Adding more data to Bremer
hosp <- as.integer(hospital[,"Bremer"])
temp <- clean %>% select("date","Bremer.Positive", 
                         "Bremer.Recovered", "Bremer.Death")
names(temp) = c("date","Positive", "Recovered", "Deaths")
temp <- subset(temp, 
               !is.na(Positive) & date < "2020-07-31")%>% 
  mutate(Tested = NA, Hospitalized = NA)
temp2 <- BremerData %>% select("date","Hospitalized", "Tested","Positive", "Recovered", "Deaths")
temp = rbind(temp, temp2)
rm(temp2)
temp <- temp  %>% 
  mutate(
    Active = Positive - Recovered - Deaths,
    New.Pos = Positive - lag(Positive),
    New.Test = Tested - lag(Tested),
    New.Rec = Recovered - lag(Recovered),
    New.Deaths = Deaths - lag(Deaths),
    Frac.Pos = New.Pos/New.Test,
    Day7.Test = Tested - lag(Tested, n=7),
    Day7.Pos = Positive - lag(Positive, n=7),
    Day7.Rec = Recovered - lag(Recovered, n=7),
    Day7.Death = Deaths - lag(Deaths, n=7),
    Day7.Active = Active - lag(Active, n=7),
    Day7.Perc = Day7.Pos/Day7.Test,
    Day14.Test = Tested - lag(Tested, n=14),
    Day14.Pos = Positive - lag(Positive, n=14),
    Day14.Rec = Recovered - lag(Recovered, n=14),
    Day14.Death = Deaths - lag(Deaths, n=14),
    Day14.Active = Active - lag(Active, n=14),
    Day14.Perc = Day14.Pos/Day14.Test
  )
BremerData = temp
write.csv(BremerData, "CountyData/BremerData.csv")

write.csv(Tested, "CountyData/CountyTests.csv")
write.csv(Positive, "CountyData/CountyPositive.csv")
write.csv(Recovered, "CountyData/CountyRecovered.csv")
write.csv(Deaths, "CountyData/CountyDeaths.csv")
write.csv(Active, "CountyData/CountyActive.csv")

#############################
## This section is writing today's data into covid19
#############################
mdate = as.character(mydate)

tempT  = CountyData["Total",-c(1,6)]
tempT[1:4] <- sapply(tempT[1:4], as.numeric)
names(tempT) = c("Total.Tested", "positive","Recovered",
                 "deaths", "date")
tempBr =   BremerData[nrow(BremerData),
                      c("Positive", "Recovered","Deaths", "Active" )]
tempBl =   Black.HawkData[nrow(Black.HawkData),
                      c("Positive", "Recovered","Deaths", "Active" )]
tempBu =   ButlerData[nrow(ButlerData),
                      c("Positive", "Recovered","Deaths", "Active" )]
names(tempBr) = c("Bremer.Positive", "Bremer.Recovered",
                  "Bremer.Death", "Bremer.SS") 
names(tempBu) = c("Butler.P", "Butler.R",
                  "Butler.D", "Butler.SS") 
names(tempBl) = c("BlackHawk.P", "BlackHawk.R",
                  "BlackHawk.D", "BlackHawk.SS") 
hosp = as.integer(hospital[nrow(hospital), "Totals"])
names(hosp) = "hospitalized"
temp = cbind(tempT, tempBr, tempBu, tempBl, hosp)
if (covid19[nrow(covid19), "date"] != mydate){
  covid19 <- rbind.fill(covid19, temp)
  } else
  {
    for (i in names(temp)){
      covid19[which(covid19[,"date"] == mydate),i] = temp[1,i]
    }
  }
rm(temp, tempT, tempBl, tempBr, tempBu)
##########################
## This all worked fine, 
## except that there are way too many counties.
##########################

temp = per100[23,-1]
temp = sort(temp, decreasing = TRUE)
names_list = names(temp[1:10])


temp1 = data.frame()
for(i in names_list){
  PC <- per100[, i]
  dates <- Tested[,1] 
  temp = data.frame(date = dates, PC = PC)
  temp = temp %>% mutate(County = i)
  temp1 = rbind(temp1, temp)
  
}
# names_list = c("Bremer",
#                "Black Hawk", "Butler",
#                "Fayette", "Chickasaw")
# 
names_list = c("Floyd")
temp1 = data.frame()

for(i in names_list){
  i1 <- i
    i1 <- gsub(".", " ", i)
    i1 <- gsub("'", ".", i1)
    pos <- as.numeric(Positive[,i1])
    rec <- as.numeric(Recovered[,i1])
    death <- as.numeric(Deaths[,i1])
    dates <- Tested[,1]
    temp <- data.frame(
        date = dates,
        Positive = pos,
        Recovered = rec,
        Deaths = death)

    pop  <- county[
        county$STATE=="Iowa"&county$COUNTY==i,][,3]
    temp <- temp %>%
        mutate(
            Active = Positive - Recovered - Deaths,
            PC = Active/pop*100000,
            County = i
        )
    temp1 = rbind(temp1,temp[,c("date", "County", "PC")])
}

temp1$date  <- as.Date(temp1$date)

qplot(date, PC, color = County, data = temp1,
      # show.legend = FALSE
      geom = "point") +
    geom_smooth()+
    # geom_line()+
    ylab("Cases per 100,000") +
  geom_hline(yintercept = 100, color = "purple")+
  ylab("Cases per 100,000")+
    labs(color = "County", title = "Active Cases Per 100,000")

