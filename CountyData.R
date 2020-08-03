mydate <- Sys.Date()
filename <- "C:\\Users\\mariah.birgen\\Downloads\\Summary.csv"
CountyData <- read.csv(filename, 
                       stringsAsFactors = FALSE)

CountyData <- as.data.frame(CountyData) %>%
    arrange(EventResidentCounty) %>%
    mutate(positivity = Individuals.Positive/Individuals.Tested,
           date = as.Date(mydate))
rownames(CountyData) <- CountyData[,1]
temp <-c( EventResidentCounty = "Totals",
          colSums(CountyData[,2:5]),
          positivity = NA,
          date = as.character(mydate))
CountyData <- rbind(CountyData, Total = temp)
# CountyData <-cbind(date = as.Date(mydate), CountyData)
write.csv(CountyData, file = 'CountyData/CountyData.csv', append = TRUE)

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

if( as.character(Tested[nrow(Tested), 1]) != mydate){
Tested <- rbind(Tested, d_county[1,])
Positive <- rbind(Positive, d_county[2,])
Recovered <- rbind(Recovered, d_county[3,])
Deaths <- rbind(Deaths, d_county[4,])
}

trows <- nrow(Tested)
newTest <- as.integer(Tested[trows,-1])-as.integer(Tested[trows-1,-1])
newPos <- as.integer(Positive[trows,-1])-as.integer(Positive[trows-1,-1])
newRec <- as.integer(Recovered[trows,-1])-as.integer(Recovered[trows-1,-1])
newDeath <- as.integer(Deaths[trows,-1])-as.integer(Deaths[trows-1,-1])

NewToday <- rbind(newTest, newPos, newRec, newDeath)
NewToday <- rbind(Active = as.integer(Positive[trows,-1]) - 
                      as.numeric(Recovered[trows,-1]) -
                      as.numeric(Deaths[trows,-1]),
                  NewToday, PerPos = newPos/newTest)
colnames(NewToday) <- county_names[1:101]

# for(i in county_names){
#     # county <- get(paste(i,"Data", sep=""))
#     # temp <- CountyData[i,]
#     # rownames(temp) <- mydate
#     # temp <- rbind(county, temp)
#     # assign(paste(i,"Data", sep=""), temp)
#     rm(paste(i,"Data", sep=""))
# }

county_names <- colnames(Tested)
county_names <- gsub(" ", ".", county_names)
for(i in 2:(length(county_names))){
    test <- as.numeric(Tested[,i])
    pos <- as.numeric(Positive[,i])
    rec <- as.numeric(Recovered[,i])
    death <- as.numeric(Deaths[,i])
    dates <- Tested[,1]    
    temp <- data.frame(
        date = dates,
        Tested = test,
        Positive = pos,
        Recovered = rec,
        Deaths = death)
    
    temp <- temp %>% 
        mutate(
            Active = Positive - Recovered - Deaths,
            New.Pos = Positive - lag(Positive),
            New.Test = Tested - lag(Tested),
            New.Deaths = Deaths - lag(Deaths),
            Frac.Pos = New.Pos/New.Test
            )
    
    assign(paste(county_names[i],"Data", sep=""), temp)
    # write.csv(temp, paste("CountyData/",county_names[i],
                          # "Data.csv", sep=""))
}

write.csv(Tested, "CountyData/CountyTests.csv")
write.csv(Positive, "CountyData/CountyPositive.csv")
write.csv(Recovered, "CountyData/CountyRecovered.csv")
write.csv(Deaths, "CountyData/CountyDeaths.csv")

