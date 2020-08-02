mydate <- Sys.Date()
filename <- "C:\\Users\\mariah.birgen\\Downloads\\Summary.csv"
CountyData <- read.csv(filename)
CountyData <- as.data.frame(CountyData) %>%
    arrange(EventResidentCounty) %>%
    mutate(positivity = Individuals.Positive/Individuals.Tested,
           date = as.Date(mydate))
rownames(CountyData) <- CountyData[,1]
temp <-c( EventResidentCounty = NA,
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
d_county <- cbind(date =as.character(mydate),d_county)

Tested <- read.csv( "CountyTests.csv", stringsAsFactors=FALSE)
Tested <- Tested[,-1]
Positive <- read.csv( "CountyPositive.csv", stringsAsFactors=FALSE)
Positive <- Positive[,-1]
Recovered <- read.csv( "CountyRecovered.csv", stringsAsFactors=FALSE)
Recovered <- Recovered[,-1]
Deaths <- read.csv( "CountyDeaths.csv", stringsAsFactors=FALSE)
Deaths <- Deaths[,-1]

if( as.character(Tested[nrow(Tested), 1]) != mydate){
Tested <- rbind(Tested, d_county[1,])
Positive <- rbind(Positive, d_county[2,])
Recovered <- rbind(Recovered, d_county[3,])
Deaths <- rbind(Deaths, d_county[4,])
}

# for(i in county_names){
#     # county <- get(paste(i,"Data", sep=""))
#     # temp <- CountyData[i,]
#     # rownames(temp) <- mydate
#     # temp <- rbind(county, temp)
#     # assign(paste(i,"Data", sep=""), temp)
#     rm(paste(i,"Data", sep=""))
# }
#####################################
# Problem to solve: there is an aphostraphe in O'Brien
#########################################

county_names <- gsub(" ", ".", county_names)
for(i in 2:length(county_names)){
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
    write.csv(temp, paste("CountyData/",county_names[i],
                          "Data.csv", sep=""))
}

write.csv(Tested, "CountyData/CountyTests.csv")
write.csv(Positive, "CountyData/CountyPositive.csv")
write.csv(Recovered, "CountyData/CountyRecovered.csv")
write.csv(Deaths, "CountyData/CountyDeaths.csv")

