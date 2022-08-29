##Add data to big csv files
temp = c(County = "Totals",
         as.integer(colSums(TestsNew[2:4])))
# temp[2:3] = sapply(temp[2:3], as.integer)
TestsNew1 = TestsNew %>%
    select("County", "Total.Positive.Tests","Total.Positive.Cases", "Total.Deaths")%>%
    rbind(temp)

TestsNew1 =rbind("date"= as.character(date),TestsNew1)
d_county <- t(TestsNew1)

colnames(d_county) <- d_county[1,]
d_county = d_county[-1,]
colnames(d_county)[1] <- "date"
d_county[,-1] = sapply(d_county[,-1], as.integer)

Positive <- read.csv( "CountyData/CountyPositive.csv", stringsAsFactors=FALSE)
Positive <- Positive[,-1]

Positive.Tests <- read.csv( "CountyData/CountyPositive.csv", stringsAsFactors=FALSE)
Positive.Tests <- Positive.Tests[,-1]

Deaths <- read.csv( "CountyData/CountyDeaths.csv", stringsAsFactors=FALSE)
Deaths <- Deaths[,-1]

templist = c(
    "Positive", 
    "Deaths"
)
# 
# Positive <- Positive[-nrow(Positive),]
# Positive.Tests <- Positive.Tests[-nrow(Positive.Tests),]
# Deaths <- Deaths[-nrow(Deaths),]


if(anydate(Positive[nrow(Positive), 1]) != date){
    Positive <- rbind(Positive, d_county["Total.Positive.Cases",])
    Deaths <- rbind(Deaths, d_county["Total.Deaths",])
    Positive.Tests <- rbind(Positive.Tests, d_county["Total.Positive.Tests",])
} else{
   Positive[Positive$date == date,] =  d_county["Total.Positive.Cases",]
    #  Positive[nrow(Positive),]=  d_county["Total.Positive.Cases",]
    # # Recovered[Recovered$date==date,] =  d_county["Total.Recovered",]
    Deaths[nrow(Deaths),] = d_county["Total.Deaths",]
    # Active[Active$date == date,] =  d_county["Active",]
    Positive.Tests[Positive.Tests$date == date,] =  d_county["Total.Positive.Tests",]
}

# Tested[,-1] = sapply(Tested[,-1], as.integer)
Positive[,-1] = sapply(Positive[,-1], as.integer)
Positive.Tests[,-1] = sapply(Positive.Tests[,-1], as.integer)
# Recovered[,-1] = sapply(Recovered[,-1], as.integer)
Deaths[,-1] = sapply(Deaths[,-1], as.integer)
# Active[,-1] = sapply(Active[,-1], as.integer)

trows <- nrow(Positive)
# newTest <- Tested[trows,-1]-Tested[trows-1,-1]
newPos <- Positive[trows,-1]-Positive[trows-1,-1]
# newRec <- Recovered[trows,-1]-Recovered[trows-1,-1]
newDeath <- Deaths[trows,-1]-Deaths[trows-1,-1]
# PerPos7 <- (Positive[trows,-1]-
#                 Positive[trows-7,-1])/
#     (Tested[trows,-1]-
#          Tested[trows-7,-1])

Positive[,102]=rowSums(Positive[,2:101], na.rm=TRUE)
Positive.Tests[,102]=rowSums(Positive.Tests[,2:101], na.rm=TRUE)

for(i in (nrow(Recovered2)+1):nrow(Positive)){
    tempday = Positive$date[i]
    lastmonth = closestdate2(Positive,tempday,28)
    tempPos = Positive[lastmonth,]
    tempDeath = Deaths[i,]
    tempRec = tempPos[,-1] - tempDeath[,-1]
    tempRec = cbind(date = as.character(tempday), tempRec)
    Recovered2 = rbind(Recovered2,tempRec)
}

county_names <- colnames(Positive)
county_names <- county_names[county_names != "Pending Investigation"]
county_names <- county_names[county_names != "Unknown"]
county_names <- county_names[county_names != "Pending.Investigation"]
# county_names <- county_names[-76]
county_names <- gsub(" ", ".", county_names)
county_names <- gsub("'", ".", county_names)
dates <- anydate(Positive[,1]    )
Positive$date = dates
OneMonthAgo = closestdate(Positive, 28)
# if(!("Pending.Investigation" %in% names(hospital))){
#     target = which(names(hospital)== "Palo.Alto")
#     hospital = cbind(hospital[,1:target,drop=F], data.frame("Pending.Investigation"=""), hospital[,(target+1):length(hospital),drop=F])
# }

for(i in 2:(length(county_names))){
    i = county_names[i]
    # test <- Tested[,i]
    pos <- as.integer(Positive[,i])
    temp = length(pos)
    postest <- as.integer(Positive.Tests[,i])
    postest = c(rep(NA, length(pos)-length(postest)),postest)
    rec <- as.numeric(Recovered2[,i])
    # hosp <- as.integer(hospital[,i])
    # hosp = c(rep(NA, length(test)-length(hosp)),hosp)
    death <- as.numeric(Deaths[,i])
    temp <- data.frame(
        date = dates,
        TestedPos = postest,
        Positive = pos,
        Recovered = rec,
        # Hospitalized = hosp,
        Deaths = death) 
    
    temp <- temp %>%
        mutate(
            Active = Positive - Recovered - Deaths,
            New.Pos = Positive - lag(Positive),
            #         New.Test = Tested - lag(Tested),
            New.Rec = Recovered - lag(Recovered),
            New.Deaths = Deaths - lag(Deaths),
            # True.Active = Positive - lag(Positive, n=2) -
            Deaths
        )
    
    assign(paste(i,"Data", sep=""), temp)
    
}