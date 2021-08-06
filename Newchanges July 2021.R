
Summary <- pdf_text(
    "Iowa COVID-19 Information - access04.pdf") %>%
    readr::read_lines() %>% str_squish()
# temp1 = strsplit(Summary[1]," ") %>% ldply()
temp1 = Summary[6] %>% ldply()
date = as.character(mdy(temp1[1]))

Summary <- str_remove_all(Summary, " Translate") %>% 
    str_remove_all("Translate")
CountyDataOld <- Summary[44:149] %>%
    strsplit("(?= [A-Za-z])(?<=[0-9])|(?= [0-9])(?<=[A-Za-z])", perl=TRUE)
CountyDataOld <- CountyDataOld[-c(10:12,72:74)]
CountyDataOld <- plyr::ldply(CountyDataOld)
temp <- strsplit(CountyDataOld[,2], " ", fixed = TRUE)
temp <- plyr::ldply(temp)
temp[,1] = CountyDataOld[,1]
colnames(temp) = c("County", "Individuals.Tested","Individuals.Positive", 
                   "Total.Recovered", "Total.Deaths")
CountyDataOld <- temp
CountyDataOld[-1] = sapply(CountyDataOld[-1], as.integer)
CountyDataOld <- as.data.frame(CountyDataOld) %>%
    arrange(County)%>%
    mutate(Active = Individuals.Positive - Total.Recovered - Total.Deaths,
           positivity = Individuals.Positive/Individuals.Tested,
           date = as.Date(mydate),
    )

temp <-c(County = "Totals",
         colSums(CountyDataOld[,2:6]),
         positivity = NA,
         date = as.character(date))
temp["positivity"] = as.integer(temp["Individuals.Positive"])/as.integer(temp["Individuals.Tested"])
CountyDataOld[101,] = temp
CountyDataOld[,2:6] = sapply(CountyDataOld[,2:6], as.integer)
CountyDataOld[,7] = sapply(CountyDataOld[,7], as.numeric)
write.csv(CountyDataOld, file = paste(
    'CountyData/',date,'CountyData.csv'))

#More interesting information
tempnames =Summary[seq(7,41, 2)]
tempnames = tempnames[-(9:10)]
tempdata =Summary[seq(8,42, 2)]
tempdata = tempdata[-(8:9)]
tempdata = plyr::ldply(as.integer(gsub(",","",tempdata)))
tempdata = t(tempdata)
colnames(tempdata) <- tempnames
tempdata = cbind("date"=date, tempdata)
statedata <- read.csv(paste(
  'CountyData/StateData.csv'),
                   stringsAsFactors = FALSE)
# statedata = rbind(statedata, tempdata)
if (statedata$date[nrow(statedata)] != tempdata[1]){
statedata[nrow(statedata)+1,]=tempdata
}
write.csv(statedata, file = paste(
    'CountyData/StateData.csv'), 
    row.names = FALSE)

##Add data to big csv files
d_county <- t(CountyDataOld)
county_names <- d_county[1,]
colnames(d_county) <- d_county[1,]
d_county = d_county[-1,]
# colnames(d_county)[1] <- "date"
d_county[1:5,] = sapply(d_county[1:5,], as.integer)
# d_county[9,] = sapply(d_county[9,], as.numeric)
# d_county <- as.data.frame(d_county)%>%
#     mutate(Total = Reduce(`+`, .))
d_county <- cbind(date=as.character(date),d_county)

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
templist = c("Tested", "Positive", "Recovered", "Deaths", "Active" )

# Tested <- Tested[-nrow(Tested),]
# Positive <- Positive[-nrow(Positive),]
# Recovered <- Recovered[-nrow(Recovered),]
# Deaths <- Deaths[-nrow(Deaths),]


if(anydate(Tested[nrow(Tested), 1]) != date){
    Tested <- rbind(Tested, d_county["Individuals.Tested",])
    Positive <- rbind(Positive, d_county["Individuals.Positive",])
    Recovered <- rbind(Recovered, d_county["Total.Recovered",])
    Deaths <- rbind(Deaths, d_county["Total.Deaths",])
    Active <- rbind(Active, d_county["Active",])
}

Tested[,-1] = sapply(Tested[,-1], as.integer)
Positive[,-1] = sapply(Positive[,-1], as.integer)
Recovered[,-1] = sapply(Recovered[,-1], as.integer)
Deaths[,-1] = sapply(Deaths[,-1], as.integer)
Active[,-1] = sapply(Active[,-1], as.integer)

trows <- nrow(Tested)
newTest <- Tested[trows,-1]-Tested[trows-1,-1]
newPos <- Positive[trows,-1]-Positive[trows-1,-1]
newRec <- Recovered[trows,-1]-Recovered[trows-1,-1]
newDeath <- Deaths[trows,-1]-Deaths[trows-1,-1]
PerPos7 <- (Positive[trows,-1]-
                Positive[trows-7,-1])/
    (Tested[trows,-1]-
         Tested[trows-7,-1])

county_names <- colnames(Tested)
county_names <- county_names[county_names != "Pending Investigation"]
# county_names <- county_names[-76]
county_names <- gsub(" ", ".", county_names)
dates <- anydate(Tested[,1]    )
if(!("Pending.Investigation" %in% names(hospital))){
    target = which(names(hospital)== "Palo.Alto")
    hospital = cbind(hospital[,1:target,drop=F], data.frame("Pending.Investigation"=""), hospital[,(target+1):length(hospital),drop=F])
}

for(i in 2:(length(county_names))){
    i = county_names[i]
    test <- Tested[,i]
    temp = length(test)
    pos <- as.numeric(Positive[,i])
    rec <- as.numeric(Recovered[,i])
    hosp <- as.integer(hospital[,i])
    hosp = c(rep(NA, length(test)-length(hosp)),hosp)
    death <- as.numeric(Deaths[,i])
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
            Day14.Perc = Day14.Pos/Day14.Test,
            Not.Sick = (lag(Positive, n= 10) -
                            lag(Positive, n=28))*0.7,
            True.Active = Positive - Recovered -
                Deaths - round(Not.Sick, digits = 0)
        )
    
    assign(paste(i,"Data", sep=""), temp)
    # write.csv(temp, paste("CountyData/",county_names[i],
    # "Data.csv", sep=""))
}

write.csv(Tested, "CountyData/CountyTests.csv")
write.csv(Positive, "CountyData/CountyPositive.csv")
write.csv(Recovered, "CountyData/CountyRecovered.csv")
write.csv(Deaths, "CountyData/CountyDeaths.csv")
write.csv(Active, "CountyData/CountyActive.csv")


#####################################
#### Fill in covid19
#####################################

covid19 <- read.csv("covid19.csv", 
                    stringsAsFactors = FALSE)
names(covid19)[names(covid19) == "ï..date"] <- 'date'
covid19$date = anydate(covid19$date)

covid19[nrow(covid19)+1, "date"] = as.character(date)
i=as.Date(date)
covid19[covid19$date == i,
        c("Bremer.Positive", 
          "Bremer.Recovered", "Bremer.Death")]=
    BremerData[BremerData$date==i, 
               c( "Positive", "Recovered", 
                 "Deaths")]

covid19[covid19$date == i,
        c("Butler.P", 
          "Butler.R", "Butler.D")]=
    ButlerData[ButlerData$date==i, 
               c("Positive", "Recovered", 
                 "Deaths")]
covid19[covid19$date == i,
        c("BlackHawk.P", 
          "BlackHawk.R", "BlackHawk.D")]=
    Black.HawkData[Black.HawkData$date==i, 
               c("Positive", "Recovered", 
                 "Deaths")]


    covid19[covid19$date == as.Date(date),"Total.Tested"] = 
        tempdata[1,"Individuals Tested"]
    
    covid19[covid19$date == as.Date(date),"positive"] = 
        tempdata[1,"Total Confirmed Cases"]
    covid19[covid19$date == as.Date(date),"Recovered"] = 
        tempdata[1,"Recovering"]
    covid19[covid19$date == as.Date(date),"deaths"] = 
        tempdata[1,"Deceased"]
    covid19[covid19$date == as.Date(date),"hospitalized"] = 
        tempdata[1,"Hospitalized"]
    covid19[covid19$date == as.Date(date),"ICU"] = 
        tempdata[1,"COVID-19 Patients in ICU"]
    covid19[covid19$date == as.Date(date),"Admit"] = 
        tempdata[1,"COVID-19 Patients Admitted in last 24 hrs"]
    
    # covid19[covid19$date == temp1[i],"positive"] = 
    #     Positive[Positive$date == temp[i],"Totals"]
    # covid19[covid19$date == temp1[i],"Recovered"] = 
    #     Recovered[Recovered$date == temp[i],"Totals"]
    # covid19[covid19$date == temp1[i],"deaths"] = 
    #     Deaths[Deaths$date == temp[i],"Totals"]

covid19[-1] = sapply(covid19[,-1], as.numeric)
covid19 = covid19 %>%
  mutate(Bremer.SS = Bremer.Positive-Bremer.Death - Bremer.Recovered,
         Butler.SS = Butler.P - Butler.R,
         BlackHawk.SS = BlackHawk.P- BlackHawk.D - BlackHawk.R)

write.csv(covid19, "covid19.csv",row.names = FALSE)

#############################################
###Print Hospital

qplot(as.Date(date), hospitalized, 
      data = tail(clean, n=90),
      geom = c("point", "smooth")) + 
  labs(title = "Patients Hospitalized with 
       COVID-19 in Iowa")
