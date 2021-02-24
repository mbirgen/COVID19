Summary <- pdf_text("Iowa COVID-19 Information - access.pdf") %>%
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
rm(temp)
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
          date = as.character(mydate))
temp["positivity"] = as.integer(temp["Individuals.Positive"])/as.integer(temp["Individuals.Tested"])
CountyDataOld[101,] = temp

###############################################
# Remove other interesting data
###############################################
temp =Summary[-(22:25)]
SummaryNames = temp[seq(7,37,2)]
SummaryData = temp[seq(8,38,2)] %>% 
    str_remove_all(",") %>%
    as.integer()
SummaryData = data.frame(SummaryData, 
                         row.names = SummaryNames) %>%
    t()
SummaryData <- cbind("Date"=date,SummaryData)
rownames(SummaryData) = date
temp <- read.csv("SummaryData.csv",
         stringsAsFactors = FALSE) %>% as.data.frame()
# rownames(temp) = temp[,1]
colnames(SummaryData) = names(temp)
temp <- rbind(temp, SummaryData) 
write.csv(temp, file = "SummaryData.csv", row.names = FALSE)
rm(temp, temp1)

###################################################
d_county <- t(CountyData[,-1])
county_names <- d_county[1,]
colnames(d_county) <- d_county[1,]
d_county = d_county[-1,]
# colnames(d_county)[1] <- "date"
d_county[-9,] = sapply(d_county[-9,], as.integer)
d_county[9,] = sapply(d_county[9,], as.numeric)
# d_county <- as.data.frame(d_county)%>%
#     mutate(Total = Reduce(`+`, .))
d_county <- cbind(date=as.character(mydate),d_county)
