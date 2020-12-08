Summary <- pdf_text("Iowa COVID-19 Information - access.pdf") %>%
    readr::read_lines() %>% str_squish()
temp1 = strsplit(Summary[1]," ") %>% ldply()
date = as.character(mdy(temp1[1]))

Summary <- str_remove_all(Summary, " Translate") %>% 
    str_remove_all("Translate")
CountyData <- Summary[44:149] %>%
    strsplit("(?= [A-Za-z])(?<=[0-9])|(?= [0-9])(?<=[A-Za-z])", perl=TRUE)
CountyData <- CountyData[-c(10:12,72:74)]
CountyData <- plyr::ldply(CountyData)
temp <- strsplit(CountyData[,2], " ", fixed = TRUE)
temp <- plyr::ldply(temp)
temp[,1] = CountyData[,1]
colnames(temp) = c("County", "Individuals.Tested","Individuals.Positive", 
                   "Total.Recovered", "Total.Deaths")
CountyData <- temp
rm(temp)
CountyData[-1] = sapply(CountyData[-1], as.integer)
CountyData <- as.data.frame(CountyData) %>%
    arrange(County)%>%
    mutate(Active = Individuals.Positive - Total.Recovered - Total.Deaths,
           positivity = Individuals.Positive/Individuals.Tested,
           date = as.Date(mydate),
    )

temp <-c(County = "Totals",
          colSums(CountyData[,2:6]),
          positivity = NA,
          date = as.character(mydate))
temp["positivity"] = as.integer(temp["Individuals.Positive"])/as.integer(temp["Individuals.Tested"])
CountyData[101,] = temp

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
colnames(SummaryData) = names(temp[,-1])
temp <- rbind(temp, SummaryData) 
write.csv(temp, file = "SummaryData.csv", row.names = FALSE)
rm(temp)
