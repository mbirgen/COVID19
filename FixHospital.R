require(plyr)
require(pdftools)
require(tidyverse)

hosp_by_county <- pdf_text("hosp_by_county.pdf") %>%
    readr::read_lines() %>% 
    str_squish()
temp <- hosp_by_county
hosp_by_county <- temp[5:37]
temp1 = strsplit(temp[40]," ") %>% ldply()
date = as.character(mdy(temp1[3]))
# date = "8/6/20"
# hosp_by_county <- temp[46:78]
# date = "8/7/20"
# hosp_by_county <- temp[87:119]
# date = "8/8/20"
hosp_by_county <-hosp_by_county %>%
    strsplit("(?= [A-Za-z])(?<=[0-9])|(?= [0-9])(?<=[A-Za-z])", perl=TRUE)

hosp_by_county_df <- plyr::ldply(hosp_by_county)
temp1 <- hosp_by_county_df[,1:2]
temp2 <- hosp_by_county_df[,3:4]
temp3 <- hosp_by_county_df[,5:6]
colnames(temp2) <- names(temp1)
colnames(temp3) <- names(temp1)
hosp_by_county <- rbind(temp1, temp2, temp3)
colnames(hosp_by_county) <- c("County", "Hospitalized")
hosp_by_county <-hosp_by_county %>%
    mutate(Hospitalized = as.numeric(Hospitalized))
temp1 = sum(hosp_by_county[,2])
hosp_by_county <- rbind(hosp_by_county, c("Totals", temp1))
rm(temp1, temp2, temp3, hosp_by_county_df)

rownames(hosp_by_county) <- hosp_by_county[,1]
# hosp_by_county <- cbind(date = c("2020-08-06", hosp_by_county))

hospital <- as.data.frame(read.csv("CountyHospitilizations.csv"))
hospital <- hospital[,-1]

h_county <-  t(hosp_by_county[,2])

colnames(h_county) <-  hosp_by_county[,1]
h_county <- cbind(date = c(date, h_county))
rownames(h_county) <- colnames(hospital)
h_county <- t(h_county)
# rownames(h_county) <- as.character(day)
if(as.character(hospital[nrow(hospital),1])!= day){
    hospital <- rbind(hospital, h_county)
    }

write.csv(hospital, "CountyHospitilizations.csv")
