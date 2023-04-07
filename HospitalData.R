require(dplyr)
require(lubridate)
require(plyr)
require(tidyverse)
require(anytime)
date = Sys.Date()
covid19 <- read.csv("covid19.csv", 
                    stringsAsFactors = FALSE)
names(covid19)[1] <- "date"
require(anytime)

# URL = "https://healthdata.gov/resource/6xf2-c3ie.csv"
URL ="https://healthdata.gov/api/views/6xf2-c3ie/rows.csv"
inpatient = read.csv(URL, na.strings = c("-999999","-999999.0"))
inpatient = inpatient %>% filter(state == "IA") %>%
    select(contains("covid")) %>% select(-contains("coverage"))
temp = cbind(date =as.Date(date), inpatient)
inpatientSave <- read.csv("HospitalData.csv", stringsAsFactors = FALSE)
# inpatientSave = read.csv("HospitalData.csv")
# HospitalFed = as.data.frame(temp)
# HospitalFed = rbind(HospitalFed, temp)
if(sum(grepl(date, covid19$date)) == 0){
    inpatientSave[nrow(inpatientSave)+1,] = temp
    inpatientSave$date[nrow(inpatientSave)] = as.character(date)
    covid19[nrow(covid19)+1, "date"] = as.character(date)
} else
{
    inpatientSave[inpatientSave$date == date,] = temp
    inpatientSave$date[nrow(inpatientSave)] = as.character(date)
}
inpatientSave = inpatientSave %>% unique() %>% 
    mutate(total_ICU= staffed_icu_adult_patients_confirmed_covid + 
    staffed_icu_pediatric_patients_confirmed_covid)

write.csv(inpatientSave, "HospitalData.csv",row.names = FALSE)
covid19[covid19$date == as.Date(date),c("hospitalized", 
                                        "new.hospital","ICU")] = 
    inpatient[c("inpatient_beds_used_covid", 
                "previous_day_admission_adult_covid_confirmed", 
                "staffed_icu_adult_patients_confirmed_and_suspected_covid")]
covid19[covid19$date == as.Date(date),"Admit"] = 
    sum((inpatient %>% select(contains("previous_day")))[1:4])    
covid19 = covid19 %>% unique()
write.csv(covid19, "covid19.csv",row.names = FALSE, na="")
