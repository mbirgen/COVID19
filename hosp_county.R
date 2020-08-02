#This is created to download the file 
# from coronavirus.iowa.gov that contains 
# hospitilizations by county which is a pdf file
# and turn it into a data frame.

# install.packages("pdftools")
library(pdftools)
library(tidyverse)
# library(tabulizer)
# library(rJava)

mydate <- Sys.Date() -2  # Get today's date minus 2 days
myprettydate <- format(mydate, format = "%d%b%Y")

# pdffilename <- "https://www.homelandsecurity.iowa.gov/documents/covid-site/IA_C19_23JUL2020.pdf"
pdffilename <- paste("https://www.homelandsecurity.iowa.gov/documents/covid-site/IA_C19_", 
                     myprettydate, ".pdf", sep="")
download.file(pdffilename, 'hosp_by_county.pdf', mode="wb")
# tempfile <- "C:\\Users\\mariah.birgen\\Dropbox\\2019-2020\\Winter 2020\\COVID19\\hosp_by.county.pdf"
# hosp_by_county2 <- extract_tables(file = tempfile,
#                                   method = "decide",
#                                   output = "data.frame")
hosp_by_county <- pdf_text("hosp_by_county.pdf") %>%
    readr::read_lines() %>% 
    str_squish()
hosp_by_county <- hosp_by_county[-(1:4)]
hosp_by_county <- hosp_by_county[-(34:36)]
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
rm(temp1, temp2, temp3, hosp_by_county_df)
rownames(hosp_by_county) <- hosp_by_county[,1]
