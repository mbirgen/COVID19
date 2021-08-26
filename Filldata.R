date = seq.Date(min(covid19$date), 
                max(covid19$date), by = "day")
temp = covid19[, 1:6] %>%
    complete(
        date = seq.Date(min(covid19$date), 
                        max(covid19$date), by = "day")) %>%
    fill("Total.Tested", "positive", "Recovered", "deaths",
         "hospitalized") 
temp[,-1] = sapply(temp[,-1], as.integer)
# names(temp) = c("date", "Tested", "Positive", 
#                "Recovered", "Deaths", "Hospitalized")
temp = temp %>% mutate(
    negative = Total.Tested - positive,
    New.Positive = positive - lag(positive),
    New.Negative = negative - lag(negative),
    new.hospital = hospitalized - lag(hospitalized),
    Total.Daily.Tests = Total.Tested - lag(Total.Tested),
    New.Deaths = deaths - lag(deaths),
    Still.Sick = positive - Recovered) 
temp = temp%>% mutate(
        New.Percent.Positive = New.Positive/Total.Daily.Tests,
        Week.Positivity.Rate = (positive - lag(positive, n=7))/(Total.Tested - lag(Total.Tested, n=7)),
        Percent.Hospitalized = hospitalized/Still.Sick
    )%>%
    mutate(
        negative = Total.Tested - positive,
        Percent.Pos = positive/Total.Tested*100,
        #np_7day = rollmean(New.Positive, k=7, align = "right", fill = NA),
        np_7day = (positive - lag(positive, n=7))/7,
        New.Percent.Positive = New.Positive/Total.Daily.Tests,
        Percent.Hospitalized = hospitalized/Still.Sick*100
    )%>% mutate(
        Week.Positivity = positive - 
            lag(positive, n=7),
        Week.Tests = Total.Tested - 
            lag(Total.Tested, n=7),
        Week.Positivity.Rate = Week.Positivity/Week.Tests,
        Week.Deaths = deaths - lag(deaths, n=7),
        Day.Total.Tests = Total.Tested - 
            lag(Total.Tested,n=1)
        ) %>% mutate(
        wktot = Week.Positivity,
    pp_7day = (positive - lag(positive, n=7))/ (Total.Tested - lag(Total.Tested, n=7))#
)
    # mutate(
    #     Active = Positive - Recovered - Deaths,
    #     New.Pos = Positive - lag(Positive),
    #     New.Test = Tested - lag(Tested),
    #     New.Rec = Recovered - lag(Recovered),
    #     New.Deaths = Deaths - lag(Deaths),
    #     Day7.Test = Tested - lag(Tested, n=7),
    #     Day7.Pos = Positive - lag(Positive, n=7),
    #     Day7.Rec = Recovered - lag(Recovered, n=7),
    #     Day7.Death = Deaths - lag(Deaths, n=7),
    #     Day7.Active = Active - lag(Active, n=7),
    #     Day14.Test = Tested - lag(Tested, n=14),
    #     Day14.Pos = Positive - lag(Positive, n=14),
    #     Day14.Rec = Recovered - lag(Recovered, n=14),
    #     Day14.Death = Deaths - lag(Deaths, n=14),
    #     Day14.Active = Active - lag(Active, n=14)
    #     )

good_dates = as.data.frame(covid19$date)
names(good_dates) = "date"
temp2 = merge(good_dates, temp, by = "date")
nam = names(temp2[,-(1:6)])
covid19 = cbind(covid19, temp2[,nam])


################################################
##############  Scribbling
names1 = names(statedata)
names1 = names1[-1]
paste0("New.", names1)
New <- function(x, na.rm = FALSE){(as.numeric(x) - lag(as.numeric(x)))}

statedata = statedata %>% mutate(
    New.Total.Confirmed.Cases = 
        New(Total.Confirmed.Cases),
    New.Deceased = New(Deceased),
    New.Hospitalized = New(Hospitalized),
    New.Recovering= New(Recovering),
    New.Child.Cases..0.17. = New(Child.Cases..0.17.),
    New.Adult.Cases..18.40. = New(Adult.Cases..18.40.),
    New.Middle.Aged.Cases..41.60.= New(Middle.Aged.Cases..41.60.),
    New.Older.Adult.Cases..61.80.= New(Older.Adult.Cases..61.80.),
    New.Elderly.Cases..81.or.Older. = New(Elderly.Cases..81.or.Older.),
    New.Male.Cases = New(Male.Cases),
    New.Female.Cases = New(Female.Cases),
    New.Individuals.Tested = New(Individuals.Tested),
    New.COVID.19.Patients.Admitted.in.last.24.hrs =
        New(COVID.19.Patients.Admitted.in.last.24.hrs),
    New.COVID.19.Patients.in.ICU =
        New(COVID.19.Patients.in.ICU),
    New.Total.Ventilators.Available =
        New(Total.Ventilators.Available),
    New(COVID.19.Patients.on.Ventilators)
)


########################################
###### More Calculating Re??   #########
########################################

Re <- function(R0,efficacy,population,vaccinated){
    R0*(1-(vaccinated/population)*efficacy)
}

########################################
###### Calculating Risk ################
########################################

Risk1 <- function(active, population, n){
    1-(1-active*5/population)^n
}

Risk2 <- function(active, population, 
                  vaccinepercentage, n, 
                  groupvaccinepercentage){
    1-(1-(active*5/population)*
       (3-2*groupvaccinepercentage)/
       (3-2*vaccinepercentage))^n
}
