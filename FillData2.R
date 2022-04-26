covid19_1 =  covid19 %>% 
    complete(date = 
                 seq.Date(min(covid19$date),
                          max(covid19$date), 
                          by = "day")
             ) %>%
    fill(`positive`,`Recovered`,`deaths`, 
         "Still.Sick", "hospitalized", 
         `Bremer.Positive`,`Butler.P`, 
         `BlackHawk.P`, `Bremer.Death`, 
         `Butler.D`, `BlackHawk.D`) %>%
    mutate(
        Recovered = lag(positive, n=28) - lag(deaths, n=28),
        Bremer.Recovered = lag(Bremer.Positive, n=28) - lag(Bremer.Death, n=28),
        Butler.R = lag(Butler.P, n=28) - lag(Butler.D, n=28),
        BlackHawk.R = lag(BlackHawk.P, n=28) - lag(BlackHawk.D, n=28)
        )

covid19_1 = covid19_1 %>%
mutate(Bremer.SS = Bremer.Positive-Bremer.Death - Bremer.Recovered,
Butler.SS = Butler.P - Butler.R,
BlackHawk.SS = BlackHawk.P- BlackHawk.D - BlackHawk.R)
