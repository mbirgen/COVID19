temp = clean[,c("date","Bremer.SS", "Butler.SS", "BlackHawk.SS")]
temp = temp %>% mutate(Bremer = Bremer.SS/Bremer_Population*100000,
                       Butler = Butler.SS/Butler_Population* 100000,
                       BlackHawk = BlackHawk.SS/BlackHawk_Population * 100000)
temp = temp[-(1:71),]
mtemp = melt(temp, id.vars = "date", 
             measure.vars = c('Bremer', 'Butler', 'BlackHawk'))
qplot(date, value, color = variable, data = mtemp) +
    ylab("Active Cases per 100,000") +
    labs(color = "County")
