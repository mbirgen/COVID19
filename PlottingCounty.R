require(reshape2)
mydate <- Sys.Date()

Bremer_Population <- county[county$COUNTY=="Bremer",][,3]
Butler_Population <- county[county$STATE=="Iowa"&county$COUNTY=="Butler",][,3]
BlackHawk_Population <- county[
    county$STATE=="Iowa"&county$COUNTY=="Black Hawk",
    ][,3]

temp = clean[,c("date","Bremer.SS", "Butler.SS", "BlackHawk.SS")]
temp = temp %>% mutate(Bremer = Bremer.SS/Bremer_Population*100000,
                       Butler = Butler.SS/Butler_Population* 100000,
                       BlackHawk = BlackHawk.SS/BlackHawk_Population * 100000)
temp = temp[-(1:71),]
mtemp = melt(temp, id.vars = "date", 
             measure.vars = c('Bremer', 'Butler', 'BlackHawk'))
printplot <- qplot(date, value, color = variable, data = mtemp) +
    ylab("Cases per 100,000") +
    labs(color = "County", title = paste("Active Cases Per 100,000", mydate))

png("BBBCounties.png")
printplot
dev.off()
         