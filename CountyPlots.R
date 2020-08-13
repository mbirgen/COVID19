names_list <- CountyData[,1]
names_list <- names_list[-75]
names_list <- names_list[-100]

for(i in 1:length(names_list)){
   pop[i] <- county[
        county$STATE=="Iowa"&county$COUNTY==names_list[i],][,3]   
}

temp1 = data.frame()
for(i in names_list){
    temp1 = data.frame()
    i1 <- gsub(" ", ".", i)
    i1 <- gsub("'", ".", i1)
    pos <- as.numeric(Positive[,i1])
    rec <- as.numeric(Recovered[,i1])
    death <- as.numeric(Deaths[,i1])
    dates <- Tested[,1]  
    temp <- data.frame(
        date = dates,
        Positive = pos,
        Recovered = rec,
        Deaths = death)
    
    pop  <- county[
        county$STATE=="Iowa"&county$COUNTY==i,][,3]
    temp <- temp %>% 
        mutate(
            Active = Positive - Recovered - Deaths,
            PC = Active/pop*100000,
            County = i
        )
    temp1 = rbind(temp1,temp[,c("date", "County", "PC")])
    temp1$date  <- as.Date(temp1$date)
    printplot <- qplot(date, PC, color = County, data = temp1,
                       # show.legend = FALSE
                       geom = "point") +
        geom_smooth()+
        # geom_line()+
        ylab("Cases per 100,000") +
        labs(color = "County", title = "Active Cases Per 100,000")
    
    png(paste("CountyPlot/",i,".png", sep = ""))
    print(printplot)
    dev.off()
}



