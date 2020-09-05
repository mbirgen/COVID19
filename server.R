
library(shiny)
suppressPackageStartupMessages(require(plotly))


shinyServer(function(input, output, session) {
    
    # Data Cleaning
    selected1 <- reactive({
        filter(countiesRaw, state == input$stateChoice)
    })
    
    selected <- reactive({
        filter(selected1(), date == input$date)
        
    })
    statesRawN1 <- reactive({
        
        s <- stringr::str_sub(JHdata, end = -15)
        date <- format(input$date, "%m-%d-%Y")
        s <- paste(s,date,".csv", sep="")
        s <- data.frame(read.csv(s))
        s <- filter(s, Province_State == input$stateChoice)
        if (length(s)>0){
            return(s)
        } 
    })
    censusCountiesRaw <- reactive({
        censusCounty1 <- filter(censusCounty, STATE==input$stateChoice)
        data <- selected()
        
        data <- left_join(censusCounty1, selected(), by=c("COUNTY"="county"))
        data1<- data %>% mutate(perCap = cases/Population)
        
        
        state <- counties(state = input$stateChoice)
        
        stateData <- geo_join(state,data1, "GEOID", "fips")
        

    })
    observeEvent(
        input$stateChoice, 
        updateSliderInput(session,"date","Date:",
                          min=Sys.Date()-31,
                          max=max(selected1()$date),
                          value = max(selected1()$date),
                          timeFormat="%m-%d-%Y")
        )
    
    # 7 Day Average Chart 
    plotReady <- reactive({
        
        plot1 <- filter(statesRawN, state == input$stateChoice)
        Daily_Positive <- c(abs(diff(plot1$positive)),0)
        plot1<- cbind(plot1, Daily_Positive)
        
        names(plot1)[4] <- "Daily_Positive"
        file1 <- plot1 %>% select(date, Daily_Positive) %>%
            mutate(pRate7 = rollmean(Daily_Positive, k=7, fill = NA, align = "left"))
        file1 <- file1[!is.na(file1$pRate7),]})
        
    
    x <- list(
        title="Date"
    )
    y <- list(
        title="New Positives"
    )
    output$av_week <- renderPlotly(
    
    plotly:: plot_ly(plotReady(), 
                     hoverinfo="text", showlegend=F) %>%
        plotly::add_bars(x=~date,y=~Daily_Positive, 
                         hovertext=paste("Date: ", plotReady()$date,
                                         "<br>New Positives: ", plotReady()$Daily_Positive
                         )) %>%
        plotly::add_lines(x=~date,y~pRate7, 
                          hovertext=paste("Date: ", plotReady()$date,
                                    "<br>7 Day Avg: ", round(plotReady()$pRate7)
                                    )) %>%
        plotly::layout(xaxis=x,yaxis=y))
    
    # Cases Map
    zoomData <- reactive({
        states_df <- states()
        subset(states_df, NAME %in%  input$stateChoice)
    })
    
    output$map <- renderLeaflet(
        leaflet("map") %>% addTiles("CartoDB.Positron") 
    )
    countiesMap <- reactive({
        counties(cb=T,state=input$stateChoice)
        
    })
    statesMerged <-reactive({
        s <- geo_join(countiesMap(), selected(),"GEOID", "fips")
    })

    # Leaflet Outputs
    observe({if (input$Condition=="Total Positives"){
        pal <- colorBin("Reds", statesMerged()$cases, bins = 6)   
        leafletProxy("map", data = selected()) %>% clearShapes()%>%
            setView(as.numeric(zoomData()$INTPTLON), 
                    as.numeric(zoomData()$INTPTLAT), zoom = 6) %>%
            addPolygons(
                data=statesMerged(),
                fillOpacity = 1,
                stroke = TRUE,
                weight = 1,
                smoothFactor = .5,
                fillColor = ~pal(statesMerged()$cases),
                label = paste0(statesMerged()$NAME,": ",statesMerged()$cases),
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding="3px 8px"),
                    textsize = "12px",
                    direction = "auto")
            )}else if (input$Condition=="Positives per Capita"){
                pal <- colorBin("Reds", censusCountiesRaw()$perCap, bins = 6)
                
                leafletProxy("map", data = censusCountiesRaw()) %>% clearShapes()%>%
                    setView(as.numeric(zoomData()$INTPTLON), 
                            as.numeric(zoomData()$INTPTLAT), zoom = 6)%>%
                    addPolygons(
                        data=censusCountiesRaw(),
                        fillOpacity = 1,
                        stroke = TRUE,
                        weight = 1,
                        smoothFactor = .5,
                        fillColor = ~pal(censusCountiesRaw()$perCap),
                        label = paste0(censusCountiesRaw()$NAME,": ", round(censusCountiesRaw()$perCap, 4)),
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding="3px 8px"),
                            textsize = "12px",
                            direction = "auto")
            )}
    })
    
    observe({if(input$Condition=="Total Positives"){
        pal <- colorBin("Reds", statesMerged()$cases, bins = 6)
        proxy <- leafletProxy("map", data= statesMerged())
        proxy %>% clearControls()
        if (input$legend){
            proxy %>% addLegend("bottomright", title= "Positive Cases",pal=pal, 
                                values=statesMerged()$cases, na.label = "No Data")
        }} else if (input$Condition=="Positives per Capita"){
            pal <- colorBin("Reds", censusCountiesRaw()$perCap, bins = 6)
            proxy <- leafletProxy("map", data= censusCountiesRaw())
            proxy %>% clearControls()
            if (input$legend){
                proxy %>% addLegend("bottomright", title= "Positive Cases",pal=pal, 
                                    values=censusCountiesRaw()$perCap, na.label = "No Data")
        }
    }})
    
    # Info Boxes
    weekGrowth <- reactive({
        plot1 <- filter(statesRawN, state == input$stateChoice)
        Daily_Positive <- c(abs(diff(plot1$positive)),0)
        plot1<- cbind(plot1, Daily_Positive)
        
        names(plot1)[4] <- "Daily_Positive"
        file1 <- plot1 %>% select(date, Daily_Positive) %>%
            mutate(pRate7 = rollmean(Daily_Positive, k=7, align = "left",fill = NA ))
        
        D1 <- filter(file1, date==input$date)
        D2 <- filter(file1, date==input$date-7)
        
        wGrowth <- ((D1[,3]/D2[,3]) - 1) * 100
        
        if(length(wGrowth)>0){
        return(round(wGrowth,1))}
        else{
            return(FALSE)
        }
       
    })
    
    
    
    # Info Box Outputs
    output$stateDate <- renderInfoBox(
        infoBox("Date", input$date, icon=icon("calendar")))
    output$statePos <- renderValueBox(
        valueBox(statesRawN1()[,6], "Positive Cases", icon=icon("line-chart"))
        
    )
    output$stateDeaths <- renderValueBox(
        valueBox(statesRawN1()[,7], "Deaths", icon=icon("heart"), color="orange")
    )
    output$stateRecovered <- renderValueBox(
        if (length(statesRawN1()[,8])>0){
        valueBox(statesRawN1()[,8], "Recovered", icon=icon("medkit"), color = "green")
    } else{
        valueBox("Unknown", "Recovered", icon=icon("medkit"), color = "olive")
    })
    output$stateHospital <- renderValueBox(
        if (length(statesRawN1()[,13])>0){
            valueBox(statesRawN1()[,13], "Hospitalized", icon=icon("ambulance"), color = "maroon")
        } else{
            valueBox("Unknown", "Hospitalized", icon=icon("ambulance"), color = "maroon")
        })
    output$stateInfected <- renderValueBox(
        if (length(statesRawN1()[,9])>0 && !is.na(statesRawN1()[,8])){
            valueBox((statesRawN1()[,9]), 
                     "Active Cases", icon=icon("heartbeat"), color = "red")
        } else{
            valueBox("Unknown", "Active Cases", icon=icon("medkit"), color = "red")
        })
    output$weekPGrowth <- renderValueBox(
        
    if(weekGrowth()!=F){
        if(10>=weekGrowth() && weekGrowth() >= 0){
            valueBox(paste(weekGrowth(), " %"), "7 day Average Growth", 
                     icon=icon("angle-up"),color = "yellow")
        }
        else if(weekGrowth()>10){
            valueBox(paste(weekGrowth(), " %"), "7 Day Average Growth", 
                     icon=icon("angle-double-up"),color = "red")
        }
        else if(0> weekGrowth() && weekGrowth()>=-10){
            valueBox(paste(weekGrowth(), " %"), "7 Day Average Change", 
                     icon=icon("angle-down"),color = "teal")
        }
        else if(-10>weekGrowth()){
            valueBox(paste(weekGrowth(), " %"), "7 Day Average Change", 
                     icon=icon("angle-double-down"),color = "green")
        }
    }else{
        valueBox("Unknown", "7 day Average Growth",
                 color = "red")
        }

    )
    
# Rankings Page
    # Get Data from Johns Hopkins and clean it
    stateLink <- reactive({
        s <- stringr::str_sub(JHdata, end = -15)
        date <- format(input$dateState, "%m-%d-%Y")
        s <- paste(s,date,".csv", sep="")
        s <- data.frame(read.csv(s))
        
        USdata <- filter(s, Province_State %in% state.name)
        if (!is.null(USdata)){
            return(USdata)
        } else{
            return(FALSE)
        }
    })

    output$map2 <- renderLeaflet({
        
        
        if (input$stateMapCondition == "Total Positives"){
            
        pal2 <- colorNumeric("Reds", USmerged$positive, na.color = NA)
        leaflet("map2") %>% 
            addProviderTiles("CartoDB.Positron")%>%
            setView(-98, 42, zoom = 4)%>%
            addPolygons(
                data=USmerged,
                fillOpacity = 1,
                stroke = TRUE,
                weight = 1,
                smoothFactor = .5,
                fillColor = ~pal2(USmerged$positive),
                label = paste0(USmerged$NAME,": ", USmerged$positive),
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding="3px 8px"),
                    textsize = "12px",
                    direction = "auto"))%>% 
                    addLegend("bottomright", title= "Total Positive Cases",
                              pal2,values=USmerged$positive, na.label = "")
            }  else if (input$stateMapCondition == "Positives per Capita"){
                pal2 <- colorNumeric("Reds", domain = USmerged$perCap, na.color = NA)
                leaflet("map2") %>% 
                    addProviderTiles("CartoDB.Positron")%>%
                    setView(-98, 42, zoom = 4)%>%
                    addPolygons(
                        data=USmerged,
                        fillOpacity = 1,
                        stroke = T,
                        weight = 1,
                        smoothFactor = .5,
                        fillColor = ~pal2(USmerged$perCap),
                        label = paste0(USmerged$NAME,": ", round(USmerged$perCap, 4)),
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding="3px 8px"),
                            textsize = "12px",
                            direction = "auto")
                        ) %>% 
                    addLegend("bottomright", title= "Positives per Capita",pal2, 
                                  values=USmerged$perCap, na.label = "")
                    } 
        

    })
    
        
    rankings <- reactive({
        rankRaw <- filter(statesRaw, date == max(statesRawN$date))
        rankRaw<-rankRaw[,c(2,3,17)]
        rankRaw <- left_join(rankRaw, censusState, by=c("state"="STATE"))
        names(rankRaw)[4] <- "Population"
        if (input$rankBy=="Total Positives"){
        rank <- rankRaw[rev(order(rankRaw$positive)),]
        rank <- rank[, c(1,2)]
        names(rank)[1] <- "State"
        names(rank)[2] <- "Total Positive"
        rank
        } 
        
        else if(input$rankBy == "Total Deaths"){
            rank <- rankRaw[rev(order(rankRaw$death)),]
            rank <- rank[, c(1,3)]
            names(rank)[1] <- "State"
            names(rank)[2] <- "Total Deaths"
            rank
        } else if(input$rankBy=="Deaths per Capita"){
            rank <- rankRaw[, c(1,3,4)]
            rank <- rank %>% mutate(dPP= round(rank$death/rank$Population, 4))
            
            rank <- rank[, c(1,4)]
            rank <- rank[rev(order(rank$dPP)),]
            
            names(rank)[1] <- "State"
            names(rank)[2] <- "Deaths per Capita"
            rank
        }else if(input$rankBy=="Positives per Capita"){
            rank <- rankRaw[, c(1,2,4)]
            rank <- rank %>% mutate(dPP= round(rank$positive/rank$Population, 4))
            
            rank <- rank[, c(1,4)]
            rank <- rank[rev(order(rank$dPP)),]
            
            names(rank)[1] <- "State"
            names(rank)[2] <- "Positive per Capita"
            rank
        }
    })
    
    output$statesRank <- renderDataTable({
        
        datatable(rankings(), options=list(paging=FALSE, searching=FALSE, info=FALSE), rownames = F)
    })

})