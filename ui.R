#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

shinyUI(dashboardPage(skin="green",
    dashboardHeader(title = "COVID-19 Project"),
    dashboardSidebar(
        sidebarMenu(
            menuItem(
                "State Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                
            menuItem("Rankings", tabName = "Rank", icon=icon("list-ul"))
            
            )
        ),
    dashboardBody(
    tabItems(
        tabItem(tabName = "dashboard",
            fluidRow(
                column(width=4,
                    infoBoxOutput(width = NULL,
                        "stateDate"
                    ),box(status="primary",title="Controls", width=NULL, solidHeader = T,
                          selectInput("stateChoice", "Choose State:", choices = state.name),
                          sliderInput("date",
                                      "Dates:", min = Sys.Date()-30, 
                                      max=Sys.Date(), value=Sys.Date()-15),
                          selectInput("Condition","Choose Map Condition:", 
                                      choices=c("Total Positives","Positives per Capita"))
                    
                )),
                column(width=4,
                    valueBoxOutput(width = NULL,
                        "statePos"
                    ),
                    valueBoxOutput(width = NULL,
                        "stateRecovered"
                    ),
                    valueBoxOutput(width = NULL,
                        "weekPGrowth")
                ),
                column(width=4,
                    valueBoxOutput(width = NULL,
                        "stateDeaths"
                    ),
                    valueBoxOutput(width = NULL,
                        "stateHospital"),
                    valueBoxOutput(width = NULL,
                        "stateInfected"
                    )
                )
            ),
            fluidRow(
                box(title = "Cases Map",status = "success", solidHeader = T,
                    leafletOutput("map"),
                    checkboxInput("legend", "Show legend", TRUE), height = "500"
                    
                    ),
                box(title = "New Positives", solidHeader = T,
                    status = "success", plotlyOutput("av_week"), height = "500")
            )
        ),
        
        tabItem(tabName = "Rank",
                fluidRow(
                    box(title = "Map Controls",status= "primary", solidHeader = T, height = "640",
                        selectInput("stateMapCondition", "Choose Map Condition:",
                                    choices=c("Total Positives","Positives per Capita")), 
                        leafletOutput("map2")),
                box(title="State Rankings", status = "primary", 
                    solidHeader = T, width = '6', selectInput("rankBy", "Rank by:", 
                                                              choices=c("Total Positives", 
                                                                        "Positives per Capita", 
                                                                        "Total Deaths", 
                                                                        "Deaths per Capita")),
                    column(width=12, DT:: dataTableOutput("trace_table"), 
                           style="height:500px; overflow-y: scroll;overflow-x: scroll;",
                    dataTableOutput("statesRank"))
        )
                
    )

))
)))

