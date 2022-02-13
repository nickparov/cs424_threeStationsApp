

library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(dplyr)
library(tidyr)


# read
all_data <- read.table(file = "./data.tsv", sep = "\t", header = TRUE)
# logic
AllDates <- separate(all_data, date, c("Month", "Day", "Year"))
AllDates$FullDate <- paste(AllDates$Year, AllDates$Month, AllDates$Day, sep = "-")
AllDates$dayName <- weekdays(as.Date(AllDates$FullDate))


# UIC
UIC_ALL_ENTRIES <- AllDates[AllDates$station_id == 40350,]
UIC_2020 <- UIC_ALL_ENTRIES[UIC_ALL_ENTRIES$Year == 2020,]
UIC_2021 <- UIC_ALL_ENTRIES[UIC_ALL_ENTRIES$Year == 2021,]

# O HARE
OHARE_ALL_ENTRIES <- AllDates[AllDates$station_id == 40890,]
OHARE_2021 <- OHARE_ALL_ENTRIES[OHARE_ALL_ENTRIES$Year == 2021,]

# Central Lake
LAKE_ALL_ENTRIES <- AllDates[AllDates$station_id == 40280,]
LAKE_2021 <- LAKE_ALL_ENTRIES[LAKE_ALL_ENTRIES$Year == 2021,]

years <- unique(UIC_ALL_ENTRIES[c("Year")])
yearsOrdered <- years[order(years$Year),]



# Define UI for application
ui <- dashboardPage(
  
  # Application title
  dashboardHeader(title="App"),
  
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
       sidebarMenu(
           menuItem("About this Website", tabName = "AboutPage", icon = icon("th")),
           menuItem("All Graphs", tabName = "Graphs", icon = icon("dashboard")),
           menuItem("All Tables", tabName = "Tables", icon = icon("th"))
         ),
       
       selectInput("Year", "UIC Halsted Year", yearsOrdered, selected = 2021),
       selectInput("Year_OHARE", "O'Hare Year", yearsOrdered, selected = 2021),
       selectInput("Year_LAKE", "Central Lake Year", yearsOrdered, selected = 2021)
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "Graphs",
              fluidRow(
                column(4, 
                       fluidRow(
                         box(title = "UIC Halsted All Years", solidHeader = TRUE, status = "primary", width = 12, plotOutput("UIC_BAR", height = 200))
                       ),
                       fluidRow(box(title = "UIC Halsted Each Month", solidHeader = TRUE, status = "primary", width = 12,
                                    plotOutput("UIC_MONTH_BAR", height = 200)
                        )
                       ),
                       fluidRow(box(title = "UIC Halsted Each Day", solidHeader = TRUE, status = "primary", width = 12,
                                    plotOutput("UIC_DAY_BAR", height = 200)
                        )
                       ),
                       fluidRow(box(title = "UIC Halsted Each Weekday", solidHeader = TRUE, status = "primary", width = 12,
                                    plotOutput("UIC_WEEKDAY_BAR", height = 200)
                        )
                       ),
                       fluidRow(box(title = "UIC Halsted Each Month/Day", solidHeader = TRUE, status = "primary", width = 12,
                                    plotOutput("UIC_DAY_MONTH_BAR", height = 1400)
                        )
                       )
                    ),
                column(4, 
                       fluidRow(
                         box(title = "O'Hare All Years", solidHeader = TRUE, status = "primary", width = 12, 
                             plotOutput("OHARE_BAR", height = 200))
                       ),
                       fluidRow(
                         box(title = "O'Hare Each Month", solidHeader = TRUE, status = "primary", width = 12,
                                    plotOutput("OHARE_MONTH_BAR", height = 200))
                       ),
                       fluidRow(
                         box(title = "O'Hare Each Day", solidHeader = TRUE, status = "primary", width = 12,
                                    plotOutput("OHARE_DAY_BAR", height = 200))
                       ),
                       fluidRow(
                         box(title = "O'Hare Each Weekday", solidHeader = TRUE, status = "primary", width = 12,
                                    plotOutput("OHARE_WEEKDAY_BAR", height = 200))
                       ),
                       fluidRow(
                         box(title = "O'Hare Each Month", solidHeader = TRUE, status = "primary", width = 12,
                                    plotOutput("OHARE_DAY_MONTH_BAR", height = 1400))
                       )
                  ),
                column(4, 
                       fluidRow(
                         box(title = "Central Lake All Years", solidHeader = TRUE, status = "primary", width = 12, 
                             plotOutput("LAKE_BAR", height = 200))
                       ),
                       fluidRow(
                         box(title = "Central Lake Each Month", solidHeader = TRUE, status = "primary", width = 12,
                             plotOutput("LAKE_MONTH_BAR", height = 200))
                       ),
                       fluidRow(
                         box(title = "Central Lake Each Day", solidHeader = TRUE, status = "primary", width = 12,
                             plotOutput("LAKE_DAY_BAR", height = 200))
                       ),
                       fluidRow(
                         box(title = "Central Lake Each Weekday", solidHeader = TRUE, status = "primary", width = 12,
                             plotOutput("LAKE_WEEKDAY_BAR", height = 200))
                       ),
                       fluidRow(
                         box(title = "Central Lake Each Month", solidHeader = TRUE, status = "primary", width = 12,
                             plotOutput("LAKE_DAY_MONTH_BAR", height = 1400))
                       )
                    )
                )
      ),
      tabItem(tabName = "Tables",
              h2("Tables tab content"),
              fluidRow(
                column(4, 
                       fluidRow(box(title = "UIC All Data", solidHeader = TRUE, status = "primary", width = 12,
                                    dataTableOutput("UIC_TABLE")
                       )),
                       fluidRow(box(title = "UIC Each Month Data", solidHeader = TRUE, status = "primary", width = 12,
                                    dataTableOutput("UIC_MONTH_TABLE")
                       )),
                       fluidRow(box(title = "UIC Each Day Data", solidHeader = TRUE, status = "primary", width = 12,
                                    dataTableOutput("UIC_WEEKDAY_TABLE")
                       )),
                       fluidRow(box(title = "UIC Month/Day Data", solidHeader = TRUE, status = "primary", width = 12,
                                    dataTableOutput("UIC_DAY_MONTH_TABLE")
                       ))
                  ),
                column(4, 
                       fluidRow(box(title = "OHARE All Years", solidHeader = TRUE, status = "primary", width = 12,
                                    dataTableOutput("OHARE_TABLE")
                       )),
                       fluidRow(box(title = "OHARE Month Of Year", solidHeader = TRUE, status = "primary", width = 12,
                                    dataTableOutput("OHARE_MONTH_TABLE")
                       )),
                       fluidRow(box(title = "OHARE Weekday Of Year", solidHeader = TRUE, status = "primary", width = 12,
                                    dataTableOutput("OHARE_WEEKDAY_TABLE")
                       )),
                       fluidRow(box(title = "OHARE Each Day of Year", solidHeader = TRUE, status = "primary", width = 12,
                                    dataTableOutput("OHARE_DAY_MONTH_TABLE")
                       ))
                  ),
                column(4, 
                       fluidRow(box(title = "Central Lake All Years", solidHeader = TRUE, status = "primary", width = 12,
                                    dataTableOutput("LAKE_TABLE")
                       )),
                       fluidRow(box(title = "Central Lake Month Of Year", solidHeader = TRUE, status = "primary", width = 12,
                                    dataTableOutput("LAKE_MONTH_TABLE")
                       )),
                       fluidRow(box(title = "Central Lake Weekday Of Year", solidHeader = TRUE, status = "primary", width = 12,
                                    dataTableOutput("LAKE_WEEKDAY_TABLE")
                       )),
                       fluidRow(box(title = "Central Lake Each Day of Year", solidHeader = TRUE, status = "primary", width = 12,
                                    dataTableOutput("LAKE_DAY_MONTH_TABLE")
                       ))
                    )
              )
      ),
      tabItem(tabName = "AboutPage",
              h2("This Website was created by Mykyta Parovyi, data was taken from the official website. It was launched on 10 February 2022. This app is made for class")
        )
    ),
  
  )
  
)


# recalc specific year data for UIC Halsted

# Define server logic
server <- function(input, output) {
  SpecificYearData <- reactive({subset(UIC_ALL_ENTRIES, UIC_ALL_ENTRIES$Year == input$Year)})
  OHAREYearData <- reactive({subset(OHARE_ALL_ENTRIES, OHARE_ALL_ENTRIES$Year == input$Year_OHARE)})
  LAKEYearData <- reactive({subset(LAKE_ALL_ENTRIES, LAKE_ALL_ENTRIES$Year == input$Year_LAKE)})
  # Create bar chart of brands
  output$UIC_BAR <- renderPlot({
    ggplot(data=UIC_ALL_ENTRIES, aes(x=Year, y=rides)) + geom_bar(stat="identity")
  })
  
  output$UIC_MONTH_BAR <- renderPlot({
    dataToPlot <- SpecificYearData()
    ggplot(data=dataToPlot, aes(x=Month, y=rides)) + geom_bar(stat="identity")
  })
  
  output$UIC_DAY_BAR <- renderPlot({
    dataToPlot <- SpecificYearData()
    ggplot(data=dataToPlot, aes(x=Day, y=rides)) + geom_bar(stat="identity")
  })
  
  output$UIC_WEEKDAY_BAR <- renderPlot({
    dataToPlot <- SpecificYearData()
    ggplot(data=dataToPlot, aes(x=dayName, y=rides)) + geom_bar(stat="identity")
  })
  
  output$UIC_DAY_MONTH_BAR <- renderPlot({
    dataToPlot <- SpecificYearData()
    dataToPlot$MonthDay = paste(dataToPlot$Month, dataToPlot$Day, sep="/")
    
    ggplot(data=dataToPlot, aes(x=rides, y=MonthDay)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(size = 10))
  })
  
  # O'HARE BARS
  output$OHARE_BAR <- renderPlot({
    ggplot(data=OHARE_ALL_ENTRIES, aes(x=Year, y=rides)) + geom_bar(stat="identity")
  })
  output$OHARE_MONTH_BAR <- renderPlot({
    dataToPlot <- OHAREYearData()
    ggplot(data=dataToPlot, aes(x=Month, y=rides)) + geom_bar(stat="identity")
  })
  
  output$OHARE_DAY_BAR <- renderPlot({
    dataToPlot <- OHAREYearData()
    ggplot(data=dataToPlot, aes(x=Day, y=rides)) + geom_bar(stat="identity")
  })
  
  output$OHARE_WEEKDAY_BAR <- renderPlot({
    dataToPlot <- OHAREYearData()
    ggplot(data=dataToPlot, aes(x=dayName, y=rides)) + geom_bar(stat="identity")
  })
  
  output$OHARE_DAY_MONTH_BAR <- renderPlot({
    dataToPlot <- OHAREYearData()
    dataToPlot$MonthDay = paste(dataToPlot$Month, dataToPlot$Day, sep="/")
    
    ggplot(data=dataToPlot, aes(x=rides, y=MonthDay)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(size = 10))
  })
  
  
  # Lake Bars
  
  output$LAKE_BAR <- renderPlot({
    ggplot(data=LAKE_ALL_ENTRIES, aes(x=Year, y=rides)) + geom_bar(stat="identity")
  })
  output$LAKE_MONTH_BAR <- renderPlot({
    dataToPlot <- LAKEYearData()
    ggplot(data=dataToPlot, aes(x=Month, y=rides)) + geom_bar(stat="identity")
  })
  
  output$LAKE_DAY_BAR <- renderPlot({
    dataToPlot <- LAKEYearData()
    ggplot(data=dataToPlot, aes(x=Day, y=rides)) + geom_bar(stat="identity")
  })
  
  output$LAKE_WEEKDAY_BAR <- renderPlot({
    dataToPlot <- LAKEYearData()
    ggplot(data=dataToPlot, aes(x=dayName, y=rides)) + geom_bar(stat="identity")
  })
  
  output$LAKE_DAY_MONTH_BAR <- renderPlot({
    dataToPlot <- LAKEYearData()
    dataToPlot$MonthDay = paste(dataToPlot$Month, dataToPlot$Day, sep="/")
    
    ggplot(data=dataToPlot, aes(x=rides, y=MonthDay)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(size = 10))
  })
  
  
  # Create tables
  output$UIC_TABLE <- renderDataTable({
    yearsSum <- aggregate(UIC_ALL_ENTRIES$rides, list(UIC_ALL_ENTRIES$Year), sum)
  })
  
  
  output$UIC_MONTH_TABLE <- renderDataTable({
    data <-  SpecificYearData()
    monthsSum <- aggregate(data$rides, list(data$Month), sum)
  })
  
  output$UIC_WEEKDAY_TABLE  <- renderDataTable({
    data <- SpecificYearData()
    daySUM = aggregate(data$rides, list(data$dayName), sum)
  })
  
  output$UIC_DAY_MONTH_TABLE  <- renderDataTable({
    data <- SpecificYearData()
    data$MonthDay = paste(data$Month, data$Day, sep="/")
    dayMonthSUM <- aggregate(data$rides, list(data$MonthDay), sum)
  })
  
  
  # OHARE
  output$OHARE_TABLE <- renderDataTable({
    yearsSum <- aggregate(OHARE_ALL_ENTRIES$rides, list(OHARE_ALL_ENTRIES$Year), sum)
  })
  
  output$OHARE_MONTH_TABLE <- renderDataTable({
    data <-  OHAREYearData()
    monthsSum <- aggregate(data$rides, list(data$Month), sum)
  })
  
  output$OHARE_WEEKDAY_TABLE  <- renderDataTable({
    data <- OHAREYearData()
    daySUM = aggregate(data$rides, list(data$dayName), sum)
  })
  
  output$OHARE_DAY_MONTH_TABLE  <- renderDataTable({
    data <- OHAREYearData()
    data$MonthDay = paste(data$Month, data$Day, sep="/")
    dayMonthSUM <- aggregate(data$rides, list(data$MonthDay), sum)
  })
  
  
  # LAKE
  output$LAKE_TABLE <- renderDataTable({
    yearsSum <- aggregate(LAKE_ALL_ENTRIES$rides, list(LAKE_ALL_ENTRIES$Year), sum)
  })
  
  output$LAKE_MONTH_TABLE <- renderDataTable({
    data <-  LAKEYearData()
    monthsSum <- aggregate(data$rides, list(data$Month), sum)
  })
  
  output$LAKE_WEEKDAY_TABLE  <- renderDataTable({
    data <- LAKEYearData()
    daySUM = aggregate(data$rides, list(data$dayName), sum)
  })
  
  output$LAKE_DAY_MONTH_TABLE  <- renderDataTable({
    data <- LAKEYearData()
    data$MonthDay = paste(data$Month, data$Day, sep="/")
    dayMonthSUM <- aggregate(data$rides, list(data$MonthDay), sum)
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)