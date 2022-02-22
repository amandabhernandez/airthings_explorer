#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


dashboardPage(
    
    # Application title
    dashboardHeader(title = "EH 252  Airthings Explorer"),
    
    # Sidebar with a slider input for number of bins
    dashboardSidebar(
        pickerInput("date_select", "Select Date Range to Show", choices = c("Date Range" = "dates", 
                                                                    "12 hours" = "12hr", 
                                                                    "24 hours" = "24hr",
                                                                    "36 hours" = "36hr",
                                                                    "48 hours" = "48hr", 
                                                                    "1 week" = "1wk", 
                                                                    "1 month" = "1mo"),
                    options = list(`actions-box` = TRUE,
                                   title = "Select"),
                    multiple = FALSE),
        conditionalPanel(condition = "input.date_select ==`dates`", 
                         dateRangeInput("date_range", "Date Range:", 
                                        start = min(air_dat_long$date),
                                        end = max(air_dat_long$date),
                                        min = min(air_dat_long$date),
                                        max = max(air_dat_long$date))),

        #uiOutput("metric"),
        sidebarMenu(menuItem("Dashboard", tabName = "dashboard")),
        sidebarMenu(menuItem("Metrics", tabname = "metrics", 
                             menuSubItem("Temperature",
                                         tabName = "temp"),
                             menuSubItem("Humidity",
                                         tabName = "humidity"),
                             menuSubItem("Pressure",
                                         tabName = "pressure"),
                             menuSubItem("Radon",
                                         tabName = "radon"),
                             menuSubItem("CO2",
                                         tabName = "co2"),
                             menuSubItem("VOC",
                                         tabName = "voc"),
                             menuSubItem("PM",
                                         tabName = "pm"))),
        sidebarMenu(menuItem("Source code", 
                             icon = icon("github"), 
                             href = "https://github.com/amandabhernandez/airthings_explorer"))
        
    ),
    
    # Show a plot of the generated distribution
    dashboardBody(
        tabItems(
            tabItem(tabName = "dashboard",
                    fluidRow(h3(" Current Measurements"),
                             infoBoxOutput("temp_current"),
                             infoBoxOutput("humidity_current"),
                             infoBoxOutput("pressure_current"),
                             infoBoxOutput("radon_current"),
                             infoBoxOutput("co2_current"),
                             infoBoxOutput("voc_current"),
                             infoBoxOutput("pm2.5_current"),
                             infoBoxOutput("pm10_current"),
                             ),
                    fluidRow(h3(" Time Series Graphs"), 
                             plotlyOutput("airplot"))
                    # fluidRow(box(
                    #     # width = 8, status = "info", solidHeader = TRUE,
                    #     # title = "Popularity by package (last 5 min)",
                    #     plotlyOutput("airplot"))
                    # )
            ),
            tabItem(tabName = "temp",
                    fluidRow(plotlyOutput("tempplot"))),
            tabItem(tabName = "humidity",
                    fluidRow(plotlyOutput("humplot"))),
            tabItem(tabName = "pressure",
                    fluidRow(plotlyOutput("pressplot"))),
            tabItem(tabName = "radon",
                    fluidRow(plotlyOutput("radplot"))),
            tabItem(tabName = "co2",
                    fluidRow(plotlyOutput("co2plot"))),
            tabItem(tabName = "voc",
                    fluidRow(plotlyOutput("vocplot"))),
            tabItem(tabName = "pm",
                    fluidRow(plotlyOutput("pmplot")))
            )
    )
    
)

