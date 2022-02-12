#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(plotly)
library(tidyverse)


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("EH 252 - Airthings Explorer"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            uiOutput("date_range"),
            uiOutput("metric"),
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotlyOutput("airplot")
        )
    )
))
