#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readxl)
library(lubridate)
library(testit)
library(plotly)

source('etl_module.R')
# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("ICU Covid Dashboard"),
    sidebarLayout(
        sidebarPanel(img(src="cardiff-and-vale-LHB.jpg", width=200),
                     br(),
                     br(),
                     img(src="universitylogo.jpg", width=100, align="center")),
        mainPanel(
            h2("ICU L3 occupancy and forecasts", align = "center"),
            h2("Admission statistics", align = "center"),
            h2("Survival curves", align = "center")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
