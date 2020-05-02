#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(readxl)
library(lubridate)
library(testit)
library(plotly)

source('etl_module.R')
# Fixed variables
cont.vars <- c('age', 'height', 'weight',
               'AP2', 'DaysVentilated',
               '`ICU Duration (hours)`')
disc.vars <- c('covid19', 'Unit_Outcome',
               'Hosp_Outcome', 'sex',
               'HIV/AIDS', 'Cancer',
               'Chemotherapy', 'ChronicHeart',
               'ChronicRenal', 'ChronicResp',
               'LiverCirrhosis', 'Home_Ventilate',
               'Immunosup', 'Leukaemia',
               'Leukaemia_Chron', 'Lymphoma',
               'Portal_Hyper', 'Radiotherapy',
               'MechanicalVentilation', 'RenalRT')
# Define UI for application
ui <- fluidPage(
    theme = shinytheme("united"),
    titlePanel("ICU Covid Dashboard"),
    sidebarLayout(
        sidebarPanel(img(src="cardiff-and-vale-LHB.jpg", width=200),
                     br(),
                     br(),
                     img(src="universitylogo.jpg", width=100, align="center")),
        mainPanel(
            h2("ICU L3 occupancy and forecasts", align = "center"),
            br(),
            br(),
            fluidRow(column(4, dateInput(inputId='occ.start', 
                               label = "Start",
                               value="2020-03-15",
                               format="yyyy-mm-dd")),
                     column(4, dateInput(inputId='occ.end', 
                               label = "End",
                               value="2020-05-01",
                               format="yyyy-mm-dd")),
                     column(4, numericInput(inputId="limit",
                                  "ICU Funding Limit",
                                  value=28))),
            plotlyOutput(outputId = "occupancy"),
            h2("Admission statistics", align = "center"),
            fluidRow(column(6, selectInput(inputId="violin.y",
                                            label="Y-axis Variable",
                                            choices=cont.vars,
                                            selected="age"),
                                selectInput(inputId="violin.colour",
                                            label="Dot colour",
                                            choices=disc.vars,
                                            selected="Unit_Outcome"),
                                numericInput(inputId="violin.jitter",
                                            label="Control Jitter",
                                            value=0.1,
                                            step=0.1),
                                h4('Inference tests'),
                                textOutput(outputId="stat")),
                     column(6, plotlyOutput(outputId = "violin"))),
            h2("Survival curves", align = "center")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    ######### OCCUPANCY PLOTS #########
    output$occupancy <- renderPlotly(
        ggplotly(
            occ.aggregates %>% 
                filter(DateTime >= input$occ.start & DateTime <= input$occ.end) %>%
                ggplot(aes(x=DateTime)) +
                geom_bar(aes(y=L3, fill=L3), stat='identity', alpha=1) +
                geom_line(aes(y=L3Covid, colour='L3Covid')) +
                geom_line(aes(y=L3NonCovid, colour='L3NonCovid')) +
                geom_hline(yintercept = input$limit, linetype="dashed") +
                scale_fill_gradient2(name="Total L3 Occupancy",
                                     midpoint=input$limit, 
                                     low='green', 
                                     high='red', 
                                     mid='yellow') +
                scale_color_manual(name="",values=c("L3Covid"="Blue",
                                                    "L3NonCovid"="Black")) +
                theme(panel.background = element_rect(fill = "white", 
                                                      colour = "grey50")) +
                xlab("Date") +
                ylab("Occupancy")
        )
    )
    ######### ADMISSIONS STATS #########
    
    # Does the chosen var exhibit normality?
    unpaired.t <- function(x, y){
        return(list("method"="Two-tailed unpaired T test",
                    "p.value"=t.test(x, y, var.equal = TRUE)$p.value))
    }
    mann.whit <- function(x, y){
        return(list("method"="Two-tailed Mann-Whitney test",
               "p.value"=wilcox.test(x, y, alternative = "two.sided")$p.value))
    }
    output$stat <- reactive({
        covid <- as.numeric(unlist(admissions %>% 
                                       filter(covid19 == 'Yes') %>% 
                                       select(input$violin.y)))
        not.covid <- as.numeric(unlist(admissions %>%
                                           filter(covid19 == 'No') %>% 
                                           select(input$violin.y)))
        covid.norm.p <- round(shapiro.test(covid)$p.value, digits=2)
        not.covid.norm.p <- round(shapiro.test(not.covid)$p.value, digits=2)
        norm <- covid.p > 0.05 & not.covid.p > 0.05
        if(norm){
            # Are the variances equal?
            if(var.test(covid, 
                        not_covid, 
                        alternative = "two.sided")$p.value > 0.05){
                # Perform a unpaired T test
                stats <- unpaired.t(covid, not.covid)
            }else{
                # Perform unpaied Mann Whitney
                stats <- mann.whit(covid, not.covid)
            }
        }else{
            stats <- mann.whit(covid, not.covid)
        }
        header <- paste(input$violin.y, "~ Covid infection status:", sep=" ")
        method <- paste("Method:", stats$method)
        p <- paste("p-value:", round(stats$p.value, digits=3))
        paste(header, method, p, sep='; ')
    })
    output$violin <- renderPlotly(
        ggplotly(
            admissions %>%
                ggplot(aes_string(x='covid19', y=input$violin.y)) +
                geom_violin(alpha=0.1) +
                geom_jitter(aes_string(fill=input$violin.colour), 
                            width=input$violin.jitter, 
                            height=0,
                            shape = 21,
                            size=2,
                            alpha=0.8) +
                theme(panel.background = element_rect(fill = "white", 
                                                      colour = "grey50")) +
                xlab('COVID-19 infected')
        )
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
