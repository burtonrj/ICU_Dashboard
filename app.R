checkPackages <- function(){
    requirements <- c("shiny",
                      "shinythemes",
                      "tidyverse",
                      "readxl",
                      "lubridate",
                      "testit",
                      "plotly",
                      "survival",
                      "survminer")
    check <- match(requirements, 
                   utils::installed.packages()[,1])
    packagestoinstall <- requirements[is.na(check)]
    if(length( packagestoinstall ) > 0L){
        utils::install.packages(packagestoinstall)
    }else{
        print("All requirements already fulfilled")
    }
    
}
checkPackages()

library(shiny)
library(shinythemes)
library(tidyverse)
library(readxl)
library(lubridate)
library(testit)
library(plotly)
library(survival)
library(survminer)

source('etl_module.R')
# Global variables
cont.vars <- c('age', 'height', 'weight',
               'AP2', 'DaysVentilated',
               '`ICU Duration (hours)`',
               '`Hosp Duration (days)`')
disc.vars <- c('Unit_Outcome',
               'Hosp_Outcome', 'sex',
               'HIV/AIDS', 'Cancer',
               'Chemotherapy', 'ChronicHeart',
               'ChronicRenal', 'ChronicResp',
               'LiverCirrhosis', 'Home_Ventilate',
               'Immunosup', 'Leukaemia',
               'Leukaemia_Chron', 'Lymphoma',
               'Portal_Hyper', 'Radiotherapy',
               'MechanicalVentilation', 'RenalRT')
all.vars <- c(cont.vars, disc.vars)
kap.times <- c('ICU Duration (hours)', 'Hosp Duration (days)')
kap.events <- c('ICUSurvival', 'HospSurvival')

#--------------------------------------------------------#
######################## FRONTEND ########################
#--------------------------------------------------------#

ui <- fluidPage(
    theme = shinytheme("united"),
    titlePanel("ICU Covid Dashboard"),
    sidebarLayout(
        sidebarPanel(img(src="logo.png", width=200),
                     br(),
                     br(),
                     p("Welcome to the Cardiff ICU COVID-19 dashboard.
                       This application runs in the R Shiny framework and
                       helps visualise the standard output from WardWatcher.
                       Data must be located in the app directory and named
                       `data.xlsx`. See app documentation for details."),
                     br(),
                     p("Authored by: Ross Burton, Alexander Greenshields-Watson,
                       and Michael Ware"),
                     br(),
                     h5("Contact"),
                     p("burtonrj@cardiff.ac.uk; michael.ware@wales.nhs.uk")),
        mainPanel(tabsetPanel(tabPanel("Occupancy",
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
            plotlyOutput(outputId = "occupancy")),
            tabPanel("Admission Stats",
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
            fluidRow(column(6, selectInput(inputId="density.x",
                                           label="X-axis Variable",
                                           choices=cont.vars,
                                           selected="age"),
                               numericInput(inputId="density.adjust",
                                            label="Bandwidth adjustment",
                                            value=1,
                                            step=0.1)),
                     column(6, plotlyOutput(outputId = "density")))),
            tabPanel("Survival curves",
            h2("Survival curves", align = "center"),
            br(),
            h4("Covid vs Non-Covid"),
            fluidRow(column(6, selectInput(inputId="kap.time",
                                           label="Timeframe",
                                           choices=kap.times,
                                           selected="`ICU Duration (hours)`"),
                               selectInput(inputId="kap.event",
                                           label="Event",
                                           choices=kap.events,
                                           selected="ICUSurvival")),
                     column(6, plotOutput(outputId = "kap"))),
            br(),
            h4("Within the Covid cohort:"),
            fluidRow(column(6, selectInput(inputId="kap.time.covid",
                                           label="Timeframe",
                                           choices=kap.times,
                                           selected="`ICU Duration (hours)`"),
                            selectInput(inputId="kap.event.covid",
                                        label="Event",
                                        choices=kap.events,
                                        selected="ICUSurvival"),
                             selectInput(inputId="kap.factor.covid",
                                         label="Factor",
                                         choices=disc.vars,
                                         selected="sex")),
                     column(6, plotOutput(outputId = "kap.covid"))),
            br(),
            h4("Cox proportional hazard model:"),
            fluidRow(column(6, selectInput(inputId="cox.time",
                                           label="TimeFrame",
                                           choices=kap.times,
                                           selected="`ICU Duration (hours)`"),
                            selectInput(inputId="cox.event",
                                        label="Event",
                                        choices=kap.events,
                                        selected="ICUSurvival"),
                            selectInput(inputId="cox.covar1",
                                        label="1st covariate",
                                        choices=all.vars,
                                        selected="sex"),
                            selectInput(inputId="cox.covar2",
                                        label="2nd covariate",
                                        choices=all.vars,
                                        selected="age"),
                            selectInput(inputId="cox.covar3",
                                        label="3rd covariate",
                                        choices=all.vars,
                                        selected="weight")),
                     column(6, plotOutput(outputId = "cox"))),
            fluidRow(verbatimTextOutput(outputId = "cox.print"))
        ))
    ))
)

#--------------------------------------------------------#
######################### SERVER #########################
#--------------------------------------------------------#

server <- function(input, output) {
    ######### OCCUPANCY PLOTS #########
    output$occupancy <- renderPlotly(
        ggplotly(
            occ.aggregates %>% 
                filter(DateTime >= input$occ.start & 
                           DateTime <= input$occ.end) %>%
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
        y <- gsub("`", "", input$violin.y)
        covid <- as.numeric(unlist(admissions %>% 
                                       filter(covid19 == 'Yes') %>% 
                                       select(y)))
        not.covid <- as.numeric(unlist(admissions %>%
                                           filter(covid19 == 'No') %>% 
                                           select(y)))
        covid.norm.p <- shapiro.test(covid)$p.value
        not.covid.norm.p <- shapiro.test(not.covid)$p.value
        norm <- covid.norm.p > 0.05 & not.covid.norm.p > 0.05
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
        if(stats$p.value < 0.001){
            p <- "p-value: < 0.001"
        }else{
            p <- paste("p-value:", round(stats$p.value, digits=3))
        }
        paste(header, method, p, sep='; ')
    })
    # Violin plot
    output$violin <- renderPlotly(
        ggplotly(
            admissions %>%
                ggplot(aes_string(x='covid19', 
                                  y=input$violin.y)) +
                geom_violin(alpha=0.1) +
                geom_jitter(aes_string(fill=input$violin.colour,
                                       text='Hospital_Number'), 
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
    # Density plot
    output$density <- renderPlotly(
        ggplotly(
            admissions %>% 
                ggplot(aes_string(x=input$density.x, fill='covid19')) +
                geom_density(alpha=0.5, adjust=input$density.adjust) +
                ylab('') + 
                scale_x_continuous(expand=c(0,0)) +
                scale_y_continuous(expand=c(0,0)) +
                theme(panel.background = element_rect(fill = "white", 
                                                      colour = "grey50"))
        )
    )
    # Survival curves
    output$kap <- renderPlot({
        d <- admissions %>% 
            select(c(input$kap.time, 
                     input$kap.event,
                     "covid19")) %>%
            as.data.frame()
        colnames(d) <- c("Time", "Event", "Covid")
        fit <- survfit(Surv(Time, Event) ~ Covid, data=d)
        ggsurvplot(fit,
                   pval = TRUE, conf.int = TRUE,
                   linetype = "strata", # Change line type by groups
                   surv.median.line = "hv", # Specify median survival
                   ggtheme = theme_bw(), # Change ggplot2 theme
                   palette = c("#E7B800", "#2E9FDF"),
                   data=d)
    })
    output$kap.covid <- renderPlot({
        d <- admissions %>% 
            filter(covid19 == "Yes") %>%
            select(c(input$kap.time.covid, 
                     input$kap.event.covid,
                     input$kap.factor.covid)) %>%
            as.data.frame()
        colnames(d) <- c("time", "event", "factor")
        fit <- survfit(Surv(time, event) ~ factor, data=d)
        ggsurvplot(fit,
                   pval = TRUE, conf.int = TRUE,
                   linetype = "strata", # Change line type by groups
                   surv.median.line = "hv", # Specify median survival
                   ggtheme = theme_bw(), # Change ggplot2 theme
                   palette = c("#E7B800", "#2E9FDF"),
                   data=d)
    })
    output$cox <- renderPlot({
        d <- admissions %>% 
            filter(covid19 == "Yes") %>%
            select(c(input$cox.time, 
                     input$cox.event,
                     input$cox.covar1,
                     input$cox.covar2,
                     input$cox.covar3))
        d <- d %>% as.data.frame()
        colnames(d) <- c("time", "event",
                         "covar1","covar2","covar3")
        cox.fit <- coxph(Surv(time, event) ~ covar1 + covar2 + covar3,
                         data=d)
        ggforest(cox.fit, data=d)
    })
    output$cox.print <- renderPrint({
        d <- admissions %>%
            filter(covid19 == "Yes") %>%
            select(c(input$cox.time, 
                     input$cox.event,
                     input$cox.covar1,
                     input$cox.covar2,
                     input$cox.covar3)) %>%
            as.data.frame()
        colnames(d) <- c("time", "event",
                         "covar1","covar2","covar3")
        cox.fit <- coxph(Surv(time, event) ~ covar1 + covar2 + covar3,
                         data=d)
        return(summary(cox.fit))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
