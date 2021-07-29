
library(shiny)
library(quantmod)
library(xts)
library(tseries)
library(forecast)
library(timeSeries)
library(tidyverse)
library(dplyr)
library(tsfknn)
library(ggplot2 )
source("fonctions1.R")
# Define UI for application that draws a histogram
ui <- fluidPage(
    
    
    
    titlePanel("Stock Prediction Application"),
    
    tabsetPanel(type="tabs",
                
                tabPanel("Data",br(),
                         
                         sidebarLayout(
                             sidebarPanel(
                                 
                                 
                                 selectInput("share", "Please select the market share", 
                                             choices = c('BTC-USD','ETH-USD','DOGE-USD','XRP-USD','ADA-USD')),
                                 
                                 
                                 dateRangeInput("dates", 
                                                "Date range",
                                                start = "2020-02-01", 
                                                end = as.character(Sys.Date())),
                                 
                                 selectInput("variable_choice", "Please select what to represent", 
                                             choices = c('All','Open','High','Low','Close')),
                                 
                                 
                                 selectInput("TA_choice", "Please select a technical indicator", 
                                             choices = c('Nothing','Volume','Moving Average Convergence Divergence',
                                                         'Bollinger Bands','Relative Strength Indicator',
                                                         "Commodity Channel Index")),
                                 
                                 
                                 br(),
                                 br(),
                                 actionButton("zoomIN", "Zoom in last week"),
                                 actionButton("zoomOUT", "Zoom out"),
                                 br(),
                                 br()
                                 
                                 
                             ),
                             
                             mainPanel(
                                 
                                 
                                 tabsetPanel(type="tabs",
                                             
                                             tabPanel("visualization",br(),plotOutput("plot")
                                             ),
                                             tabPanel("Data & Summary",br(),
                                                      tableOutput("dataset"),
                                                      verbatimTextOutput("summaryDset") )
                                             
                                 )
                                 
                             )#main panel
                             
                         )#side bar layout
                ),#tab Data
                
                tabPanel("ARIMA",br(),
                         sidebarLayout(
                             sidebarPanel(
                                 helpText("Please select your timeseries and ARIMA parameters"),
                                 selectInput("variable_ARIMA", "Please select what to represent", 
                                             choices = c('Open','High','Low','Close'),
                                             selected = "Close"),
                                 
                                 selectInput("timeseries", "Please select your time series", 
                                             choices = c('Actual','Differenced')),
                                 
                                 checkboxInput("log_arima", "Log tranformation", 
                                               value = TRUE),
                                 
                                 # Horizontal line ----
                                 tags$hr(),
                                 
                                 helpText("ARIMA (P,D,Q)"),
                                 # Input: Enter P,D,Q value ----
                                 numericInput("pval", "P", value=1),
                                 numericInput("dval", "D", value=1),
                                 numericInput("qval", "Q", value=1),
                                 
                                 checkboxInput("auto_arima", "Auto ARIMA", 
                                               value = FALSE),
                                 
                                 numericInput("forecast_period", "Forecasting Period", value=10)
                                 
                                 
                             ),#sidebarPanel ARIMA
                             
                             mainPanel(
                                 
                                 
                                 tabsetPanel(type="tabs",
                                             
                                             tabPanel("Pre-analysis",br(),
                                                      plotOutput("plot_arima1"),
                                                      verbatimTextOutput("Fuller_Test"),
                                                      plotOutput("ACF_plot"),
                                                      plotOutput("PACF_plot")
                                             ),
                                             tabPanel("ARIMA",br(),
                                                      verbatimTextOutput("summary_arima"),
                                                      verbatimTextOutput("aic_bic"),
                                                      plotOutput("multiple_plot")
                                             ),
                                             
                                             tabPanel("Forecasting",br(),
                                                      plotOutput("arima_forecast_plot"),
                                                      verbatimTextOutput("forecasting_values")
                                             )
                                             
                                             
                                 )# tabsetpanel
                             )#main panel arima
                             
                         )#sidebarLayout ARIMA
                ),#Menu ARIMA
                tabPanel("KNN",br(),
                         sidebarLayout(
                             sidebarPanel(
                                 helpText("Forecasting Using K Nearest Neighbours"),
                                 selectInput("variable_KNN", "Please select what to predict", 
                                             choices = c('Open','High','Low','Close'),
                                             selected = "Close"),
                                 checkboxInput("log_knn", "Log tranformation", 
                                               value = TRUE),
                                 numericInput("K", "K", value=10),
                                 numericInput("KNN_period", "Forecasting period", value=30)
                             ),
                             mainPanel(
                                 plotOutput("KNN_plot"),
                                 verbatimTextOutput("KNN_accuracy")
                             )
                         )
                         
                ), # menu knn
                tabPanel("Neural Networks",br(),
                         sidebarLayout(
                             sidebarPanel(
                                 
                                 selectInput("variable_NN", "Please select what to predict", 
                                             choices = c('Open','High','Low','Close'),
                                             selected = "Close"),
                                 checkboxInput("log_knn", "Log tranformation", 
                                               value = TRUE),
                                 numericInput("period_NN", "Forecasting Period", value=20),
                                 br(),
                                 actionButton("execute_nn", "Run Model"),
                                 helpText("NB : The execution takes some time, please wait until the plot appears")
                                 
                             ),
                             mainPanel(plotOutput("NN"))
                             
                         )
                )# menu NN
    )# tabsetpanel les menus
)


# Define server logic required to draw a histogram
server <- function(input, output)  {
    
    
    
    dataInput <- reactive({   
        return(data_function(input$share,input$dates,input$variable_choice) ) 
    })
    
    dataInput_arima <- reactive({   
        data<-data_function(input$share,input$dates,input$variable_ARIMA)
        
        if(input$log_arima){
            return(log(data))
        }
        
        else {return(data)}
        
    })
    
    dataInput_knn <- reactive({   
        data<-data_function(input$share,input$dates,input$variable_KNN)
        
        if(input$log_knn){
            return(log(data))
        }
        
        else {return(data)}
        
    })
    
    dataInput_nn <- reactive({   
        data<-data_function(input$share,input$dates,input$variable_NN)
        
        if(input$log_knn){
            return(log(data))
        }
        
        else {return(data)}
        
    })
    
    
    output$plot <- renderPlot({
        chartSeries(dataInput(),multi.col = FALSE,theme = 'white',    
                    type = "candles", 
                    TA=technical_indicators(input$TA_choice) )
        
        
    }) 
    observeEvent(input$zoomIN, {
        
        output$plot<-renderPlot({
            chartSeries(dataInput(),multi.col = FALSE,theme = "white",
                        type = "candles",subset = "last 1 week",
                        
                        TA=technical_indicators(input$TA_choice) )
            
            
            
        })
        
    })
    observeEvent(input$zoomOUT, {
        
        output$plot<-renderPlot({
            
            chartSeries(dataInput(),multi.col = FALSE,theme = 'white',    
                        type = "candles", 
                        TA=technical_indicators(input$TA_choice)  )
            
        })
        
    })
    
    output$dataset <- renderTable(head(dataInput()))
    output$summaryDset <- renderPrint({
        summary(dataInput()) 
    })
    
    output$plot_arima1 <- renderPlot({
        
        if(input$timeseries == "Actual"){
            plot(dataInput_arima(), main = "Time-Series plot", col = "blue")
        }
        else if (input$timeseries == "Differenced"){
            diff_price <- diff(dataInput_arima(),1)[-1,]
            plot(diff_price)
        }
        
    })
    
    output$Fuller_Test <- renderPrint({
        
        print(adf.test(actual_or_diff(dataInput_arima(),input$timeseries)))
        
    })
    
    output$ACF_plot <- renderPlot({
        if(input$timeseries == "Actual"){
            acf(dataInput_arima(),main=paste(c('ACF for actual data of',input$share),
                                             collapse=' '),lag.max = 320)
        }
        
        if(input$timeseries == "Differenced"){
            diff_price <- diff(dataInput_arima(),1)[-1,]
            acf(diff_price,main=paste(c('ACF for differenced data of',input$share),
                                      collapse=' '),lag.max = 320)
        }
    })
    
    output$PACF_plot <- renderPlot({
        if(input$timeseries == "Actual"){
            pacf(dataInput_arima(),main=paste(c('PACF for actual data of',input$share),
                                              collapse=' '),lag.max = 320)
        }
        
        if(input$timeseries == "Differenced"){
            diff_price <- diff(dataInput_arima(),1)[-1,]
            pacf(diff_price,main=paste(c('PACF for differenced data of',input$share),
                                       collapse=' '),lag.max = 320)
        }
    })
    
    #------------------------------- ARIMA Model -----------------------------------
    
    # get the model
    model_ARIMA<-reactive({
        arima_model(dataInput_arima(),input$pval,input$dval,input$qval,input$auto_arima)
    })
    output$multiple_plot <- renderPlot({
        arima_model_outputs(model_ARIMA(),input$pval,input$dval,input$qval,choice="plot",input$auto_arima)
    })
    
    output$summary_arima<-renderPrint({
        arima_model_outputs(model_ARIMA(),input$pval,input$dval,input$qval,choice="summary",input$auto_arima)
    })
    
    output$aic_bic <- renderPrint({
        arima_model_outputs(model_ARIMA(),input$pval,input$dval,input$qval,choice="aic_bic",input$auto_arima)
    })
    
    # Forecasting 
    output$arima_forecast_plot <- renderPlot({
        price_forecast<-arima_forecast(model_ARIMA(),input$forecast_period)
        plot(price_forecast)
    })
    output$forecasting_values <- renderPrint({
        price_forecast<-arima_forecast(model_ARIMA(),input$forecast_period)
        mean_forecast <- price_forecast$mean
        values <- data.frame(ForecastedPrice = c(mean_forecast))
        print(values)
    })
    
    # ------------------------------- KNN ----------------------------
    
    output$KNN_plot <- renderPlot({
        KNN_model(dataInput_knn(),input$K,input$KNN_period,"plot")
    })
    
    output$KNN_accuracy <- renderPrint(
        KNN_model(dataInput_knn(),input$K,input$KNN_period,"accuracy")
    )
    
    
    # ---------------------------- Neural Networks ----------------------
    observeEvent(input$execute_nn, {
        
        output$NN <- renderPlot({
            NN_model(dataInput_nn(),input$period_NN)
        })
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
