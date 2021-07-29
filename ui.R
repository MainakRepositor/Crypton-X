

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



# Define UI for application that draws a histogram
shinyUI(fluidPage(
    

    
    titlePanel("CryptonX: Cryptocurrency Predictor"),
    
    tabsetPanel(type="tabs",
                
            tabPanel("Data",br(),
                         
    sidebarLayout(
        sidebarPanel(
           
            
            selectInput("share", "Please select the crypto-currency symbols", 
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
                        
                        tabPanel("Visualization",br(),plotOutput("plot")
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
))# shinyUI

