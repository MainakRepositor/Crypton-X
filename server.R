

library(shiny)
source("fonctions1.R")

shinyServer(function(input, output) {
    
    
    
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
    
    })#////server
