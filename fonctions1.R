
data_function <- function(input,dates,variable_choice){
  
  data <- getSymbols(input, src = "yahoo",      
                     from = dates[1],
                     to = dates[2],
                     auto.assign = FALSE)
                    
  if (variable_choice == "All"){
    return(data)
  }
  
  if (variable_choice == "Open"){
    return(data[,1])
  }
  
  if (variable_choice == "High"){
    return(data[,2])
  }
  
  if (variable_choice == "Low"){
    return(data[,3])
  }
  
  if (variable_choice == "Close"){
    return(data[,4])
  }
  
}

technical_indicators <- function(input){
  if(input == "Nothing"){return("NULL")}
  if(input == "Volume"){return("addVo()")}
  if(input == "Moving Average Convergence Divergence"){return("addMACD()")}
  if(input == "Bollinger Bands"){return("addBBands()" )}
  if(input == "Relative Strength Indicator"){return("addRSI()" )}
  if(input == "Commodity Channel Index"){return("addCCI()" )}
}

actual_or_diff <- function(data,choice){
  if(choice == "Actual"){
    return(data)
  }
  
  if(choice == "Differenced"){
    diff_price <- diff(data,1)[-1,]
    return(diff_price)
  }
}

arima_model <- function(price,p,d,q,check){
  
  if(check){
    
    return(auto.arima(price, lambda = "auto",allowdrift = TRUE))
  }
  if(check== FALSE){
    
    return(Arima(price,order=c(p,d,q),include.drift=TRUE))
  }
}


arima_model_outputs <- function(model,p,d,q,choice,check){
  
  fit1<-model
  
  if(choice=="plot"){
    if(check==FALSE){
        tsdisplay(residuals(fit1),lag.max = 40,
            main = paste(c('(',p,',',d,',',q,') Model Residuals'), collapse=' '))
    }
    if(check==TRUE){
      tsdisplay(residuals(fit1),lag.max = 40,
                main = "Auto ARIMA model Residuals")
    }
  }
  if(choice=="summary"){
    print(summary(fit1))
  }
  if(choice=="aic_bic"){
     print(paste("AIC:",AIC(fit1)," BIC:",BIC(fit1)))
  }
}

arima_forecast <- function(model,forecasted_period){
  
  return(forecast(model, h=forecasted_period))
}

# """### **Forecasting Using K Nearest Neighbours**"""

KNN_model <- function(data,K,forecasting_period,choice){

  price <- as.numeric(x = data)
  knn_result <- knn_forecasting(price, h = forecasting_period, lags = 1:30, k = K,
                                msas = "MIMO")

  if(choice == "accuracy"){
    ro <- rolling_origin(knn_result)  #accuracy
    print(ro$global_accu)          #accuracy
  }
  if(choice=="plot"){
   plot(knn_result)
  }

}
  # """### **Forecasting using Feed Foward Neural network**"""

NN_model<-function(price,period_NN){
  
  alpha <- 1.5^(-10)
  hn <- length(price)/(alpha*(length(price)+30)) # calculating number of hidden nodes
  
  lambda <- BoxCox.lambda(price)
  dnn_pred <- nnetar(price, size= hn, lambda = lambda) # Single layer dnn
  
  dnn_forecast <- forecast(dnn_pred, h= period_NN, PI = TRUE)
  options(repr.plot.width = 30, repr.plot.height = 10)
  plot(dnn_forecast)
}
  

