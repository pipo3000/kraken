#' This function calculates features from a trade dataset (data.frame) with the variables:
#'   - "Price": the price of the trade  
#'   - "Volume": the amount of currency traded
#'   - "Time": the UNIX timestamp 
#' @param trades_data the data.frame with trades data
#' @return feature vector with daily features
#' @author Konstantin Hopf <konstantin.hopf@uni-bamberg>,
#'         Da Conceição Barata  Filipe <fbarata@ethz.ch>
extract_trade_features <- function(trades_data) {
  
  #format the time in the R format (POSIXt)
  trades_data$Time_Format <- as_datetime(trades_data$Time)
  trades_data$day <- date(trades_data$Time_Format)
  
  #create data frame for features
  daily_features <- data.frame(
    #avg_price = tapply(trades_data$Price, trades_data$day, mean), #TODO: is that correct?
    min_price = tapply(trades_data$Price, trades_data$day, min), 
    max_price = tapply(trades_data$Price, trades_data$day, max), 
    volume = tapply(trades_data$Volume, trades_data$day, sum)
  )
  
  open_times = tapply(trades_data$Time, trades_data$day, min)
  close_times = tapply(trades_data$Time, trades_data$day, max)
  
  daily_features$open <- trades_data$Price[match(open_times, trades_data$Time)]
  daily_features$close <- trades_data$Price[match(close_times, trades_data$Time)]
  
  
  price_diff_1d <- 0
  daily_features$price_rateOfChange_1d <- 0
  daily_features$price_rsi <-0
  daily_features$stochastic_oscilator <- 0
  daily_features$williams <- 0
  
  daily_features$obv <- 0
  daily_features$price_var_10d <- 0
  
  daily_features$price_rateOfChange_2d <- 0
  daily_features$price_rateOfChange_3d <- 0
  daily_features$price_rateOfChange_4d <- 0
  daily_features$price_rateOfChange_5d <- 0
  
  
  gain_average <- 0
  loss_average <- 0
  
  
  
  for(i in 15:(nrow(daily_features))){
    #Price Rate of Change
    price_diff_1d[i] <- daily_features$close[i] - daily_features$close[i-1]
    
    daily_features[i, c("price_rateOfChange_1d", "price_rateOfChange_2d", "price_rateOfChange_3d", "price_rateOfChange_4d", "price_rateOfChange_5d")] <- 
      (daily_features$close[i] - daily_features$close[i-c(1:5)])#/daily_features$close[i-c(1:5)]
    
    #Relative Strength Index
    gain_average <- mean(price_diff_1d [i + 1 - which(price_diff_1d[i- c(1:14)] > 0)])
    loss_average <- abs(mean(price_diff_1d [i + 1 - which(price_diff_1d[i- c(1:14)] < 0)]))
    
    daily_features$price_rsi[i] <- (100 - 100/(1 + gain_average/loss_average))
    
    daily_features$price_var_10d[i] <- var(daily_features$close[i-c(0:9)])
    
    low14 <- min(daily_features$min_price[i- c( 1 : 14)])
    high14 <- max(daily_features$max_price[i- c( 1 : 14)])
    
    #Stochastic oscilator
    daily_features$stochastic_oscilator[i] <- 100 * (daily_features$close[i] - low14)/(high14 - low14)
    
    #Williams %R
    daily_features$williams[i] <- (high14 - daily_features$close[i])/(high14 - low14) * (-100)
    
    #On Balance Volume
    if(price_diff_1d[i] > 0 ) {
      
      daily_features$obv[i] <- daily_features$obv[i-1] + daily_features$volume[i]
      
    }else if( price_diff_1d[i] < 0){
      
      daily_features$obv[i] <- daily_features$obv[i-1] - daily_features$volume[i]
    }else {
      daily_features$obv[i] <- daily_features$obv[i-1]
    }
  }
  return(daily_features)
}