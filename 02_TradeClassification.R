# install and load the needed packages ------------------------------------
used_packages <- c("lubridate",   # date/time conversions
                   "caret",       # cross-fold generation
                   "party",       # decision tree classifier
                   "class",       # kNN classifier
                   "e1071",       # SVM classifier
                   "randomForest", # random forest classifier
                   "forecast"      # exponential smoothing
)
missing_packages <- setdiff(used_packages, installed.packages())
if(length(missing_packages)>0){install.packages(missing_packages)}
lapply(used_packages, require, character.only = TRUE)
rm(used_packages, missing_packages)

# load the datasets ----
trades_ETHEUR <- read.csv("trades_ETHEUR.csv", encoding = "UTF-8")
#trades_XBTEUR <- read.csv("trades_XBTEUR.csv", encoding = "UTF-8")

# ETHEUR Trades: between 2015-08-07 11:09:50 and 2017-06-28 20:51:20 ----
trades_ETHEUR$Time_Format <- as_datetime(trades_ETHEUR$Time)
trades_ETHEUR$BS <- as.factor(trades_ETHEUR$BS)
trades_ETHEUR$ML <- as.factor(trades_ETHEUR$ML)
summary(trades_ETHEUR$Time_Format)



#> calculate features per day ----
trades_ETHEUR$day <- date(trades_ETHEUR$Time_Format)
dailystats_ETHEUR <- data.frame(
  #avg_price = tapply(trades_ETHEUR$Price, trades_ETHEUR$day, mean), #TODO: is that correct?
  min_price = tapply(trades_ETHEUR$Price, trades_ETHEUR$day, min), 
  max_price = tapply(trades_ETHEUR$Price, trades_ETHEUR$day, max), 
  volume = tapply(trades_ETHEUR$Volume, trades_ETHEUR$day, sum)
)

open_times = tapply(trades_ETHEUR$Time, trades_ETHEUR$day, min)
close_times = tapply(trades_ETHEUR$Time, trades_ETHEUR$day, max)

dailystats_ETHEUR$open <- trades_ETHEUR$Price[match(open_times, trades_ETHEUR$Time)]
dailystats_ETHEUR$close <- trades_ETHEUR$Price[match(close_times, trades_ETHEUR$Time)]


price_diff_1d <- 0
dailystats_ETHEUR$price_rateOfChange_1d <- 0
dailystats_ETHEUR$price_rsi <-0
dailystats_ETHEUR$stochastic_oscilator <- 0
dailystats_ETHEUR$williams <- 0

dailystats_ETHEUR$obv <- 0
dailystats_ETHEUR$price_var_10d <- 0

dailystats_ETHEUR$price_rateOfChange_2d <- 0
dailystats_ETHEUR$price_rateOfChange_3d <- 0
dailystats_ETHEUR$price_rateOfChange_4d <- 0
dailystats_ETHEUR$price_rateOfChange_5d <- 0


gain_average <- 0
loss_average <- 0



for(i in 15:(nrow(dailystats_ETHEUR))){
  #Price Rate of Change
  price_diff_1d[i] <- dailystats_ETHEUR$close[i] - dailystats_ETHEUR$close[i-1]
  
  dailystats_ETHEUR[i, c("price_rateOfChange_1d", "price_rateOfChange_2d", "price_rateOfChange_3d", "price_rateOfChange_4d", "price_rateOfChange_5d")] <- 
    (dailystats_ETHEUR$close[i] - dailystats_ETHEUR$close[i-c(1:5)])#/dailystats_ETHEUR$close[i-c(1:5)]
  
  #Relative Strength Index
  gain_average <- mean(price_diff_1d [i + 1 - which(price_diff_1d[i- c(1:14)] > 0)])
  loss_average <- abs(mean(price_diff_1d [i + 1 - which(price_diff_1d[i- c(1:14)] < 0)]))
  
  dailystats_ETHEUR$price_rsi[i] <- (100 - 100/(1 + gain_average/loss_average))
  
  dailystats_ETHEUR$price_var_10d[i] <- var(dailystats_ETHEUR$close[i-c(0:9)])
  
  low14 <- min(dailystats_ETHEUR$min_price[i- c( 1 : 14)])
  high14 <- max(dailystats_ETHEUR$max_price[i- c( 1 : 14)])
  
  #Stochastic oscilator
  dailystats_ETHEUR$stochastic_oscilator[i] <- 100 * (dailystats_ETHEUR$close[i] - low14)/(high14 - low14)
  
  #Williams %R
  dailystats_ETHEUR$williams[i] <- (high14 - dailystats_ETHEUR$close[i])/(high14 - low14) * (-100)
  
  #On Balance Volume
  if(price_diff_1d[i] > 0 ) {

    dailystats_ETHEUR$obv[i] <- dailystats_ETHEUR$obv[i-1] + dailystats_ETHEUR$volume[i]

  }else if( price_diff_1d[i] < 0){

    dailystats_ETHEUR$obv[i] <- dailystats_ETHEUR$obv[i-1] - dailystats_ETHEUR$volume[i]
  }else {
    dailystats_ETHEUR$obv[i] <- dailystats_ETHEUR$obv[i-1]
  }
}


#> plot the price ranges per day ----
plot(dailystats_ETHEUR$close, type="l", ylim=c(0,400), main="Price per day ETHEUR",
     ylab="ETHEUR", xlab="Day since 2015-08-07")
# lines(dailystats_ETHEUR$max_price, col=2)
# lines(dailystats_ETHEUR$min_price, col=2)
polygon(x=c(1:nrow(dailystats_ETHEUR), nrow(dailystats_ETHEUR):1),
        y=c(dailystats_ETHEUR$max_price, 
            dailystats_ETHEUR$min_price[nrow(dailystats_ETHEUR):1]), 
        col = "lightblue", border = "lightblue")
lines(dailystats_ETHEUR$close)

#> prepare classification ----

threshold <- 0.125 * sd(diff(dailystats_ETHEUR$close))

dailystats_ETHEUR$nextdayprice <- ""
for(i in 1:(nrow(dailystats_ETHEUR)-1)){
  diff_nextday <- dailystats_ETHEUR$close[i+1] - dailystats_ETHEUR$close[i]
  if(diff_nextday > threshold){
    dailystats_ETHEUR$nextdayprice[i] <- "higher"
  } else if(diff_nextday < -threshold){
    dailystats_ETHEUR$nextdayprice[i] <- "lower"
  } else {
    dailystats_ETHEUR$nextdayprice[i] <- "equal"
  }
}
dailystats_ETHEUR$nextdayprice[dailystats_ETHEUR$nextdayprice==""] <- NA
dailystats_ETHEUR$nextdayprice <- ordered(dailystats_ETHEUR$nextdayprice, 
                                          levels=c("lower","equal" ,"higher"))

summary(dailystats_ETHEUR$nextdayprice)

#> prepare data for classification ----

#remove NA rows
dailystats_ETHEUR <- dailystats_ETHEUR[!apply(dailystats_ETHEUR, 1, anyNA), ]

# divide data in cross-folds
set.seed(1000)
xfolds <- createFolds(dailystats_ETHEUR$nextdayprice, k=10)

#> classification algorithms ----§§§

#the IDs for training and validation
test_cases     <- 1:nrow(dailystats_ETHEUR) %in% xfolds$Fold01
training_cases <- !test_cases

#>> decisoon tree ----

#train the model
model_DT <- ctree(nextdayprice ~ . , 
                  data=dailystats_ETHEUR[training_cases, ])

#predict test cases
clres_DT <- predict(model_DT, newdata=dailystats_ETHEUR[test_cases,])

#inspect the model - here the decision tree with decision borders and the distributions of the classes
plot(model_DT)

#>> random forest ----

#train the model
model_RF <- randomForest(nextdayprice ~ . , 
                         data=dailystats_ETHEUR[training_cases, ],
                         importance=T)

#predict test cases
clres_RF <- predict(model_RF, newdata=dailystats_ETHEUR[test_cases,])

#inspect the model - here variable importances, see ?importance for the definition of measures
importance(model_RF)

#>> kNN ----
features <- setdiff(colnames(dailystats_ETHEUR), "nextdayprice")

testdata  <- dailystats_ETHEUR[test_cases, ]
traindata <- dailystats_ETHEUR[training_cases, ]

# predict test cases from training data (lazy learning algoritm has no explicit training step!)
clres_KNN <- knn(train = traindata[, features], test = testdata[, features], 
                 cl = traindata$nextdayprice, k = 5)


#>> kNN with probabilities ----
testdata  <- dailystats_ETHEUR[test_cases, ]
traindata <- dailystats_ETHEUR[training_cases, ]

clres_KNN <- knn(train = traindata[, features], test = testdata[, features], 
                 cl = traindata$nextdayprice, k = 5, prob = T)

prob <- attr(clres_KNN, "prob")


#>> SVM ----

#train the model
model_SVM <- svm(nextdayprice ~ . , data=traindata)

#predict the test cases
clres_SVM <- predict(model_SVM, newdata=testdata)

#>> SVM with probabilities ----
model_SVM <- svm(nextdayprice ~ . , data=traindata, probability = T)
clres_SVM <- predict(model_SVM, newdata=testdata, probability = T)

high_buying_props <- order(attributes(clres_SVM)$probabilities[,"higher"], decreasing = T)
attributes(clres_SVM)$probabilities[high_buying_props, ]


#> Simple model evaluation ---------------------------------------------

cm <- table(clres_RF, dailystats_ETHEUR[test_cases,"nextdayprice"])
(accuracy_RF <- (sum(diag(cm))/sum(as.vector(cm))))

cm <- table(clres_DT, dailystats_ETHEUR[test_cases,"nextdayprice"])
(accuracy_DT <- (sum(diag(cm))/sum(as.vector(cm))))

cm <- table(clres_SVM, dailystats_ETHEUR[test_cases,"nextdayprice"])
(accuracy_SVM <- (sum(diag(cm))/sum(as.vector(cm))))


cm <- table(clres_KNN, dailystats_ETHEUR[test_cases,"nextdayprice"])
(accuracy_KNN <- (sum(diag(cm))/sum(as.vector(cm))))























































































# XBTEUR Trades: between 2013-09-10 23:47:11 and 2017-06-28 20:49:23 ----
# trades_XBTEUR$Time_Format <- as_datetime(trades_XBTEUR$Time)
# trades_XBTEUR$BS <- as.factor(trades_XBTEUR$BS)
# trades_XBTEUR$ML <- as.factor(trades_XBTEUR$ML)
# summary(trades_XBTEUR$Time_Format)

