# install and load the needed packages ------------------------------------
used_packages <- c("lubridate",    # date/time conversions
                   "caret",        # cross-fold generation
                   "party",        # decision tree classifier
                   "class",        # kNN classifier
                   "e1071",        # SVM classifier
                   "randomForest", # random forest classifier
                   "forecast"      # exponential smoothing
)
missing_packages <- setdiff(used_packages, installed.packages())
if(length(missing_packages)>0){install.packages(missing_packages)}
lapply(used_packages, require, character.only = TRUE)
rm(used_packages, missing_packages)

source("F_extract_trade_features.R")

# load the datasets ----
trades_ETHEUR <- read.csv("trades_ETHEUR.csv", encoding = "UTF-8")
#trades_XBTEUR <- read.csv("trades_XBTEUR.csv", encoding = "UTF-8")

# ETHEUR Trades: between 2015-08-07 11:09:50 and 2017-06-28 20:51:20 ----
#trades_ETHEUR$BS <- as.factor(trades_ETHEUR$BS) #variables not used
#trades_ETHEUR$ML <- as.factor(trades_ETHEUR$ML)

#> calculate features per day ----
dailystats_ETHEUR <- extract_trade_features(trades_ETHEUR)

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

#> classification algorithms (iteration on x-folds) ----

#a matrix for the results
class_acc <- matrix(NA, nrow=4, ncol=10, 
                    dimnames = list(c("DT", "RF", "kNN", "SVM"),1:10))

for(xfold in 1:10){
  #the IDs for training and validation
  test_cases     <- 1:nrow(dailystats_ETHEUR) %in% xfolds[[xfold]]
  training_cases <- !test_cases
  
  #>> decisoon tree ----
  
  #train the model
  model_DT <- ctree(nextdayprice ~ . , 
                    data=dailystats_ETHEUR[training_cases, ])
  
  #predict test cases
  clres_DT <- predict(model_DT, newdata=dailystats_ETHEUR[test_cases,])
  
  #inspect the model - here the decision tree with decision borders and the distributions of the classes
  #plot(model_DT)
  
  #>> random forest ----
  
  #train the model
  model_RF <- randomForest(nextdayprice ~ . , 
                           data=dailystats_ETHEUR[training_cases, ],
                           importance=T)
  
  #predict test cases
  clres_RF <- predict(model_RF, newdata=dailystats_ETHEUR[test_cases,])
  
  #inspect the model - here variable importances, see ?importance for the definition of measures
  #importance(model_RF)
  
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
  #attributes(clres_SVM)$probabilities[high_buying_props, ]
  
  
  #> Simple model evaluation ---------------------------------------------

  cm <- table(clres_RF, dailystats_ETHEUR[test_cases,"nextdayprice"])
  class_acc["RF", xfold]  <- (sum(diag(cm))/sum(as.vector(cm)))
  
  cm <- table(clres_DT, dailystats_ETHEUR[test_cases,"nextdayprice"])
  class_acc["DT", xfold] <- (sum(diag(cm))/sum(as.vector(cm)))
  
  cm <- table(clres_SVM, dailystats_ETHEUR[test_cases,"nextdayprice"])
  class_acc["SVM", xfold] <- (sum(diag(cm))/sum(as.vector(cm)))
  
  cm <- table(clres_KNN, dailystats_ETHEUR[test_cases,"nextdayprice"])
  class_acc["kNN", xfold] <- (sum(diag(cm))/sum(as.vector(cm)))
  
}

#> Plot the accuracies ----
acc_means <- rowMeans(class_acc)
acc_sds <- apply(class_acc, 1, sd)

bp <- barplot(acc_means, #the data
              main="Classification accuracies",
              ylab="Accuracy (10-fold cross-validation)",
              ylim=c(.2,1), #change the y-axis limits
              col="goldenrod",
              xpd=F) #cut the bars at x-axis

#bp contains the centers of the bars, add now the standard error-bars
segments(x0 = bp, x1 = bp, y0 = acc_means+acc_sds, y1 = acc_means - acc_sds)
seb <- 0.05 #size of the error bars
segments(x0 = bp-seb, x1 = bp+seb, y0 = acc_means+acc_sds, y1 = acc_means+acc_sds)
segments(x0 = bp-seb, x1 = bp+seb, y0 = acc_means-acc_sds, y1 = acc_means-acc_sds)
