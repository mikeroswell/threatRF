# function to fit RF models taken from Chris Free 
# https://github.com/cfree14/domoic_acid/blob/29e49da9ec4b16b6d416d389d37867e4b4b7f95d/code/functions/fit_rf.R

fit_rf <- function(data, formu, sampling = NULL, tuneMethod = "none", repeats = NULL){ # set up to take formula as a string
  
  # Define tuning parameter grid
  # mtry = Number of variables randomly sampled as candidate variables per tree
  fitGrid <- expand.grid(mtry=seq(2, 25, 1))
  
  # Define tuning and training method
  
  
  fC = caret::trainControl(method = tuneMethod
                           , number = ifelse(tuneMethod =="repeatedcv", 10, NA)
                           , repeats = ifelse(is.null(repeats), ifelse(tuneMethod != "none", 10, NA), repeats)
                           , sampling =  sampling
                           , classProbs = TRUE
                           , summaryFunction = twoClassSummary)
  
  
  # Train RF model
  rf_fit <- caret::train(formu 
                         , data = data
                         , method = "rf" # this implements randomForest::randomForest but with controls 
                         , distribution = "bernoulli" 
                         , metric = "ROC" # for more robust results with class imbalance
                         , tuneGrid =if(tuneMethod !="none"){fitGrid} else{NULL} # try trees with different numbers of variables
                         , trControl = fC # cross validation and samplling
                         , na.action = na.pass # shouldn't be an issues here
                         # , verbose = T 
                         , importance = T
                         # might tell me something interesting.
  )
  
  # Plot RF tuning results
  # rf_fit$bestTune
  # rf_tune <- rf_fit$results
  # 
  
  
  # Return fit
  return(rf_fit)
  
}