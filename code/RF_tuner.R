# function to fit RF models taken from Chris Free 
# https://github.com/cfree14/domoic_acid/blob/29e49da9ec4b16b6d416d389d37867e4b4b7f95d/code/functions/fit_rf.R

fit_rf <- function(data, formu, up=TRUE){ # set up to take formula as a string

  # Define tuning parameter grid
  # mtry = Number of variables randomly sampled as candidate variables per tree
  fitGrid <- expand.grid(mtry=seq(3, 33, 2))
  
  # Define tuning and training method
  fitControl <- caret::trainControl(method="repeatedcv"
                                    , number=10
                                    , repeats=10  #tenfold cross-validation, i think
                                    # , verboseIter = T
                                    , if(up){sampling = "up"}
                                    )
  
  # Train RF model
  rf_fit <- caret::train(formu 
                         , data = data
                         , method = "rf" # this implements randomForest::randomForest but with controls 
                         , distribution = "bernoulli" 
                         , metric = "Kappa" # for more robust results with class imbalance
                         , tuneGrid = fitGrid # try trees with different numbers of variables
                         , trControl = fitControl # cross validation
                         , na.action = na.pass # shouldn't be an issues here
                         # , verbose = T 
                         , importance = T
                         # might tell me something interesting.
                         )
  
  # Plot RF tuning results
  rf_fit$bestTune
  rf_tune <- rf_fit$results
  g <- ggplot(rf_tune, aes(x=mtry, y=Kappa)) +
    labs(x="Number of variables\nsampled at each split" , y="Cohen's kappa", main="Random forest model tune") +
    geom_line() +
    geom_point() +
    theme_bw()
  print(g)
  
  # Return fit
  return(rf_fit)
  
}
