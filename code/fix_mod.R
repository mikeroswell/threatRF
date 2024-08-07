# Functions to deal with novel factor levels in RF predictions
# Note that this blunt tool may be worse than available imputation methods, c.f.
# https://www.randomforestsrc.org/index.html

#' Indices for categorical data
#'
#' @param x `data.frame`
#'
#' @return Logical vector of `length(x)`, `TRUE` for categorical, `FALSE`
#'   otherwise
#' @export
#'
#' @examples
#' set.seed(123)
#' dat <- data.frame( 
#' y = sample(1:2, 5, replace =T)
#' , x1 = 1:5
#' , x2 = letters[1:5]
#' , x3 = LETTERS[1:5]
#'   )
#' 
#' is_categorical(dat)
is_categorical <- function(x) {
  sapply(x, function(y) {is.factor(y) | is.character(y)})
}


# fix the model with blind attempt
#' Add unseen levels to fit object
#'
#' @param mod model object
#' @param test.dat `data.frame`, "new.data" on which to predict value from `mod`
#'
#' @return revised model object without missing factor levels
#' @export
#'
#' @examples
#' 
#' # make data
#' set.seed(123)
#' dat <- data.frame( 
#' y = sample(0:1, 7, replace =T)
#' , x1 = 1:7
#' , x2 = letters[1:7]
#' , x3 = LETTERS[1:7]
#'   )
#'   
#' # split into test and train
#' test <- dat[1, ]
#' train <- dat[2:7, ]
#' 
#' # fit model on test data
#' my_mod <- glm(y ~ ., data = train, family = "binomial")
#' 
#' # predict throws an error
#' # predict(my_mod, test)
#' 
#' # after fix, predict does not throw error
#' predict(fix.mod(my_mod, test, resp = "y"), test)
#' 
fix.mod <- function(mod, test.dat, resp){
  v.names <- is_categorical(test.dat)
  # v.names <- v.names[which(v.names != resp)]
  v.names[which(names(v.names) %ni% names(mod$xlevels))] <- FALSE # Not sure this edit was
  # called for but it seemed like maybe it was the move. 
  mod$xlevels = Map(union
                    , mod$xlevels
                    , lapply(test.dat[v.names], unique)
                    , USE.NAMES = TRUE)
  return(mod)
}

# fix the input variable types
fix.types <- function(mod, test.dat, resp){
  v.names <- is_categorical(test.dat)
  v.names <- v.names[which(v.names != resp)]
  mod$xlevels = Map(union
                    , mod$xlevels
                    , lapply(test.dat[v.names], unique)
                    , USE.NAMES = TRUE)
  return(mod)
}

# drop problematic variables
# compare factor levels from two dataframes with identical variables
nomatch <- function(x, y){
  colnames(x)[!sapply(1:length(colnames(x)), function(x.name){
    all(y[ ,x.name] %in% x[ ,x.name])
  })]
}
