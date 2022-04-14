#' Runs a LASSO regression on Ken Pomeroy
#'
#'
#'
#'
#'
#' @export
#'
#'


cbb_lasso <- function(data, lambda){
  set.seed(2022)
  test_ind <- sample(nrow(data), 0.2*nrow(data))
  test <- data[test_ind,]
  train <- data[-test_ind,]
  bball_lasso <- cv.glmnet(as.matrix(train[,-c(1,2,12,13,14,15)]), train$win_perc_tot, alpha = 1)
  preds <- predict(bball_lasso, s = bball_lasso$lambda.min, newx = as.matrix(test[,-c(1,2,12,13,14,15)]))
  mse <- mean((preds - test$win_perc_tot)^2)
  text <- paste("The Elastic Net model with \u03B1 = 1 (LASSO) and \u03BB =", round(bball_lasso$lambda.min,6),
                "yields a training Mean-Squared Error (MSE) of", round(mse,4))
  
  return(list(bball_lasso, text))
}

