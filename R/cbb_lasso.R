#' Runs a LASSO regression on Ken Pomeroy
#'
#'
#'
#'
#'
#' @export
#'
#'


cbb_lasso <- function(data, lambda = NA){
  set.seed(2022)
  test_ind <- sample(nrow(data), 0.2*nrow(data))
  test <- data[test_ind,]
  train <- data[-test_ind,]
  bball_lasso <- cv.glmnet(as.matrix(train[,-c(1,2,12,13,14,15)]), train$win_perc_tot, alpha = 1)
  if(is.na(lambda)){lambda <- bball_lasso$lambda.min}
  preds <- predict(bball_lasso, s = lambda, newx = as.matrix(test[,-c(1,2,12,13,14,15)]))
  mse <- mean((preds - test$win_perc_tot)^2)
  rsq_plot = 1 - bball_lasso$cvm/var(train$win_perc_tot)
  rsq = glmnet(as.matrix(train[,-c(1,2,12,13,14,15)]), train$win_perc_tot, lambda = lambda, alpha = 1)$dev.ratio
  text <- paste("The Elastic Net model with \u03B1 = 1 (LASSO) and log(\u03BB) =", round(log(lambda),2),
                "yields a R^2 of", round(rsq,4), " and a testing Mean-Squared Error (MSE) of", round(mse,4))
  
  return(list(bball_lasso, text, rsq_plot))
}

 
