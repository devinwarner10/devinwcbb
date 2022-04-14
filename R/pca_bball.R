#' Calculates Principle Components for Basketball Shiny
#'
#'
#'
#'
#'
#'
#' @export
#'

pca_bball <- function(data, val = 1){
  pr <- prcomp(data[,-c(1,2,16)])
  if(val == 1){
    x <- pr$x
    pc1 <- x[,1]
    pc2 <- x[,2]
    pc3 <- x[,3]
    pc4 <- x[,4]
    pc5 <- x[,5]
    pc6 <- x[,6]
    pc7 <- x[,7]
    pc8 <- x[,8]
    pc9 <- x[,9]
    pc10 <- x[,10]
    pc11 <- x[,11]
    pc12 <- x[,12]
    pc13 <- x[,13]
    tibble("team" = data$team, "conference" = data$conference,"tournament" = data$tournament,
           "PC1" = pc1, "PC2" = pc2,"PC3" = pc3,"PC4" = pc4,
           "PC5" = pc5, "PC6" = pc6,"PC7" = pc7,"PC8" = pc8,
           "PC9" = pc9, "PC10" = pc10,"PC11" = pc11,"PC12" = pc12, "PC13" = pc13)
  }else if(val == -1){
    x <- round(summary(pr)["importance"]$importance,3)
    tibble("Metric" = c("Standard Deviation", "Proportion of Variance", "Cumulative Proportion"),
           "PC1" = x[,1],"PC2" = x[,2],"PC3" = x[,3],"PC4" = x[,4],"PC5" = x[,5],"PC6" = x[,6],
           "PC7" = x[,7],"PC8" = x[,8],"PC9" = x[,9],"PC10" = x[,10],"PC11" = x[,11],"PC12" = x[,12],
           "PC13" = x[,13])
  }else{
    y <- tibble(names(data[,-c(1,2,16)]), round(pr$rotation[,1],4), 
                round(pr$rotation[,2],4), round(pr$rotation[,3],4), round(pr$rotation[,4],4),
                round(pr$rotation[,5],4), round(pr$rotation[,6],4), round(pr$rotation[,7],4),
                round(pr$rotation[,8],4), round(pr$rotation[,9],4), round(pr$rotation[,10],4),
                round(pr$rotation[,11],4), round(pr$rotation[,12],4), round(pr$rotation[,13],4))
    tibble("Variable" = y[,1], "PC1" = y[,2], "PC2" = y[,3], "PC3" = y[,4], "PC4" = y[,5],
           "PC5" = y[,6], "PC6" = y[,7], "PC7" = y[,8], "PC8" = y[,9],
           "PC9" = y[,10], "PC10" = y[,11], "PC11" = y[,12], "PC12" = y[,13], "PCA13" = y[,14])
  }
}