#' Plots Basketball PCA
#'
#'
#'
#' @export


plot_pca <- function(data, xcol, ycol, d){
  data <- as.data.frame(pca_bball(data, d1 = d))
  data %>%
    ggplot(aes(x = data[,xcol], y = data[,ycol], color = conference, shape = as.factor(tournament))) +
    geom_point() +
    xlab(xcol) + 
    ylab(ycol)
}