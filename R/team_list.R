#' Create a List of Teams
#'
#' Takes the Ken Pomeroy basketball data and creates a list of distinct teams featured
#' in the data set. Used for the Team drop down selection in the Shiny app. 
#'
#' @param data Tibble of the Ken Pomeroy data.
#'
#' @return A tibble with one column of distinct teams 
#'
#' @export
#' 

team_list <- function(data){
  tibble(team = append(data$home, data$vis)) %>% 
    distinct()
}
