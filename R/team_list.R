#' Creates a list of teams
#'
#' Takes the basketball data and creates a list of distinct teams featured
#' in the dataset. 
#'
#' @param data A tibble of the Ken Pom data
#'
#' @return tibble  
#'
#' @export
#' 

team_list <- function(data){
  tibble(team = append(data$home, data$vis)) %>% distinct()
}