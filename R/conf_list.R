#' Creates a list of conferences
#'
#' Takes the basketball data and creates a list of distinct conferences featured
#' in the dataset. 
#'
#' @param data A tibble of the Ken Pom data
#'
#' @return tibble  
#'
#' @export
#' 

conf_list <- function(conf_data){
  conf_data %>% 
    select(conference) %>%
    distinct()
}