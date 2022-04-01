#' Creates a list of conferences
#'
#' Takes the Ken Pomeroy basketball data and creates a list of distinct 
#' conferences featured in the data set. Used to create the conference selection
#' drop down in the Shiny app. 
#'
#' @param data A tibble of the Ken Pomeroy data filtered for conference games
#'
#' @return A tibble. Has one column of distinct conference names. 
#'
#' @export
#' 
#' @example 
#' conf_list(get_cbb_data(1))
#' 

conf_list <- function(conf_data){
  conf_data %>% 
    select(conference) %>%
    distinct()
}