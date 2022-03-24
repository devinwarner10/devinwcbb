#' Returns as a vector or string the win record of a given team.
#'
#' Asked in the homework as part of Question 3e. Counts the total number of 
#' games played by the team, calculates how many games the team won, and calculates
#' a win percentage. Returns a sentance (character string) or a numeric
#' vector based on the value of 'text'.
#'
#' @param data A tibble, preferable the kenpom data
#' @param team A string, the team to filter for
#' @param text Binary, 0 for vector, 1 for string
#'
#' @return numeric vector or character string
#'
#' @export
#'
#' @examples
#' team_win_record(get_cbb_data(), "Southern Utah")
#' team_win_record(get_cbb_data(), "Southern Utah", 1)
#' 

team_win_record <- function(data, team, text = 0) {
  x <- team_filter(data,team)
  tot_games <- x %>%
    summarise(n = n()) %>%
    pull()
  wins <- sum(ifelse(x$scoredifference > 0, 1, 0))
  
  if (text == 1) {
    cat(team, "has won", wins, "of their", tot_games, "games, for a win percentage of", round(wins / tot_games * 100, digits = 2), "%. \n")
  } else {
    c(wins, tot_games, round(wins / tot_games * 100, 2))
  }
}
