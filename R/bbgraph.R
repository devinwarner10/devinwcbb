#' Scatter Plot of Basketball Game Scores
#'
#' For a filtered men's college basketball team builds a scatter plot where 
#' opponent score is on the x-axis, and team score is on the y-axis. A y = x line
#' is also shown to show which games are close or not
#'
#' @param data A tibble of the Ken Pomeroy basketball data
#' @param team Character value of the desired team. 
#'
#' @return A geom_point plot. 
#'
#' @export
#' 
#' @example
#' 
#' bbgraph(get_cbb_data(), "Duke")
#'

bbgraph <- function(data, team = NULL) {
  if (is_null(team)) {
    g <- data %>% ggplot(aes(x = score1, y = score2))
    g + geom_point() + labs(x = "Home Score", y = "Visiting Score",
                            title = "Basketball Scores")
  } else{
    data <- data %>% team_filter(team)
    
    data %>% 
      mutate("Result" = result) %>%
      ggplot(aes(x = opponentscore, y = teamscore, color = Result)) + 
      geom_point(size = 5) + 
      geom_abline() +
      guides(color = guide_legend(reverse = TRUE)) +
      labs(y = paste(team, "Score"), x = "Opponent Score", title = paste(team, "Basketball Scores"))
  }
  
}