#' Time Series Graph
#'
#' For a given basketball team, creates a line graph with date on the x-axis and
#' score on the y-axis. Has two lines for team score and opponent score. Includes
#' an option to use regular lines or a smoother. 
#'
#' @param data Tibble of Ken Pomeroy data
#' @param team Character of desired string
#' @param smooth Binary Use smoother. 
#' 
#' @return A ggplot geom_line or geom_smooth
#'
#' @export
#' 
#' @examples 
#' 
#' ## Regular line plot for Duke
#' time_series_graph(get_cbb_data(), "Duke")
#' 
#' ## Smoothed line plot for Duke
#' time_series_graph(get_cbb_data(), "Duke", TRUE)
#'

time_series_graph <- function(data, team, smooth){
  
  if(smooth == 0){
    team_filter(data,team) %>%
      ggplot(aes(x = date)) +
      geom_line(aes(y = teamscore, color = "Team Score"), size = 1, alpha = 0.8) +
      geom_point(aes(y = teamscore, color = "Team Score"), size = 1.5) +
      geom_line(aes(y = opponentscore, color = "Opponent Score"), size = 1, alpha = 0.6) +
      geom_point(aes(y = opponentscore, color = "Opponent Score"), size = 1.5) +
      scale_color_manual(name = team, 
                         values = c("Team Score" = "steelblue2",
                                    "Opponent Score" = "springgreen3")) +
      labs(x = "Game Date", y = "Score", title = paste(team,"Scores Over Time"))
  }else if(smooth == 1){
    team_filter(data,team) %>%
      ggplot(aes(x = date)) +
      geom_smooth(aes(y = teamscore, color = "Team Score"), size = 1, alpha = 0.8) +
      geom_smooth(aes(y = opponentscore, color = "Opponent Score"), size = 1, alpha = 0.6) +
      scale_color_manual(name = team, 
                         values = c("Team Score" = "steelblue2",
                                    "Opponent Score" = "springgreen3")) +
      labs(x = "Game Date", y = "Score", title = paste(team,"Scores Over Time"))
  }
  
}



