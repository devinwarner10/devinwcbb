#' Creates a ggplot of Team and Opponent scores over time
#'
#'
#'
#'
#' @param data kenpom data set
#' @param team character string of team name
#' 
#' @return ggplot2
#'
#' @export
#'

time_series_graph <- function(data, team){
  team_filter(data,team) %>%
    ggplot(aes(x = date)) +
    geom_line(aes(y = teamscore, color = "Team Score"), size = 1, alpha = 0.8) +
    geom_point(aes(y = teamscore, color = "Team Score"), size = 1.5) +
    geom_line(aes(y = opponentscore, color = "Opponent Score"), size = 1, alpha = 0.6) +
    geom_point(aes(y = opponentscore, color = "Opponent Score"), size = 1.5) +
    scale_color_manual(name = team, 
                       values = c("Team Score" = "steelblue2",
                                  "Opponent Score" = "springgreen3")) +
    labs(x = "Game Date", y = "Score", title = "Game Scores Over Time")
}



