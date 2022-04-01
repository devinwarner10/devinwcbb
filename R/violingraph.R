#' Builds a plot of Score Difference distribution as a violin graph
#'
#' Works with the devinwcbb Shiny app to display a violin distribution of score
#' differences between the selected team and their opponent. A positive score
#' represents a win and a negative score represents a loss. 
#'
#' @param data A tibble of the Ken Pomeroy data
#' @param team Character of the desired team
#' 
#' @return A ggplot geom_violin
#' 
#' @export
#' 
#' @example 
#' violingraph(get_cbb_data(), "Southern Utah")
#'
#'


violingraph <- function(data, team){
  team_filter(data,team) %>%
    mutate('Team' = team) %>%
    mutate('Result' = result) %>%
    ggplot(aes(x = Team, y = scoredifference)) +
    geom_violin(fill = "mistyrose1", color = "black") +
    geom_jitter(aes(color = Result), width = 0.02) +
    geom_abline(y = 0, color = "red", fill = "red") +
    guides(color = guide_legend(reverse = TRUE)) +
    labs(y = paste("Score Difference (",team, " - Opponent)"), 
         title = "Distribution of Score Differences")
}

