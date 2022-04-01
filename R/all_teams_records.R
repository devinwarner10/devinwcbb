#' Builds Team Win Records Table
#'
#' Takes the Ken Pomeroy basketball data and builds a tibble of each teams 
#' win record. Give the full Ken Pomeroy data to get total ranking. Give the
#' Ken Pomeroy data filtered for conference games to get conference rankings.
#'
#' @param data A tibble of the Ken Pomeroy basketball data
#'
#' @return A tibble. The rows are individual teams and sorted by weighted rank. 
#' The columns give games won, total games, and win percentage. 
#'
#' @export
#'
#' @examples 
#' 
#' ## Get total ranking for each team
#' all_teams_records(get_cbb_data(0))
#' 
#' 
#' ## Get conference ranking for each team
#' all_teams_records(get_cbb_data(1))
#'

all_teams_records <- function(data) {
  team_names <- team_list(data)
  teams_tibble <- tibble(team = character(), wins = integer(), total_games = integer(), 
                         win_percentage = numeric())
  for (i in 1:nrow(team_names)) {
    x <- pull(team_names)[i]
    X <- team_win_record(data,x)
    teams_tibble <- teams_tibble %>% 
      add_row(team = x, wins = X[1], total_games = X[2], 
              win_percentage = X[3]) 
  }
  avg_win_perc <- mean(teams_tibble$win_percentage)
  teams_tibble %>% 
    mutate("weighted_wins" = (wins^2 ) / (total_games) ) %>%
    arrange(-weighted_wins)
}
