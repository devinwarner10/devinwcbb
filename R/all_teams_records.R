#' Builds a tibble of all college men's basketball teams win records.
#'
#' Asked in the homework as part of Question 3e. Builds a column of unique
#' team names. Using the 'team_win_record' function with the vector output,
#' builds a tibble with the columns 'team', 'wins', 'total_games', and
#' 'win_percentage'. Sorts the final output by a weighted win percentage. 
#'
#' @param data A tibble, preferably the kenpom data
#'
#' @return tibble
#'
#' @export
#'
#' @example
#' all_teams_records(get_cbb_data())
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
