#' Filters the kenpom data by given team.
#'
#' Asked in the homework as part of Question 3d. Uses the filter function
#' to filter for a given team whether they were the home or visiting team.
#'
#' @param data A tibble, preferably the kenpom data
#' @param team A string, the team to filter for
#'
#' @return tibble
#'
#' @export
#'
#' @examples
#' team_filter(get_cbb_data(), "Southern Utah")
#' team_filter(get_cbb_data(), "Northern Arizona")
#' 
#' 
team_filter <- function(data, team) {
    x <- filter(data, home == team | vis == team)
    x$opponent <- ifelse(x$home == team, x$vis, x$home)
    x$teamscore <- ifelse(x$home == team, x$score1, x$score2)
    x$opponentscore <- ifelse(x$home == team, x$score2, x$score1)
    x$scoredifference <- x$teamscore - x$opponentscore
    x$result <- ifelse(x$scoredifference > 0, "Win", "Loss")
    x[,c("date","opponent", "teamscore", "opponentscore", "location", "scoredifference", "result")]
}
