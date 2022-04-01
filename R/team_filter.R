#' Filter Ken Pomeroy data
#'
#' Takes the Ken Pomeroy data and filters for all games that have a given team.
#'
#' @param data Tibble of the Ken Pomeroy data
#' @param team Character of the desired string
#'
#' @return A tibble subset of the Ken Pomeroy data
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
