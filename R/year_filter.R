#' Filters a tibble with a 'date' column for given year
#'
#' The homework asked to do this as part of question 3d. Right now this
#' only works for year values of 2021 and 2022.
#'
#' @param data A tibble, preferable the kenpom data
#' @param year An integer value representing the desired year
#'
#' @export
#'
#' @return tibble
#'
#' @examples
#' year_filter(get_cbb_data(), 2021)
year_filter <- function(data, year = 2022) {
  data %>% filter(year(date) == year)
}
