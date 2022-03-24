#' Imports and cleans the KenPom college basketball data set
#'
#' Reads in the file using a fixed width method. Names the columns and sets
#' column types. Adds the column 'location' based on the home team. Adds the
#' column 'score-diff' found by 'score_1' - 'score_2'. Removes columns 6 and 7.
#' Changes the 'date' column from a string to a date. Returns the cleaned dataset.
#' Requires the tidyverse and lubridate packages. 
#'
#' @return tibble
#'
#' @export
#'

get_cbb_data <- function() {
  
  data <- read_fwf(file = url("http://kenpom.com/cbbga22.txt"), col_positions = fwf_widths(c(11, 23, 4, 23, 4, 3, 22), c("date", "home", "score1", "vis", "score2", "ufo1", "ufo2")), col_types = "ccicicc")

  data <- data %>%
    mutate(location = home) %>%
    mutate(score_diff = score1 - score2) %>%
    select(-c(6, 7))

  data$date <- mdy(data$date)

  data
}
