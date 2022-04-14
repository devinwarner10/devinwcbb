#' Imports Ken Pomeroy Data
#'
#' Reads the data from the URL listed below. Using dylr methods puts the data 
#' into tidy format. Includes an option to filter the data for conference games
#' only. 
#' 
#' Requires the tidyverse and lubridate packages. 
#' 
#' The Ken Pomeroy data can be found at http://kenpom.com/cbbga22.txt
#' 
#' Conference data was provided by https://www.kaggle.com/c/mens-march-mania-2022/data
#' 
#' Full conference names were edited to match the format found in the Ken Pomeroy data. 
#' 
#' @param conf Boolean Filter for conference games only
#'
#' @return A tibble. Each row represents a game in the 2021-2022 season. 
#'
#' @export
#' 
#' @examples 
#' 
#' ## Get full data set
#' get_cbb_data()
#' 
#' ## Get conference data
#' get_cbb_data(1)
#'

get_cbb_data <- function(conf = FALSE) {
  
  data <- read_fwf(file = url("http://kenpom.com/cbbga22.txt"), 
                   col_positions = fwf_widths(c(11, 23, 4, 23, 4, 3, 22), 
                                              c("date", "vis", "score2", "home", "score1", "ufo1", "ufo2")), col_types = "ccicicc")

  data <- data %>%
    mutate(location = home) %>%
    mutate(score_diff = score1 - score2) %>%
    select(-c(6, 7))

  data$date <- mdy(data$date)
  
  team_id <- read_csv("data/MTeams.csv")
  team_conf <- read_csv("data/MTeamConferences.csv") %>% 
    filter(Season == 2022)
  conf_names <- read_csv("data/Conferences.csv")
  
  conf_names$Description %>%
    str_replace_all(' Conference', '') ->
    conf_names$Description
  step1 <- merge(x = team_conf, y = conf_names, by = "ConfAbbrev", all.x = TRUE)
  step2 <- merge(x = team_id, y = step1, by = "TeamID", all.x = TRUE)
  step3 <- select(step2, c("TeamName", "conference" = "Description"))
  

  data <- merge(x = data, y = step3, by.x = "home", by.y = "TeamName", all.x = TRUE)
  data <- rename(data, home_conference = conference)
  data <- merge(x = data, y = step3, by.x = "vis", by.y = "TeamName", all.x = TRUE)
  data <- rename(data, vis_conference = conference)

  data <- arrange(data, date) %>% select(c("date", "home", "score1","vis", "score2",
                                           "location", "score_diff", "home_conference",
                                           "vis_conference"))
  
  if(conf){
    data %>% 
      filter(home_conference == vis_conference)  %>%
      select(c("date", "home", "score1","vis", "score2",
               "location", "score_diff", "home_conference")) %>%
      rename(conference = home_conference)
  }else if(!conf){data}
}
