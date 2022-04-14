#' Creates a tibble of Team Stats
#'
#' This code does not run when the Shiny App is run. It takes too long. The
#' output of this function is saved in data/team_records.csv in the package. 
#'
#'
#' @export
#'

team_record_2 <- function(data){
  team_names <- team_list(data)
  teams_tibble <- tibble()
  
  for(i in 1:nrow(team_names)){
    x <- pull(team_names)[i]
    record <- team_win_record(data, x)
    
    teams_tibble[i,"team"] <- x
    teams_tibble[i,"conference"] <- data %>% filter(home == x) %>% summarise(max(home_conference))
    teams_tibble[i,"avg_point_sc_home"] <- data %>% filter(home == x) %>% summarise(mean(score1))
    teams_tibble[i,"avg_point_sc_vis"] <- data %>% filter(vis == x) %>% summarise(mean(score2))
    teams_tibble[i,"avg_point_sc_tot"] <- data %>% filter(home == x | vis == x) %>% summarise(mean(ifelse(home == x, score1, score2)))
    teams_tibble[i,"avg_point_allowed_home"] <- data %>% filter(home == x) %>% summarise(mean(score2))
    teams_tibble[i,"avg_point_allowed_vis"] <- data %>% filter(vis == x) %>% summarise(mean(score1))
    teams_tibble[i,"avg_point_allowed_tot"] <- data %>% filter(home == x | vis == x) %>% summarise(mean(ifelse(home == x, score2, score1)))
    teams_tibble[i,"avg_sc_diff_home"] <- data %>% filter(home == x) %>% summarize(mean(score1 - score2))
    teams_tibble[i,"avg_sc_diff_vis"] <- data %>% filter(vis == x) %>% summarize(mean(score2 - score1))
    teams_tibble[i,"avg_sc_diff_tot"] <- data %>% filter(home == x |vis == x) %>% summarise(mean(ifelse(home == x, score1 - score2, score2 - score1)))
    teams_tibble[i,"win_perc_home"] <- data %>% filter(home == x) %>% summarise(mean((score1 - score2) > 0))
    teams_tibble[i,"win_perc_vis"] <- data %>% filter(vis == x) %>% summarise(mean((score2 - score1) > 0))
    teams_tibble[i,"win_perc_tot"] <- record[3] / 100
    teams_tibble[i, "total_games"] <- record[2]
  }
  
  teams_tibble %>% arrange(team)
  
}
