data <- get_cbb_data()
expect_equal(
  all_teams_records(data)[1],
  tibble(team = append(data$home, data$vis)) %>% distinct()
  )
