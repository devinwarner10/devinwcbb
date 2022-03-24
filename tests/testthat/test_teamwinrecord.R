data <- get_cbb_data()

expect_length(
  team_win_record(data,"Southern Utah"),
  3
)