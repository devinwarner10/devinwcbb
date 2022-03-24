data <- get_cbb_data()
expect_equal(
  team_filter(data, "Southern Utah"), 
  filter(data, home == "Southern Utah" | vis == "Southern Utah")
)

expect_equal(
  team_filter(data, "Northern Arizona"), 
  filter(data, home == "Northern Arizona" | vis == "Northern Arizona")
)

expect_equal(
  team_filter(data, "BYU"), 
  filter(data, home == "BYU" | vis == "BYU")
)