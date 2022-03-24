data <- get_cbb_data()

expect_equal(
  year_filter(data, 2021),
  filter(data, year(date) == 2021)
)

expect_equal(
  year_filter(data),
  filter(data, year(date) == 2022)
)