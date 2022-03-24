for(i in 2:8)
expect_equal(
  get_cbb_data()[2],
  (read_fwf(file = url("http://kenpom.com/cbbga22.txt"), 
           col_positions = fwf_widths(c(11, 23, 4, 23, 4, 3, 22), 
           c("date", "home", "score1", "vis", "score2", "ufo1", "ufo2")), 
           col_types = "ccicicc") %>%
    mutate(location = home) %>%
    mutate(score_diff = score1 - score2) %>%
    select(-c(6, 7)))[2]
    )