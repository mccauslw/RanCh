test_that("copy_A_x_names works", {
  # Two different times of objects that can give names
  u <- create_universe(5, letters[1:5])
  N = RanCh::MMS_2019_counts[1, , ]
  names1 <- copy_A_x_names(u, 31, 5)
  names2 <- copy_A_x_names(N, 31, 5)
  expect_equal(names1, names2)
  expect_equal(names1[[2]], letters[1:5])
})
