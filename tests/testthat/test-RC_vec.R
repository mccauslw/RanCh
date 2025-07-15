test_that("Vectorization and unvectorization work", {

  # Example: vectorize counts
  N <- MMS_2019_counts[1,,]
  u <- create_universe(5, colnames(N))
  Nv <- vectorize(u, N)
  expect_equal(unvectorize(u, Nv, 0), N)

  # Example: vectorize single RC model
  u3 <- create_universe(3, letters[1:3])
  P <- P_Luce(c(a=1, b=2, c=3))
  Pv <- vectorize(u3, P)
  expect_equal(unvectorize(u3, Pv, 1), P)

  # Example: vectorize multiple RC models
  Alpha <- DirRC_constant_shape(3, 1.0, name_source = u3)
  P <- rDirichletRC(10, Alpha)
  Pv <- vectorize(u3, P)
  expect_equal(unvectorize(u3, Pv, 1), P)
})
