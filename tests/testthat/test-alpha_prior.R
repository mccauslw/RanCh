test_that("create_alpha_prior and compute_proposal_params work", {
  # Typical case, mean of alpha is 40
  alpha_prior <- create_alpha_prior(5, 4, 0.1)
  expect_lt(alpha_prior$mode_error, 1e-3)

  # Low mean case, mean of alpha is 4
  alpha_prior <- create_alpha_prior(5, 4, 1)
  expect_lt(alpha_prior$mode_error, 1e-3)

  n <- 5
  u <- create_universe(n)
  alpha_prior <- create_alpha_prior(n, 4, 0.1)
  Nv <- vectorize(u, RanCh::MMS_2019_counts[1, , ])
  theta <- compute_proposal_params(u, alpha_prior, Nv)
  expect_lt(abs(theta[1] - 38), 1)
  expect_lt(abs(theta[2] - 33), 1)
  expect_lt(abs(theta[3] - 10), 1)
})
