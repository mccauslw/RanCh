test_that("RC and RP simulation works", {
  n_objects <- 5
  alpha_prior <- create_alpha_prior(n_objects, 4, 0.1)
  N <- RanCh::MMS_2019_counts[1, , ]
  u <- create_universe(n_objects, object_names=dimnames(N)[[2]])
  Nv <- vectorize(u, N)
  J <- 20
  M <- 50
  RC_sim <- run_RC_sim(u, J, M, alpha_prior, Nv)
  expect_false(anyNA(RC_sim$alpha))
  expect_false(any(is.nan(RC_sim$alpha)))

  p <- c(0.1, 0.5, 0.9)
  group_stats <- ind_groups_stats(RC_sim$alpha, J, p)
  expect_lt(group_stats$mean, 14.0)
  expect_gt(group_stats$mean, 12.0)

  lambda_values <- seq(0.01, 1.00, by=0.01)
  cycle_schedule <- create_cycle_schedule(lambda_values)
  RP_sim <- run_RP_sim(u, J, M, alpha_prior, Nv,
                       lambda_values, cycle_schedule)
  expect_false(anyNA(RP_sim$alpha))
  expect_false(any(is.nan(RP_sim$alpha)))
})

test_that("AR_gamma works (autoregressive gamma process)", {
  M <- 10
  n_rep <- 100
  alpha <- 3.0
  phi <- 0.9
  gamma <- matrix(nrow = n_rep, ncol = M)
  gamma[1,] = rgamma(M, alpha)
  for (i in seq(2, n_rep)) {
    gamma[i,] = AR_gamma(gamma[i-1,], alpha, phi)
  }
})
