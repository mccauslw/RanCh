test_that("RC and RP simulation works", {
  n_objects <- 5
  alpha_prior <- create_alpha_prior(n_objects, 4, 0.1)
  N <- RanCh::MMS_2019_counts[1, , ]
  u <- create_universe(n_objects, object_names=dimnames(N)[2])
  Nflat <- vectorize_counts(u, N)
  J <- 20
  M <- 50
  RC_sim <- run_RC_sim(u, J, M, alpha_prior, Nflat)
  lambda_values <- seq(0.01, 1.00, by=0.01)
  cycle_schedule <- create_cycle_schedule(lambda_values)
  RP_sim <- run_RP_sim(u, J, M, alpha_prior, Nflat,
                       lambda_values, cycle_schedule)
})
