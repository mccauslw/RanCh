test_that("compute_pi_ln_like works", {
  n <- 5
  n_fact = factorial(n)
  pi <- rep(1/n_fact, n_fact) # Uniform distribution over preference orders
  u <- create_universe(n)
  Nv <- vectorize(u, RanCh::MMS_2019_counts[1, , ])
  pi_ln_like <- compute_pi_ln_like(pi, u, Nv)
  expect_equal(pi_ln_like, -1060.41961795424)
})

test_that("compute_pi_score works", {
  n <- 5
  n_fact = factorial(n)
  pi <- rep(1/n_fact, n_fact) # Uniform distribution over preference orders
  u <- create_universe(n)
  Nv <- vectorize(u, RanCh::MMS_2019_counts[1, , ])
  pi_score <- compute_pi_score(pi, u, Nv)
  expect_length(pi_score, n_fact)
  expect_equal(pi_score[[1]], 1305)
})

test_that("compute_pi_ln_maxl works", {
  n <- 5
  u <- create_universe(n)
  Nv <- vectorize(u, RanCh::MMS_2019_counts[1, , ])
  pi_ln_maxl <- compute_pi_ln_maxl(u, Nv)
  expect_equal(pi_ln_maxl$ln_maxl, -948.558892391828)
  expect_lt(pi_ln_maxl$ln_maxl_diff, 1e-8)
})
