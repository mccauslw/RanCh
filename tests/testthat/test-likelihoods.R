test_that("Dirichlet alpha contruction routines work", {
  u <- create_universe(5, letters[1:5])
  N = RanCh::MMS_2019_counts[1, , ]

  # Constant sum routine
  Alpha1 <- DirRC_constant_sum(5, 12.0, name_source = N)
  Alpha2 <- DirRC_constant_sum(5, 12.0, name_source = u)
  expect_identical(Alpha1, Alpha2)
  expect_equal(Alpha1["abcde", "a"], 12/5)
  expect_true(is.na(Alpha1["abcd", "e"]))

  # Constant shape routine
  Alpha1 <- DirRC_constant_shape(5, 1.0, name_source = N)
  Alpha2 <- DirRC_constant_shape(5, 1.0, name_source = u)
  expect_identical(Alpha1, Alpha2)
  expect_equal(Alpha1["abcde", "a"], 1.0)
  expect_true(is.na(Alpha1["abcd", "e"]))
})

test_that("Simple P construction routines work", {
  # P_uniform
  P <- P_uniform(3)
  expect_equal(P[3,1], 1/2)
  expect_equal(P[7,1], 1/3)

  # P_Luce
  P_noname <- P_Luce(c(1,2,3))
  P <- P_Luce(c(a=1, b=2, c=3))
  expect_equal(P_noname, P, ignore_attr = TRUE)
  expect_equal(P["abc", "a"], 1/6)

  # P_logit
  P_noname <- P_logit(c(-2,0,1))
  P <- P_logit(c(a=-2, b=0, c=1))
  expect_equal(P_noname, P, ignore_attr = TRUE)
  expect_equal(P["abc", "a"], exp(-2)/(1+exp(-2)+exp(1)))

  # P_frequencies
  P <- P_frequencies(MMS_2019_counts[1,,])
  expect_equal(P["abcde", "a"], 0.25)
  expect_equal(P["abcde", "a"], P[31, 1])
  expect_true(is.na(P["abcd", "e"]))
})

test_that("Values of marginal and maximum likelihoods are correct", {
  N <- RanCh::MMS_2019_counts[1, , ]
  P <- P_uniform(5)

  # max-min log marginal likelihood
  max_min_ln_marl <- dmultinomRC(P, N, categorical=TRUE, log=TRUE)
  expect_equal(max_min_ln_marl, -1060.41961795424)

  # P log maximum likelihod
  P <- P_frequencies(N)
  P_ln_maxl <- dmultinomRC(P, N, categorical=TRUE, log=TRUE)
  expect_equal(P_ln_maxl, -943.728101159082)

  # Uniform P log marginal likelihood
  Alpha <- DirRC_constant_shape(5, 1.0)
  uniform_P_ln_marl <- dDirMultinomRC(Alpha, N, categorical=TRUE, log=TRUE)
  expect_equal(uniform_P_ln_marl, -1017.67826542784)
})

test_that("{r,d}multinomRC work", {
  v <- c(a=1, b=2, c=3)
  n_objects <- length(v)
  n_subsets <- 2^n_objects-1
  n_trials <- 2
  P <- P_Luce(v)
  xy <- set_index(c(1, 2))      # Menu with x and y
  xyz <- set_index(c(1, 2, 3))  # Menu with x, y, and z
  N_total <- vector(mode="integer", length=n_subsets)
  N_total[xy] <- 10    # Number of trials for menu with x and y
  N_total[xyz] <- 12   # Number of trials for menu with x, y, and z
  N <- rmultinomRC(n_trials, P, N_total)
  expect_equal(N[xy, 1, 1] + N[xy, 2, 1], N_total[xy])
  expect_equal(N[xyz, 1, 1] + N[xyz, 2, 1] + N[xyz, 3, 1], N_total[xyz])
  lnf1 <- dmultinomRC(P, N[, , 1], categorical=TRUE, log=TRUE)
  lnf2 <- dmultinomRC(P, N[, , 2], categorical=TRUE, log=TRUE)
  expect_equal(dmultinomRC(P, N, categorical = TRUE, log=TRUE), lnf1 + lnf2)
  expect_equal(dmultinomRC(P, N[, , 1], categorical=TRUE, log=FALSE), exp(lnf1))
})

test_that("{r,d}DirichletRC work", {
  # Note that for constant shape of 1, P is uniform
  Alpha <- DirRC_constant_shape(3, 1.0)  # 3 objects in universe
  n_draws <- 5
  P <- rDirichletRC(n_draws, Alpha)            # 5 draws
  ln_L <- dDirichletRC(Alpha, P, log=TRUE)
  expect_length(ln_L, n_draws)
  expect_true(all(ln_L == ln_L[1])) # Relies on P uniform
})

test_that("{r,d}DirMultinomRC work", {
  v <- c(a=1, b=2, c=3)
  n_objects <- length(v)
  n_subsets <- 2^n_objects-1
  n_trials <- 2
  Alpha <- DirRC_constant_shape(n_objects, 1.0)
  xy <- set_index(c(1, 2))      # Menu with x and y
  xyz <- set_index(c(1, 2, 3))  # Menu with x, y, and z
  N_total <- vector(mode="integer", length=n_subsets)
  N_total[xy] <- 10    # Number of trials for menu with x and y
  N_total[xyz] <- 12   # Number of trials for menu with x, y, and z
  N <- rDirMultinomRC(n_trials, Alpha, N_total)
  expect_equal(N[xy, 1, 1] + N[xy, 2, 1], N_total[xy])
  expect_equal(N[xyz, 1, 1] + N[xyz, 2, 1] + N[xyz, 3, 1], N_total[xyz])
  lnf1 <- dDirMultinomRC(Alpha, N[, , 1], categorical=TRUE, log=TRUE)
  lnf2 <- dDirMultinomRC(Alpha, N[, , 2], categorical=TRUE, log=TRUE)
  expect_equal(dDirMultinomRC(Alpha, N, categorical = TRUE, log=TRUE), lnf1 + lnf2)
  expect_equal(dDirMultinomRC(Alpha, N[, , 1], categorical=TRUE, log=FALSE), exp(lnf1))
})


