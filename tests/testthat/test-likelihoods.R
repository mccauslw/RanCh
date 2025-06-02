test_that("Dirichlet alpha contruction routines work", {
  u <- create_universe(5, letters[1:5])
  N = RanCh::MMS_2019_counts[1, , ]
  Alpha1 <- DirDC_constant_sum(5, 12.0, name_source = N)
  Alpha2 <- DirDC_constant_sum(5, 12.0, name_source = u)
  # WJM: test constant shape routines here
  expect_identical(Alpha1, Alpha2)
})

test_that("Get correct value of max-min log marginal likelihood", {
  N <- RanCh::MMS_2019_counts[1, , ]
  P <- P_uniform(5)
  max_min_ln_marl <- dmultinomDC(P, N, categorical=TRUE, log=TRUE)
  expect_equal(max_min_ln_marl, -1061.11277)
})

test_that("Get correct value of P log maximum likelihood",{
  N <- RanCh::MMS_2019_counts[1, , ]
  P <- proportions(N)
  P_ln_maxl <- dmultinomDC(P, N, categorical=TRUE, log=TRUE)
  expect_equal(P_ln_maxl, -982.7762)
})

test_that("Get correct value of uniform P log marginal likelihood",{
  N <- RanCh::MMS_2019_counts[1, , ]
  Alpha <- DirDC_shape_constant_value(5, 1.0)
  uniform_P_ln_marl <- dDirMultinomRC(Alpha, N, categorical=TRUE, log=TRUE)
  expect_equal(uniform_P_ln_marl, -1055.28457)
})
