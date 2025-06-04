library(tidyverse)
library(extraDistr)

n_experiments <- dim(MMS_2019_counts)[1]

pi_maxl_set = seq(n_experiments)
ind_sim_set = seq(n_experiments)
rp_sim_set = seq(n_experiments)

# Look for list sim in inst/cache. If not there, create a new list.
cache_dir <- system.file("cache", package = "RanCh")
sim_cache_file <- file.path(cache_dir, "sim.rds")
if (!file.exists(sim_cache_file)) {
  sim <- vector('list', n_experiments)
  for (i in seq(n_experiments))
    sim[[i]] = list(id = i)
}

# Compute usefull information about universes of size 5
u <- create_universe(5, dimnames(MMS_2019_counts)[[3]])

# Create matrix with scalar information for runs
cnames <- c('max_min_ln_marl',
            'uniform_P_ln_marl',
            'P_ln_maxl',
            'pi_ln_maxl')

# SMC paramters
J <- 20    # Number of particle groups
M <- 800   # Number of particles per group

# Set up schedule of parameters for C-S-M SMC cycles
####################################################

# Grid of lambda values at which to approximate marginal likelihod
lambda_values <- seq(0.01, 1.00, by=0.01)
cycle_schedule <- create_cycle_schedule(lambda_values)

# Grids for function evaluation
p_grid <- seq(0, 1, by=1/80)
n_alpha_grid <- 40

# Group quantiles to compute
quant_p <- c(0.025, 0.05, 0.5, 0.95, 0.975)

#Rprof('profile')

# Hyperparameters defining gamma prior on scalar alpha
alpha_prior <- create_alpha_prior(u$n, 2.5, 0.08, 0.05, 1e-7)

# Loop over all data sets
for (i in seq(n_experiments)) {

  # Indicate progress through computations
  print(i)

  # Fill in one data set
  N <- MMS_2019_counts[i, , ]
  sim[[i]]$N <- N
  Nv <- vectorize(u, N)

  # Computations associated with prior
  sim[[i]]$alpha_prior = alpha_prior

  # Very fast computations
  P <- P_uniform(u$n)
  sim[[i]]$max_min_ln_marl <- dmultinomRC(P, N, categorical=TRUE, log=TRUE)
  Alpha <- DirRC_constant_shape(u$n, 1.0)
  sim[[i]]$uniform_P_ln_marl <- dDirMultinomRC(Alpha, N, categorical=TRUE, log=TRUE)
  P <- proportions(N)
  sim[[i]]$P_maxl <- dmultinomRC(P, N, categorical=TRUE, log=TRUE)

  if (i %in% pi_maxl_set) {
    sim[[i]]$pi_maxl_time <- system.time(
      sim[[i]]$pi_maxl <- compute_pi_ln_maxl(u, Nv))
  }

  if (i %in% ind_sim_set) {
    sim[[i]]$ind_time <- system.time(
      ind_sim <- run_RC_sim(u, J, M, alpha_prior, Nv))
    sim[[i]]$ind_hyper <- c(alpha_prior$a, alpha_prior$b)
    sim[[i]]$ind_J <- J
    sim[[i]]$ind_M <- M
    sim[[i]]$n_plus <- ind_sim$n_plus
    sim[[i]]$theta <- ind_sim$theta
    sim[[i]]$ind_marl_stats <- ind_sim$marl_stats
    sim[[i]]$ind_binp_funcs <-
      compute_RC_binp_funcs(u, ind_sim$alpha, J, Nv, p_grid)
    sim[[i]]$ind_alpha_funcs <-
      compute_pdf_cdf_on_grid(ind_sim$alpha, J, n_alpha_grid)
    sim[[i]]$ind_alpha_stats <- ind_groups_stats(ind_sim$alpha, J, quant_p)
    sim[[i]]$ind_alpha2_stats <- ind_groups_stats(ind_sim$alpha^2, J, quant_p)
  }

  if (i %in% rp_sim_set) {
    sim[[i]]$rp_time <- system.time(
      rp_sim <- run_RP_sim(u, J, M, alpha_prior, Nv, lambda_values,
                           cycle_schedule))
    sim[[i]]$rp_hyper <- c(alpha_prior$a, alpha_prior$b)
    sim[[i]]$rp_J <- J
    sim[[i]]$rp_M <- M
    sim[[i]]$rp_n_cycles <- nrow(cycle_schedule)
    sim[[i]]$rp_lambdas <- lambda_values
    sim[[i]]$cycle_schedule <- cycle_schedule
    sim[[i]]$rp_lambda_stats <- rp_sim$lambda_stats
    sim[[i]]$rp_cycle_stats <- rp_sim$cycle_stats
    sim[[i]]$rp_aPr <- list(big = rp_sim$big_aPr, sm = rp_sim$sm_aPr)
    sim[[i]]$rp_alpha_aPr <- rp_sim$alpha_aPr
    sim[[i]]$rp_alpha_mu <- rp_sim$alpha_mu
    sim[[i]]$rp_binp_funcs <-
      compute_RP_binp_funcs(u, rp_sim$gamma, J, Nv, p_grid)
    sim[[i]]$rp_alpha_funcs <-
      compute_pdf_cdf_on_grid(rp_sim$alpha, J, n_alpha_grid)
    sim[[i]]$rp_alpha_stats <- ind_groups_stats(rp_sim$alpha, J, quant_p)
    sim[[i]]$rp_alpha2_stats <- ind_groups_stats(rp_sim$alpha^2, J, quant_p)
    pi <- scale(rp_sim$gamma, center = F, scale = colSums(rp_sim$gamma))
    sim[[i]]$rp_pi_mean <- rowMeans(pi)
    sim[[i]]$rp_pi_pit <- pi %*% t(pi)
    sim[[i]]$rp_pi_cor <- cor(t(pi))
    sim[[i]]$rp_pi_cov <- var(t(pi))
    sim[[i]]$rp_pi_thin <- pi[,seq(10, M, by=10)]
  }
}

#Rprof(NULL)

saveRDS(sim, sim_cache_file)
