#' Use SMC to simulate the posterior distribution alpha|Nv of the Dirichlet RC model
#'
#' Use SMC to simulate the target distribution alpha|Nv, the posterior distribution
#' alpha|Nv of the scalar parameter alpha of the Dirichlet RC model.
#'
#' @inheritParams compute_pi_ln_like
#' @param J number of independent groups of particles
#' @param M number of particles per group
#' @param alpha_prior list with information on the prior distribution of alpha,
#'                    created using [create_alpha_prior()]
#'
#' @return A list with the following elements
#' \describe{
#'    \item{alpha}{An M x J matrix, a sample from the target distribution with
#'    independent columns}
#'    \item{marl_stats}{Simulation statistics for the marginal likelihood}
#'    \item{n_plus}{Number of non-zero counts in data}
#'    \item{theta}{Parameter vector of the Beta-Gamma importance distribution}
#' }
#' @export
#'
#' @importFrom extraDistr rbetapr dbetapr
#'
#' @examples
#' n <- 5 # Number of objects in choice universe
#' alpha_prior <- create_alpha_prior(n, 4, 0.1)
#' N <- RanCh::MMS_2019_counts[1, , ]
#' u <- create_universe(n, object_names=dimnames(N)[[2]])
#' Nv <- vectorize(u, N)
#' J <- 20
#' M <- 50
#' RC_sim <- run_RC_sim(u, J, M, alpha_prior, Nv)
#'
#' @inherit create_universe author references
#'
run_RC_sim <- function(u, J, M, alpha_prior, Nv) {

  # Compute alpha proposal distribution based on f(alpha) * Pr[Nv|alpha,lambda=0]
  params <- compute_proposal_params(u, alpha_prior, Nv)

  # Draw alpha from gamma proposal distribution
  alpha <- extraDistr::rbetapr(M*J, params[1], params[2], params[3])
  alpha_Ax <- compute_alpha_Ax(u, alpha)
  ln_Pr_RC_by_A <- compute_ln_Pr_by_A(u, 'RC', alpha_Ax, Nv)

  # Compute IS (importance sampling) log weights w
  ln_num <- stats::dgamma(alpha, alpha_prior$a, alpha_prior$b, log = TRUE)
  ln_num <- ln_num + compute_ln_like(u, 0.0, ln_Pr_RC_by_A, NULL)
  ln_den <- extraDistr::dbetapr(alpha, params[1], params[2], params[3], log = TRUE)
  w <- ln_num - ln_den # log weights

  # Subtract an offset to the log weights to avoid underflow, and incorporate
  # offset into the cumulative log marginal likelihood
  ln_marl_offset <- max(w) # Compute an offset to avoid underflow
  gr_cum_ln_marl <- rep(ln_marl_offset, J)
  w <- w - ln_marl_offset
  W <- exp(w)

  # Compute IS marginal likelihood for lambda = 0, taking into account offset
  C_stage_stats <- weights_to_C_stage_stats(W, ln_marl_offset, 0, gr_cum_ln_marl)

  # Initial selection step to get an unweighted sample
  dim(W) <- c(M, J)
  for (j in 1:J) {
    probabilities <- W[,j] / sum(W[,j])
    selection <- sample.int(M, M, replace = TRUE, prob = probabilities)
    alpha[(1+(j-1)*M):(j*M)] <- alpha[(selection+(j-1)*M)]
  }

  n_plus = sum(Nv > 0)
  list(alpha=alpha, marl_stats = C_stage_stats, n_plus = n_plus, theta = params)
}

#' Create a schedule of simulation parameter values for random preference simulation
#'
#' @param lambda_values Grid of lambda values at which to approximate marginal likelihood
#'
#' @returns tibble with simulation parameter values for each SCM cycle
#' @export
#'
#' @examples
#' lambda_values <- seq(0.01, 1.00, by=0.01)
#' cycle_schedule <- create_cycle_schedule(lambda_values)
create_cycle_schedule <- function(lambda_values) {

  # Indices of lambda values at which to transition from C stage to S-M stages
  lambda_break_indices <-
    c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 18, 21, 25, 30, 35, 40, 45, 50, 55, 60,
      65, 70, 75, 80, 85, 90, 95, 99, 100)
  n_cycles <- length(lambda_break_indices)

  # Number of repetitions of M step to perform, by cycle index
  n_big_sweeps <- c(rep(3, 10), rep(1, n_cycles-10))
  phi_big_sweeps <- 1 - exp(-3*seq_len(n_cycles)/n_cycles)
  n_sm_sweeps <- c(rep(1, 10), rep(4, 10), rep(6, n_cycles-20))
  phi_sm_sweeps <- 1 - exp(-1*seq_len(n_cycles)/n_cycles)

  # Table of parameter values for each cycle
  cycle_schedule <-
    tibble::tibble(lambda_break_indices = lambda_break_indices,
                   lambda_breaks = lambda_values[lambda_break_indices],
                   n_big_sweeps = n_big_sweeps,
                   phi_big_sweeps = phi_big_sweeps,
                   n_sm_sweeps = n_sm_sweeps,
                   phi_sm_sweeps = phi_sm_sweeps)
}

#' Use SMC to simulate posterior distributions of Dirichlet RC and hybrid models
#'
#' @inheritParams run_RC_sim
#' @inheritParams create_cycle_schedule
#' @param cycle_schedule Information organising simulation by cycles
#'
#' @return List with the following elements
#' \describe{
#'    \item{alpha}{An M x J matrix with a sample of alpha from the final target distribution}
#'    \item{gamma}{An \eqn{n! \times MJ} matrix of unnormalized preference weights.}
#'    \item{lambda_stats}{A list with the following components:
#'      \itemize{
#'        \item \code{gr_ESS}
#'        \item \code{gr_marl}
#'        \item \code{gr_cum_ln_marl}
#'        \item \code{aggregates}
#'      }
#'    }
#'    \item{cycle_stats}{A data frame giving simulation results for each SMC
#'    C-S-M cycle. The named columns are:
#'      \itemize{
#'        \item \code{lambda} value of lambda at the end of the cycle
#'        \item \code{ESS} Effective Sample Size
#'        \item \code{W_var}
#'        \item \code{marl} Multiplicative contribution to marginal likelihood
#'        \item \code{marl_nse} Numerical standard error of \code{marl}
#'        \item \code{ln_marl} Additive contribution to log marginal likelihood
#'        \item \code{ln_marl_nse} Numerical standard error of \code{ln_marl}
#'        \item \code{cum_lin_marl} Cumulative log marginal likelihood at end of cycle
#'        \item \code{cum_lin_marl_nse2} Numerical standard error of \code{cum_lin_marl}
#'      }
#'    }
#'    \item{big_aPr}{A matrix of acceptance probabilities for big blocks, by cycle and block}
#'    \item{sm_aPr}{A matrix of acceptance probabilities for small blocks, by cycle and block}
#'    \item{alpha_aPr}{A vector of acceptance probabilities for alpha proposals, by cycle}
#'    \item{alpha_mu}{Posterior mean of alpha parameter, by cycle}
#' }
#'
#' @export
#'
#' @importFrom stats rgamma runif
#'
#' @examples
#' n <- 5 # Number of objects in the relevant choice universe
#' alpha_prior <- create_alpha_prior(n, 4, 0.1)
#' N <- RanCh::MMS_2019_counts[1, , ]
#' u <- create_universe(n, object_names=dimnames(N)[[2]])
#' Nv <- vectorize(u, N)
#' J <- 20
#' M <- 50
#' lambda_values <- seq(0.01, 1.00, by=0.01)
#' cycle_schedule <- create_cycle_schedule(lambda_values)
#' RP_sim <- run_RP_sim(u, J, M, alpha_prior, Nv, lambda_values, cycle_schedule)
#'
#' @inherit create_universe author references
#'
run_RP_sim <- function(u, J, M, alpha_prior, Nv, lambda_values, cycle_schedule) {

  lambda_aggregate_names =
    c('ESS', 'W_var', 'marl', 'marl_nse', 'marl_rne',
      'ln_marl', 'ln_marl_nse', 'cum_ln_marl', 'cum_ln_marl_nse2')
  n_lambda_values = length(lambda_values)
  n_cycles = nrow(cycle_schedule)

  # Table to hold marginal likelihood statistics
  lambda_stats <- list(
    gr_ESS = matrix(nrow = J, ncol = n_lambda_values),
    gr_marl = matrix(nrow = J, ncol = n_lambda_values),
    gr_cum_ln_marl = matrix(nrow = J, ncol = n_lambda_values),
    aggregates =
      matrix(nrow = n_lambda_values, ncol = 1 + length(lambda_aggregate_names),
             dimnames = list(NULL, c('lambda', lambda_aggregate_names)))
  )
  lambda_stats$aggregates[, 'lambda'] <- lambda_values

  # Table for cycle parameters pertaining to gamma update
  n_bl <- c(u$n, u$n*(u$n-1)); bl_len <- c(factorial(u$n-1), factorial(u$n-2))
  bl_data <- list(big = list(), sm = list())
  for (blt in 1:length(n_bl)) {
    bl_data[[blt]]$n_bl = n_bl[blt]
    bl_data[[blt]]$bl_len = bl_len[blt]
    bl_data[[blt]]$indices <- seq_len(n_bl[blt])
    bl_data[[blt]]$start <- (seq_len(n_bl[blt]) - 1) * bl_len[blt] + 1
    bl_data[[blt]]$end <- seq_len(n_bl[blt]) * bl_len[blt]
    bl_data[[blt]]$aPr <- matrix(NA, nrow = n_cycles, ncol = n_bl[blt])
  }

  # alpha information by cycle
  alpha_mu <- rep(NA, n_cycles)
  alpha_aPr <- rep(NA, n_cycles)

  # Start with simulation based on lambda = 0
  RC_sim <- run_RC_sim(u, J, M, alpha_prior, Nv)
  alpha <- RC_sim$alpha
  gr_cum_ln_marl <- RC_sim$marl_stats$gr_cum_ln_marl
  cum_ln_marl <- RC_sim$marl_stats$cum_ln_marl
  cum_ln_marl_nse2 <- RC_sim$marl_stats$cum_ln_marl_nse2

  # Compute quantities depending on alpha
  alpha_p <- outer(rep.int(1/u$n_orders, u$n_orders), alpha)
  rownames(alpha_p) <- u$order_strings
  alpha_Ax <- compute_alpha_Ax(u, alpha)
  rownames(alpha_Ax) <- u$Ax_strings
  ln_Pr_RC_by_A <- compute_ln_Pr_by_A(u, 'RC', alpha_Ax, Nv)

  # Draw gamma_p from gamma_p|alpha, same as gamma_p|alpha, Nv, lambda = 0
  # Columns are iid, the rows are independent.
  # An element i,j is distributed as Gamma(alpha_i, 1), where alpha_i
  # is the i'th element of alpha_p.
  gamma_p <- matrix(stats::rgamma(u$n_orders * M*J, alpha_p),
                    nrow=u$n_orders, ncol=M*J)
  rownames(gamma_p) <- u$order_strings
  gamma_Ax <- compute_gamma_Ax(u, gamma_p)
  ln_Pr_rp_by_A <- compute_ln_Pr_by_A(u, 'RP', gamma_Ax, Nv)

  # Initialize loop over cycles
  old_ll <- compute_ln_like(u, 0.0, ln_Pr_RC_by_A, NULL)
  first_lambda_index = 1

  # Loop over S-C-M cycles
  for (cycle_index in 1:n_cycles) {

    n_sweeps <- c(cycle_schedule[[cycle_index, 'n_big_sweeps']],
                  cycle_schedule[[cycle_index, 'n_sm_sweeps']])
    phi_sweeps <- c(cycle_schedule[[cycle_index, 'phi_big_sweeps']],
                    cycle_schedule[[cycle_index, 'phi_sm_sweeps']])
    last_lambda_index <- cycle_schedule[[cycle_index, 'lambda_break_indices']]
    lambda_index_range <- seq(first_lambda_index, last_lambda_index)
    cycle_lambda_values <- lambda_values[lambda_index_range]
    n_lambda_index <- length(lambda_index_range)
    last_lambda <- cycle_lambda_values[n_lambda_index]

    # C stage: compute importance weights
    #####################################

    ll <- compute_ln_like(u, cycle_lambda_values, ln_Pr_RC_by_A, ln_Pr_rp_by_A)
    w <- ll - as.vector(old_ll)

    for (lambda_i in lambda_index_range) {
      single_lambda_stats <-
        weights_to_C_stage_stats(exp(w[, lambda_i - first_lambda_index + 1]),
                                 cum_ln_marl, cum_ln_marl_nse2, gr_cum_ln_marl)
      for (name in lambda_aggregate_names)
        lambda_stats$aggregates[lambda_i, name] <- single_lambda_stats[[name]]
      lambda_stats$gr_ESS[, lambda_i] <- single_lambda_stats$gr_ESS
      lambda_stats$gr_marl[, lambda_i] <- single_lambda_stats$gr_marl
      lambda_stats$gr_cum_ln_marl[, lambda_i] <- single_lambda_stats$gr_cum_ln_marl
    }

    # Values at last lambda value of cycle
    gr_cum_ln_marl <- single_lambda_stats$gr_cum_ln_marl
    cum_ln_marl <- single_lambda_stats$cum_ln_marl
    cum_ln_marl_nse2 <- single_lambda_stats$cum_ln_marl_nse2

    # Selection (resampling) stage
    ##############################

    # Resample alpha and gamma using final weights, group by group
    W = matrix(exp(w[, n_lambda_index]), nrow=M, ncol=J)
    for (j in 1:J) {
      # New residual sampling
      En <- W[,j] / mean(W[,j])
      indices = seq_len(M)
      sure_n = floor(En)
      resid_n = En - sure_n
      sure_M = sum(sure_n)
      resid_M = M - sure_M
      selection <- c(rep(seq_len(M), sure_n),
                     sample.int(M, resid_M, replace=TRUE, prob=resid_n))

      gamma_p[,(1+(j-1)*M):(j*M)] <- gamma_p[,(selection+(j-1)*M)]
      alpha[(1+(j-1)*M):(j*M)] <- alpha[(selection+(j-1)*M)]
    }
    alpha_p = outer(rep.int(1/u$n_orders, u$n_orders), alpha)

    # Mutation stage
    ################

    # Instead of recomputing gamma_Ax, alpha_Ax, ln_Pr_rp_by_A,
    # construct at resampling step
    gamma_Ax <- compute_gamma_Ax(u, gamma_p)
    alpha_Ax <- compute_alpha_Ax(u, alpha)
    ln_Pr_RC_by_A <- compute_ln_Pr_by_A(u, 'RC', alpha_Ax, Nv)
    ln_Pr_rp_by_A <- compute_ln_Pr_by_A(u, 'RP', gamma_Ax, Nv)
    den <- compute_ln_like(u, last_lambda, ln_Pr_RC_by_A, ln_Pr_rp_by_A)

    for (blt in 1:length(bl_data)) {
      bld <- bl_data[[blt]]
      bl_data[[blt]]$aPr[cycle_index, ] <- 0
      phi <- phi_sweeps[blt]
      for (i_sweep in 1:n_sweeps[blt]) {
        for (i_bl in 1:(bld$n_bl)) {

          # Redraw a random sample of gamma vectors
          small_gamma_indices <- seq(bld$start[i_bl], bld$end[i_bl])
          small_pi_to_P <- u$pi_to_P[, small_gamma_indices]
          small_alpha_p <- alpha_p[small_gamma_indices, ]
          small_alpha_total <- colSums(small_alpha_p)
          small_gamma_p <- gamma_p[small_gamma_indices, ]

          small_gamma_total <- colSums(small_gamma_p)
          AR_small_gamma_total <-
            AR_gamma(small_gamma_total, small_alpha_total, phi)
          small_gamma_p_star <- matrix(rgamma(bld$bl_len * M * J, small_alpha_p),
                                       nrow=bld$bl_len, ncol=M*J)
          small_gamma_total_star <-
            pmax(colSums(small_gamma_p_star), .Machine$double.xmin)
          scale_value <- small_gamma_total_star/AR_small_gamma_total
          small_gamma_p_star <-
            scale(small_gamma_p_star, center=F, scale=scale_value)
          small_gamma_p_star <- pmax(small_gamma_p_star, .Machine$double.xmin)

          # Recompute likelihood values for proposal, accept or reject
          gamma_Ax_diff <- small_pi_to_P %*% (small_gamma_p_star-small_gamma_p)
          gamma_Ax_star <- pmax(gamma_Ax + gamma_Ax_diff, .Machine$double.xmin)
          ln_Pr_rp_by_A_star <- compute_ln_Pr_by_A(u, 'RP', gamma_Ax_star, Nv)
          num <- compute_ln_like(u, last_lambda, ln_Pr_RC_by_A,
                                 ln_Pr_rp_by_A_star)
          H <- pmin(1, exp(num-den))
          accept <- (runif(M * J) < H)
          gamma_p[small_gamma_indices, accept] <- small_gamma_p_star[, accept]
          gamma_Ax <- compute_gamma_Ax(u, gamma_p)
          den[accept] <- num[accept]
          bl_data[[blt]]$aPr[cycle_index, i_bl] <-
            bl_data[[blt]]$aPr[cycle_index, i_bl] + mean(H)
        }
      }
      bl_data[[blt]]$aPr[cycle_index, ] <-
        bl_data[[blt]]$aPr[cycle_index, ] / n_sweeps[blt]
    }
    ln_Pr_rp_by_A <- compute_ln_Pr_by_A(u, 'RP', gamma_Ax, Nv)

    # Update alpha, update likelihood for next cycle
    res <- update_alpha(u, Nv, alpha_prior, alpha, gamma_p, last_lambda, ln_Pr_rp_by_A)
    alpha <- res$alpha
    gamma_p <- res$gamma_p
    alpha_aPr[cycle_index] = res$aPr
    alpha_mu[cycle_index] = res$mu
    alpha_Ax <- compute_alpha_Ax(u, alpha)
    alpha_p <- outer(rep.int(1/u$n_orders, u$n_orders), alpha)
    ln_Pr_RC_by_A <- compute_ln_Pr_by_A(u, 'RC', alpha_Ax, Nv)
    old_ll <- compute_ln_like(u, last_lambda, ln_Pr_RC_by_A, ln_Pr_rp_by_A)

    first_lambda_index = last_lambda_index + 1
  }

  list(alpha = alpha, gamma = gamma_p,
       lambda_stats = lambda_stats,
       cycle_stats = lambda_stats$aggregates[cycle_schedule$lambda_break_indices,],
       big_aPr = bl_data[[1]]$aPr, sm_aPr = bl_data[[2]]$aPr,
       alpha_aPr = alpha_aPr, alpha_mu = alpha_mu)
}

#' Compute sample statistics and standard errors for a collection of SMC samples
#'
#' Given a collection of J independent Sequential Monte Carlo (SMC) samples,
#' compute mean, standard deviation, numerical standard error for the mean,
#' relative numerical efficiency for the mean, specified quantiles and their
#' numerical standard errors.
#'
#' @param x vector of length M * J, with J mutually independent subvectors of length
#'  M, each subvector arising from a SMC simulation
#' @inheritParams run_RC_sim
#' @param p vector of probabilities for which to compute quantiles
#'
#' @return A list with the elements
#' \describe{
#'   \item{mu}{sample mean}
#'   \item{std}{sample standard deviation (measure of posterior uncertainty)}
#'   \item{nse}{numerical standard error for the sample mean (measure of
#'   simulation noise)}
#'   \item{rne}{relative numerical efficiency for the sample mean}
#'   \item{p}{same as input of that name}
#'   \item{q}{sample quantiles corresponding to p}
#'   \item{q_nse}{numerical standard errors for the quantiles}
#' }
#' @export
#'
#' @examples
#' n <- 5
#' u <- create_universe(n)
#' alpha_prior <- create_alpha_prior(n, 4, 0.1)
#' Nv <- vectorize(u, RanCh::MMS_2019_counts[1, , ])
#' J <- 20
#' M <- 50
#' RC_sim <- run_RC_sim(u, J, M, alpha_prior, Nv)
#' ind_groups_stats(RC_sim$alpha, J, c(0.1, 0.5, 0.9))
#'
#' @inherit create_universe author references
#'
ind_groups_stats <- function(x, J, p) {
  M <- length(x) / J
  s2 <- stats::var(x); std = sqrt(s2)
  dim(x) <- c(M, J) # organze x as a M by J matrix, with J independent columns
  mu_by_group <- colMeans(x)    # J-vector of group means
  mu <- mean(mu_by_group)       # Global mean
  nse2 <- stats::var(mu_by_group)/J; nse <- sqrt(nse2)
  q_by_group <- apply(x, 2, stats::quantile, probs = p)
  q <- rowMeans(q_by_group)
  q_nse2 = apply(q_by_group, 1, stats::var)/J; q_nse <- sqrt(q_nse2)
  list(mu = mu, std = std, nse = nse, rne = (s2/(M*J))/nse2,
       p = p, q = q, q_nse = q_nse)
}

#' Compute pdf and cdf on the grid x_grid for a collection of SMC samples
#'
#' Given a collection of J independent Sequential Monte Carlo (SMC) samples,
#' compute the sample probability density function (pdf) and cumulative
#' distribution function (cdf), with numerical standard errors, at a grid
#' of points of evaluation
#'
#' @inheritParams ind_groups_stats
#' @param x_grid vector of points of evaluation
#'
#' @return A list with elements pdf and cdf, each with elements
#' \describe{
#'   \item{x}{vector of argument values}
#'   \item{func}{vector of function values}
#'   \item{nse}{vector of numerical standard errors for the function values}
#' }
#' @export
#'
#' @importFrom graphics hist
#'
#' @examples
compute_pdf_cdf_on_grid <- function(x, J, x_grid) {
  if (length(x_grid) == 1) {
    x_grid = seq(min(x), max(x), len=x_grid)
  }
  M <- length(x) / J
  Kp1 <- length(x_grid); K = Kp1 - 1
  dim(x) <- c(M, J)
  pdf_by_group <- matrix(nrow=K, ncol=J)
  cdf_by_group <- matrix(nrow=Kp1, ncol=J)
  for (j in 1:J) {
    h <- graphics::hist(x[,j], breaks = x_grid, plot=F)
    pdf_by_group[,j] <- h$density
    cdf_by_group[,j] <- c(0, cumsum(h$counts/sum(h$counts)))
  }
  pdf <- rowMeans(pdf_by_group)
  cdf <- rowMeans(cdf_by_group)
  pdf_nse <- sqrt(rowMeans((pdf_by_group-pdf)^2)/J)
  cdf_nse <- sqrt(rowMeans((cdf_by_group-cdf)^2)/J)
  list(pdf = list(x=h$mids, func=pdf, nse = pdf_nse),
       cdf = list(x=x_grid, func=cdf, nse = cdf_nse))
}

#' Compute the posterior pdf and cdf of binary choice probabilities for the
#' Dirichlet Random Preference model
#'
#' For each binary choice probability, compute its posterior pdf and cdf for the
#' Dirichlet RP model, on the grid p_grid of binary choice probabilities,
#' given a collection of SMC samples that target the posterior distribution
#' of gamma in the Dirichlet RP model
#'
#' @inheritParams ind_groups_stats
#' @inheritParams compute_pi_ln_like
#' @param gamma_p matrix, \eqn{JM \times n!}, where each column is a vector of
#' unnormalized preference probabilities
#' @param p_grid vector of choice probability values
#'
#' @return A list with elements correponding to binary subsets of choice
#' objects. For a universe of size \eqn{n=5}, there are \eqn{{n \choose 2} = 10} such
#' subsets. Each element of the list is a list organized in the same way
#' as the return value of [compute_pdf_cdf_on_grid()].
#' @export
#'
#' @examples
#'
#' @inherit create_universe author references
#'
compute_RP_binp_funcs <- function(u, gamma_p, J, Nv, p_grid) {
  M <- ncol(gamma_p) / J
  n_grid_pts = length(p_grid)
  Ax_binaries <- u$A_table[u$A_table[,'nA']==2, 'Ax']
  n_binary <- length(Ax_binaries)
  binp_funcs <- vector("list", n_binary)

  # bin_probs is a n_binary x MJ matrix of binary choice probabilities
  bin_probs <- t(t(u$pi_to_P[Ax_binaries,] %*% gamma_p)/colSums(gamma_p))
  for (i_bin in seq_len(n_binary)) {
    binp_funcs[[i_bin]] <- compute_pdf_cdf_on_grid(bin_probs[i_bin,], J, p_grid)
  }
  binp_funcs
}

#' Compute the posterior pdf and cdf of binary choice probabilities for the
#' Dirichlet Random Choice model
#'
#' For each binary choice probability, compute its posterior pdf and cdf for the
#' Dirichlet RC model, on the grid p_grid of binary choice probabilities,
#' given a collection of SMC samples that target the posterior distribution
#' of alpha in the Dirichlet RC model
#'
#' @inheritParams compute_RP_binp_funcs
#' @param alpha vector of length M*J, sample of values of alpha
#'
#' @inherit compute_RP_binp_funcs return
#' @export
#'
#' @importFrom stats dbeta pbeta
#'
#' @examples
#'
#' @inherit create_universe author references
#'
compute_RC_binp_funcs <- function(u, alpha, J, Nv, p_grid) {
  n_grid <- length(p_grid)
  M <- length(alpha) / J
  Ax_binaries <- u$A_table[u$A_table[,'nA']==2, 'Ax']
  n_binary <- length(Ax_binaries)
  n1 <- Nv[Ax_binaries]      # first object count for n_binary choice sets
  n0 <- Nv[Ax_binaries + 1]  # second object count for n_binary choice sets

  binp_funcs <- vector("list", n_binary)

  for (i_bin in seq_len(n_binary)) {
    # Matrices of Dirichlet weights, 1st and 2nd objects
    a1 <- matrix(n1[i_bin] + 0.5 * alpha, nrow = M, ncol = J)
    a2 <- matrix(n0[i_bin] + 0.5 * alpha, nrow = M, ncol = J)

    cdf <- matrix(0, nrow = n_grid, ncol = J)
    pdf <- matrix(0, nrow = n_grid, ncol = J)
    for (j in seq_len(J)) {
      for (m in seq_len(M)) {
        pdf <- pdf + stats::dbeta(p_grid, a1[m, j], a2[m, j])
        cdf <- cdf + stats::pbeta(p_grid, a1[m, j], a2[m, j])
      }
      pdf = pdf/M; cdf <- cdf/M
    }
    pdf_mean = rowMeans(pdf); cdf_mean = rowMeans(cdf)
    binp_funcs[[i_bin]] <- list(
      pdf = list(x=p_grid, func=pdf_mean, nse=rowMeans((pdf - pdf_mean)^2/J)),
      cdf = list(x=p_grid, func=cdf_mean, nse=rowMeans((cdf - cdf_mean)^2/J))
    )
  }
  binp_funcs
}

#' Compute marginal likelihood statistics for a vector of importance weights
#'
#' Use importanace weights for the current SMC cycle and the cumulative
#' marginal likelihood statistics from previous SMC cycles to compute
#' marginal likelihood statistics for the current cycle and to update the
#' cumulative marginal likelihood statistics.
#'
#' @param W vector of importance weights of length JM
#' @param cum_ln_marl cumulative log marginal likelihood at the end of the
#' previous SMC cycle
#' @param cum_ln_marl_nse2 cumulative numerical variance of the log marginal
#' likelihood at the end of the previous SMC cycle
#' @param gr_cum_ln_marl cumulative log marginal likelihood at the end of the
#' previous SJM cycle, by group
#'
#' @returns A list with components
#' \describe{
#'   \item{gr_ESS}{Effective Sample Size, by group}
#'   \item{gr_marl}{marginal likelihood factor, by group}
#'   \item{gr_cum_ln_marl}{cumulative log marginal likelihood, by group}
#'   \item{ESS}{Effective Sample Size, aggregate}
#'   \item{W_var}{}
#'   \item{marl}{marginal likelihood incremental factor}
#'   \item{marl_nse}{numerical standard error of \code{marl}}
#'   \item{marl_rne}{relative numerical efficiency of \code{marl}}
#'   \item{ln_marl}{log marginal likelihood increment}
#'   \item{ln_marl_nse}{numerical standard error of \code{ln_marl}}
#'   \item{cum_ln_marl}{cumulative log marginal likeilhood at the end of the
#'   current SMC cycle}
#'   \item{cum_ln_marl_nse2}{cumulative numerical variance of the log marginal
#'   likelihood at the end of the current SMC cycle}
#' }
#'
#' @export
#'
#' @examples
weights_to_C_stage_stats <- function(W, cum_ln_marl, cum_ln_marl_nse2,
                                     gr_cum_ln_marl) {
  J <- length(gr_cum_ln_marl)
  M <- length(W) / J

  # To compute globally, not by group
  W_var <- stats::var(W)      # Global variance of weights
  ESS <- sum(W)^2 / sum(W^2)  # Effective sample size of weights

  # Organize weights by group, compute group means
  dim(W) <- c(M, J)            # Weights organized in a matrix, by groups

  # To compute by group, j=1,...,J
  gr_ESS <- colSums(W)^2 / colSums(W^2) # ESS
  gr_marl <- colMeans(W)             # Marginal likelihood factor
  # Cumulative log marginal likelihood
  gr_cum_ln_marl <- gr_cum_ln_marl + log(gr_marl)

  # Final computations
  marl <- mean(gr_marl)              # Marginal likelihood increment
  marl_nse2 <- stats::var(gr_marl)/J # Numerical variance of mlike increment
  marl_nse <- sqrt(marl_nse2)        # Numerical standard error of mlike increment
  marl_rne <- (W_var/(M*J)) / marl_nse2  # Relative numerical efficiency of ml
  ln_marl <- log(marl)               # Log marginal likelihood increment
  ln_marl_nse <- marl_nse / marl     # Numerical standard error of log mlike increment
  list(gr_ESS = gr_ESS, gr_marl = gr_marl, gr_cum_ln_marl = gr_cum_ln_marl,
       ESS = ESS, W_var = W_var,
       marl = marl, marl_nse = marl_nse, marl_rne = marl_rne,
       ln_marl = ln_marl, ln_marl_nse = ln_marl_nse,
       cum_ln_marl = cum_ln_marl + ln_marl,
       cum_ln_marl_nse2 = cum_ln_marl_nse2 + ln_marl_nse^2)
}

#' Compute posterior expected value of RC log likelihood function for the
#' Bayesian Dirichlet RC model
#'
#' @inheritParams compute_pi_ln_like
#' @param alpha common scalar sum of Dirichlet shape parameters
#' @param M total number of draws from each Dirichlet distribution
#'
#' @returns The posterior expected value of the RC log likelihood
#' @export
#'
#' @examples
#' n <- 5
#' u <- create_universe(n)
#' alpha <- 1.0
#' Nv <- vectorize(u, RanCh::MMS_2019_counts[1, , ])
#'
compute_EP_ln_maxl <- function(u, Nv, alpha, M) {

  # Compute log multinomial density without the "n choose r" normalization
  # constant, so that it measures the probability of the sequence, rather than
  # the count.
  lnf_seq_multinom <- function(p, N_x) {
    sum(N_x[N_x != 0] * log(p[N_x != 0]))
  }

  Eln_maxl <- 0
  for (A in 1:u$n_subsets) {
    if (u$A_table[A, 'nA'] > 1) {
      Ax <- u$A_table[A, 'Ax']
      nA <- u$A_table[A, 'nA']
      N_Ax <- Nv[Ax:(Ax+nA-1)]
      al_post = N_Ax + rep(alpha/nA, nA)
      P = bayesm::rdirichlet(M, N_Ax)
      lnf = apply(P, MARGIN=1, lnf_seq_multinom, N_Ax)
      Eln_maxl <- Eln_maxl + mean(lnf)
    }
  }
  Eln_maxl
}

#' Aggregate preference gamma weights to obtain choice object gamma weights
#'
#' @param u
#' @param gamma
#'
#' @returns A matrix of gamma weights indexed by Ax and particle
#' @export
#'
#' @examples
compute_gamma_Ax <- function(u, gamma) {
  gamma_Ax <- u$pi_to_P %*% gamma
  rownames(gamma_Ax) <- u$Ax_strings
  gamma_Ax
}

#' Compute Dirichlet alpha parameters by Ax given a
#'
#' @param u
#' @param alpha vector of alpha values by particle
#'
#' @returns matrix, n_probs by MJ, of Dirichlet alpha parameters
#' @export
#'
#' @examples
compute_alpha_Ax <- function(u, alpha) {
  card_Ax <- u$A_table[u$Ax_table[, 'A'], 'nA']
  alpha_Ax <- outer(card_Ax, alpha, FUN = function(x,y){y/x})
  rownames(alpha_Ax) <- rownames(u$Ax_table)
  alpha_Ax
}

log_prior_gamma <- function(u, gamma_p, alpha_p) {
  colSums(stats::dgamma(gamma_p, alpha_p, log = TRUE))
}

#' Compute RC or RP log likelihood terms, by menu and particle
#'
#' Compute one of the following log probabilities, by choice set
#'
#'   (1) ln_Pr_RC_by_A, n_subsets x M*J
#'     - gives, for set A (row) and particle m (col), the probability of choice
#'       count vector N_A given alpha_m and lambda = 0
#'   (2) ln_Pr_RP_by_A, n_subsets x M*J
#'     - gives, for set A (row) and particle m (col), the probability of choice
#'       count vector N_A given gamma_p (col m) and lambda = 1
#'
#' @inheritParams compute_pi_ln_like
#' @param type the string "RP" or "RC", to indicate which terms to compute
#' @param weight_Ax
#'
#' @returns A matrix of RC or RP log likelihood terms, by menu and particle
#' @export
#'
#' @examples
#' n <- 5
#' u <- create_universe(n)
#' Nv <- vectorize(u, RanCh::MMS_2019_counts[1, , ])
#' M <- 10
#' weight_Ax <- matrix(rgamma(u$n_probs * M, 4.0), nrow=u$n_probs, ncol=M)
#' ln_Pr_by_A <- compute_ln_Pr_by_A(u, "RC", weight_Ax, Nv)
compute_ln_Pr_by_A <- function(u, type, weight_Ax, Nv) {
  ln_Pr_by_A <- matrix(0, nrow=u$n_subsets, ncol=ncol(weight_Ax))
  rownames(ln_Pr_by_A) <- u$A_strings

  if (type == 'RP')
    gamma_total <- weight_Ax[1,] + weight_Ax[2,]

  for (A in 1:u$n_subsets) {
    if (u$A_table[A, 'nA'] > 1) {
      Ax <- u$A_table[A, 'Ax']
      nA <- u$A_table[A, 'nA']
      N_Ax <- Nv[Ax:(Ax+nA-1)]
      N_A <- sum(N_Ax)
      if (type == 'RP') { # weight_Ax is gamma_Ax
        ga_Ax <- weight_Ax[Ax:(Ax+nA-1), ]
        ln_Pr_by_A[A, ] <- colSums(N_Ax*log(ga_Ax)) - N_A*log(gamma_total)
      }
      if (type == 'RC') { # weight_Ax is alpha_Ax
        al_Ax <- as.matrix(weight_Ax[Ax:(Ax+nA-1), ])
        ln_Pr_by_A[A, ] <-
          (lgamma(colSums(al_Ax)) - colSums(lgamma(al_Ax))
           - lgamma(colSums(al_Ax) + N_A) + colSums(lgamma(al_Ax + N_Ax)))
      }
    }
  }
  ln_Pr_by_A
}

#' Compute log likelihood values for a hybrid model indexed by \code{lambda}
#'
#' Takes log likelihood terms, by subset A, for the RC and RP models and
#' computes a weighted log likelihood for the hybrid model indexed by the
#' scalar value \code{lambda} in the unit interval @eqn{[0,1]}.
#' @param u
#' @param lambda index of hybrid model
#' @param ln_Pr_RC_by_A matrix of RC log likelihood terms, by menu A and particle
#' @param ln_Pr_RP_by_A matrix of RP log likelihood terms, by menu A and particle
#'
#' @returns A vector of hybrid model log likelihood values, by particle
#' @export
#'
#' @examples
compute_ln_like <- function(u, lambda, ln_Pr_RC_by_A, ln_Pr_RP_by_A) {
  # If lambda is (scalar) zero, ln_Pr_RP_by_A is not referenced
  if (identical(lambda, 0.00)) {
    ln_like <- colSums(ln_Pr_RC_by_A)
  } else {
    n_lambda <- length(lambda)
    ln_like <- matrix(0, nrow=ncol(ln_Pr_RP_by_A), ncol=n_lambda)
    for (i in 1:n_lambda) {
      ln_like[, i] <-
        colSums(log(
          lambda[i] * exp(ln_Pr_RP_by_A) + (1-lambda[i]) * exp(ln_Pr_RC_by_A)
        ))
    }
  }
  ln_like
}

#' Apply a Markov transition to each element of gamma. The stationary
#' distribution of each element gamma[i] is Gamma(alpha[i], 1)
#'
#' @param gamma A vector of random variables
#' @param alpha A vector of shape parameters for the Gamma distribution or a
#' common scalar value
#' @param phi A common autocorrelation parameter, -1 < phi < 1
#'
#' @returns A vector of random variables, the result of the Markov transitions
#' @export
#'
#' @examples
AR_gamma <- function(gamma, alpha, phi) {
  M <- length(gamma)
  be <- stats::rbeta(M, phi*alpha, (1-phi)*alpha)
  gamma <- be * gamma + stats::rgamma(M, (1-phi)*alpha)
  gamma
}

#' Sample alpha from its conditional distribution given gamma and data
#'
#' @inheritParams compute_pi_ln_like
#' @inheritParams run_RC_sim
#' @param alpha current state of alpha particles
#' @param gamma_p current state of gamma particles
#' @param lambda parameter of hybrid model
#' @param ln_Pr_RP_by_A
#' @param n_reps number of times to apply Metropolis-Hastings update of alpha
#'
#' @returns
#' @export
#'
#' @examples
update_alpha <- function(u, Nv, alpha_prior, alpha, gamma_p, lambda, ln_Pr_RP_by_A,
                         n_reps = 2) {
  MJ <- length(alpha)
  # Metropolis-Hastings proposal distribution for target distribution
  # alpha|pi is based on the close approximation gamma(x) = 1/x for
  # small values of x. The proposal distribution is Ga(a_post, b_post)

  # First compute a_post, b_post and draw alpha_star
  g_bar <- pmax(colMeans(log(gamma_p)), log(.Machine$double.xmin))
  G0 <- colSums(gamma_p)
  p_bar <- g_bar - log(G0)
  n_fact = u$n_orders

  alpha_mode <- alpha_prior$alpha_mode[(p_bar - alpha_prior$p_min)/alpha_prior$h]
  a_delta <- -3
  a_bar <- alpha_prior$a + n_fact - 1 + a_delta
  b_delta <- -alpha_prior$psi_diff[(p_bar - alpha_prior$p_min)/alpha_prior$h] +
    a_delta/alpha_mode
  b_bar <- alpha_prior$b - p_bar + b_delta

  alpha_Ax <- compute_alpha_Ax(u, alpha)
  ln_Pr_RC_by_A <- compute_ln_Pr_by_A(u, 'RC', alpha_Ax, Nv)
  ln_ll <- compute_ln_like(u, lambda, ln_Pr_RC_by_A, ln_Pr_RP_by_A)
  ln_f_over_g <- lgamma(alpha + 1) -
    n_fact * lgamma(alpha/n_fact + 1) -
    a_delta * log(alpha) + b_delta * alpha

  aPr <- 0
  for (i_rep in seq_len(n_reps)) {
    # Next compute ln Pr[Nv|alpha_star, lambda = 0], ln Pr[Nv|alpha, lambda = 0]
    alpha_star <- stats::rgamma(MJ, a_bar, b_bar)
    alpha_Ax_star <- compute_alpha_Ax(u, alpha_star)
    ln_Pr_RC_by_A_star <- compute_ln_Pr_by_A(u, 'RC', alpha_Ax_star, Nv)
    ln_ll_star <- compute_ln_like(u, lambda, ln_Pr_RC_by_A_star, ln_Pr_RP_by_A)

    # Next evaluate proposal density g and target density f at alpha and alpha_star
    ln_f_over_g_star <- lgamma(alpha_star + 1) -
      n_fact * lgamma(alpha_star/n_fact + 1) -
      a_delta * log(alpha_star) + b_delta * alpha_star

    # Finally, evaluate log Hasting ratio, accept proposals with probability
    # min(1, H) and return updated alpha.
    ln_H <- ln_ll_star - ln_ll + ln_f_over_g_star - ln_f_over_g
    H = pmin(1, exp(ln_H))
    accept <- stats::runif(MJ) <= H
    aPr <- aPr + mean(H)
    alpha[accept] <- alpha_star[accept]
    ln_f_over_g[accept] <- ln_f_over_g_star[accept]
    ln_ll[accept] <- ln_ll_star[accept]
  }
  G <- stats::rgamma(MJ, alpha)
  gamma_p <- scale(gamma_p, center=F, scale=G0/G)
  aPr <- aPr / n_reps
  list(alpha = alpha, gamma_p = gamma_p, aPr = aPr, mu = mean(alpha))
}
