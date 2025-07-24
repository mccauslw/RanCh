#' Set the prior distribution of alpha
#'
#' Set the truncated Gamma prior distribution for the parameter alpha, and
#' precompute information about it, including lookup tables used for
#' Metropolis-Hastings updates of the conditional posterior distribution of
#' alpha given pi, described in Appendix B of the reference below.
#'
#' @param n number of elements in choice universe
#' @param a,b shape and rate parameters of a Gamma distribution, the
#' pre-truncation prior distribution of alpha.
#' @param alpha_min truncation parameter. The prior distribution of alpha is
#' truncated to the region /[alpha_min, infinity).
#' @param n_grid number of grid points in a grid of values of p_bar, defined in
#' Appendix B of the reference below
#'
#' @return A list with the following elements
#'\describe{
#'   \item{a,b,alpha_min}{same as inputs with those names}
#'   \item{p_grid}{grid of values of p_bar}
#'   \item{p_bar_min}{minimum value of `p_bar` in `p_grid`}
#'   \item{h}{h distance between grid points on `p_grid`}
#'   \item{alpha_mode}{grid of values of mode of alpha|pi as function of p_bar}
#'   \item{mode_error}{maximum value of error of mode on grid}
#'   \item{psi_diff}{grid of values of psi(1+alpha_mode) - psi(1+alpha_mode/n!)}
#'   \item{funcs}{grids of values of alpha, prior pdf and cdf}
#' }
#' @export
#'
#' @importFrom stats pgamma qgamma dgamma uniroot
#'
#' @examples
#' n <- 5
#' a <- 4
#' b <- 0.1
#' alpha_prior <- create_alpha_prior(n, a, b)
#'
#' @inherit create_universe author references
#'
create_alpha_prior <- function(n, a, b, alpha_min = 0.001, n_grid=2000) {

  # Parameter checking
  if (!is.numeric(a) || length(a) != 1 || a < 2) {
    stop("shape parameter a must be a real number greater than or equal to 2.")
  }
  if (!is.numeric(b) || length(a) != 1 || a <= 0) {
    stop("shape parameter b must be a strictly positive real number.")
  }

  n_fact <- factorial(n)
  p_bar_min <- log(.Machine$double.xmin)   # Numerical minimum value
  p_bar_max <- -log(n_fact)                # Theoretical maximum value
  p_grid <- seq(p_bar_min, p_bar_max, length.out = n_grid)
  h <- (p_bar_max - p_bar_min)/(n_grid-1)
  alpha_mode <- rep(NA, n_grid)
  psi_diff <- rep(NA, n_grid)
  mode_error <- 0.0
  for (i in seq(n_grid-1)) {
    res <- stats::uniroot(log_f_alpha__pi_grad, c(0,2000),
                          p_grid[i] + 0.5*h, a, b, n_fact) # passed to function
    alpha_mode[i] <- res$root
    mode_error <- max(mode_error, res$estim.prec)
    psi_diff[i] <- digamma(1 + alpha_mode[i]) - digamma(1+alpha_mode[i]/n_fact)
  }

  # Compute prior density of alpha on a grid
  n_alpha_grid <- 200
  grid <- seq(0, stats::qgamma(0.995, a, b), length.out=n_alpha_grid)
  funcs <- list(cdf = list(x = grid, func = stats::pgamma(grid, a, b),
                           nse = rep(0, 200)),
                pdf = list(x = grid, func = stats::dgamma(grid, a, b),
                           nse = rep(0, 200)))
  list(a = a, b = b, alpha_min = alpha_min,
       p_grid = p_grid, p_bar_min = p_bar_min, h = h,
       alpha_mode = alpha_mode, mode_error = mode_error,
       psi_diff = psi_diff, funcs = funcs)
}

#' Evaluate the 1st derivative of log f(alpha|pi) with respect to alpha.
#'
#' @param alpha current value of alpha
#' @param p_bar sufficient statistic for pi equal to the arithmetic mean of
#' the log preference probabiliites.
#' @inheritParams create_alpha_prior
#' @param n_fact, the value n!, where n is the number of elements in the choice
#' universe.
#'
#' @return The first derivative of log f(alpha|pi) with respect to alpha
#'
#' @noRd
log_f_alpha__pi_grad <- function(alpha, p_bar, a, b, n_fact) {
  (a + n_fact - 2)/alpha - (b - p_bar) +
    digamma(1+alpha) - digamma(1+alpha/n_fact)
}

#' Compute gamma proposal distribution for alpha
#'
#' Find values of the three parameters of the Beta prime proposal distribution
#' for alpha that minimize the Renyi divergence of the target distribution, the
#' posterior distribution of the Dirichlet Random Choice model, from the
#' proposal distribution
#'
#' @inheritParams compute_pi_ln_like
#' @param alpha_prior list containing precomputed information about a Gamma
#'        prior distribution of alpha, created using [create_alpha_prior()]
#'
#' @return Parameter vector of Beta prime distribution
#' @export
#'
#' @examples
#' n <- 5
#' u <- create_universe(n)
#' alpha_prior <- create_alpha_prior(n, 4, 0.1)
#' Nv <- vectorize(u, RanCh::MMS_2019_counts[1, , ])
#' theta <- compute_proposal_params(u, alpha_prior, Nv)
#'
#' @inherit create_universe author references
#'
compute_proposal_params <- function(u, alpha_prior, Nv) {

  # Construct grid of alpha values
  max_alpha <- qgamma(0.01, alpha_prior$a, alpha_prior$b, lower.tail = FALSE)
  h <- (alpha_prior$a / alpha_prior$b) / 1000
  alpha_grid <- seq(h, max_alpha, by = h)

  # Evaluate f(alpha) Pr(N|alpha,lambda=0) on a grid
  alpha_Ax <- compute_alpha_Ax(u, alpha_grid)
  ln_Pr_RC_by_A <- compute_ln_Pr_by_A(u, 'RC', alpha_Ax, Nv)
  ln_Pr_N__al <- compute_ln_like(u, 0.0, ln_Pr_RC_by_A, ln_Pr_RC_by_A) # Last arg ignored
  ln_f_al <- dgamma(alpha_grid, alpha_prior$a, alpha_prior$b, log = TRUE)
  ln_f_al_N <- ln_Pr_N__al + ln_f_al
  ln_g <- ln_f_al_N - max(ln_f_al_N) # Subtract offset to avoid overflow
  g <- exp(ln_g)
  g <- g/(h*sum(g))
  param_init <- log(c(alpha_prior$a, 100*alpha_prior$b, 100))
  opt <- stats::optim(param_init, Renyi_al, gr = NULL, alpha_grid, g, h, 3)
  exp(opt$par)
}

#' Kullbeck-Leibler divergence between density in grid g and a Beta-Gamma
#' distribution with parameter vector theta.
#'
#' @param ln_theta log parameter vector of Beta-Gamma distribution
#' @param alpha_grid grid of values of alpha
#' @param g grid of values of target density
#' @param h distance between adjacent values of alpha on grid
#'
#' @return Kullbeck-Leibler divergence
#'
#' @importFrom extraDistr dbetapr
#'
#' @noRd
#'
KL <- function(ln_theta, alpha_grid, g, h) {
  theta <- exp(ln_theta)
  ln_ratio = log(g) -
    extraDistr::dbetapr(alpha_grid, theta[1], theta[2], theta[3], log = TRUE)
  result <- sum(ln_ratio * h * g)
}

#' Renyi divergence between density in grid g and a Beta-Gamma
#' distribution with parameter vector theta.
#'
#' @inheritParams KL
#' @param Ral order of Renyi divergence
#'
#' @return Renyi divergence
#'
#' @importFrom extraDistr dbetapr
#'
#' @noRd
#'
Renyi_al <- function(ln_theta, alpha_grid, g, h, Ral) {
  theta <- exp(ln_theta)
  ln_dbetapr <- extraDistr::dbetapr(alpha_grid, theta[1], theta[2], theta[3],
                                    log = TRUE)
  ratio = exp((Ral-1) * (log(g) - ln_dbetapr))
  result <- (1/(Ral-1)) * log(sum(ratio * h * g))
}
