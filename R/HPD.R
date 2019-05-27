#' Quantile of third order Dirichlet density value
#'
#' \code{dDir3_quantile} computes an approximation of the given
#' quantile of a third order Dirichlet density value, under that Dirichlet
#' distribution.
#' @param quantile the quantile of the desired density value
#' @param alpha a vector of Dirichlet parameters
#' @param normalized binary; if TRUE, return the quantile as a fraction of
#' the maximum density value; if FALSE, return the unnormalized quantile.
#' @return The value of the quantile, normalized or not
dDir3_quantile <- function(quantile, alpha, normalized=FALSE) {
  ld_max = dDir_max(alpha, log=TRUE)
  m = exp(dDir_moments(alpha, 2, log=TRUE) - c(1,2)*ld_max)
  r = m[2]/m[1]
  den = r*(1-m[1]) - m[1]*(1-r)
  theta1 = m[1]*(1-r)/den
  theta2 = (1-m[1])*(1-r)/den
  q = qbeta(quantile, theta1, theta2)
  if (normalized) q else q * exp(ld_max)
}

#' Dirichlet density
#'
#' \code{dDir} computes the Dirichlet density at a point \code{p} in the
#' regular simplex, for a vector \code{alpha} of Dirichlet parameters.
#' @param p vector of probabilities on the regular simplex
#' @param alpha vector of Dirichlet parameters
#' @param log logical; if TRUE, the log density is returned
#' @return density or log density value
#' @export
dDir <- function(p, alpha, log=TRUE) {
  ln_f = lgamma(sum(alpha)) - sum(lgamma(alpha))
  ln_f = ln_f + sum((alpha-1)*log(p))
  if (log) ln_f else exp(ln_f)
}

dDir3 <- function(p1, p2, alpha, log=TRUE) {
  p3 = pmax(0, 1-p1-p2)
  ln_f = lgamma(sum(alpha)) - sum(lgamma(alpha))
  ln_f = ln_f + (alpha[1]-1)*log(p1) + (alpha[2]-1)*log(p2) + (alpha[3]-1)*log(p3)
  if (log) ln_f else exp(ln_f)
}

#' Maximum density of a Dirichlet distribution
#'
#' \code{max_dDir} computes the maximum density of a Dirichlet distribution
#' as a function of the parameter vector alpha.
#' @param alpha vector of Dirichlet parameters.
#' @param log logical; if TRUE, the log maximum density is returned.
#' @return Density or log density value.
dDir_max <- function(alpha, log=TRUE) {
  # Compute normalization constant
  ln_d_max = lgamma(sum(alpha)) - sum(lgamma(alpha))
  # Compute log density kernel at mode
  ln_d_max = ln_d_max + sum((alpha-1)*log(alpha-1))
  ln_d_max = ln_d_max - (sum(alpha)-length(alpha))*log(sum(alpha)-length(alpha))
  if (log) ln_d_max else exp(ln_d_max)
}

#' Marginal likelihood for Dirichlet-multinomial model
#'
#' \code{Dir_mult_ML} computes the marginal likelihood for a Dirichlet prior
#' and multinomial data generating process.
#' @param alpha vector of Dirichlet parameters
#' @param N vector of multinomial counts
#' @param log logical; if TRUE, return the log Bayes factor.
#' @export
#' @return Marginal likelihood or log marginal likelihood
Dir_mult_ML <- function(alpha, N, log=TRUE) {
  # Compute prior and posterior normalization constants
  alpha = na.omit(alpha)
  N = na.omit(N)
  ln_prior_nc = lgamma(sum(alpha)) - sum(lgamma(alpha))
  ln_post_nc = lgamma(sum(alpha + N)) - sum(lgamma(alpha + N))
  ln_ML = ln_prior_nc - ln_post_nc
  if (log) ln_ML else exp(ln_ML)
}

#' Marginal likelihood for independent Dirichlet-multinomial model
#'
#' \code{Ind_Dir_mult_ML} computes the marginal likelihood for a model
#' where rows of a count matrix are independent multinomial and the
#' rows of the unknown random choice structure are a priori independent
#' Dirichlet.
#' @param A matrix of Dirichlet parameters, each row giving the Dirichlet
#' distribution of the corresponding row of a random choice structure.
#' @param N count matrix for a universe of objects.
#' @export
#' @param log logical; if TRUE, return the log Bayes factor
Ind_Dir_mult_ML <- function(A, N, log=TRUE) {
  ln_ML = 0
  for (i in 1:nrow(A)) { # WJM: avoid loop if at all possible
    if (subset_card[i] > 1) {
      ln_ML = ln_ML + Dir_mult_ML(A[i, ], N[i, ], log=TRUE)
    }
  }
  if (log) ln_ML else exp(ln_ML)
}

#' One-parameter Dirichlet prior for a RCS
#'
#' \code{RCS_prior_1} computes a matrix of Dirichlet parameters for a
#' one-parameter Dirichlet prior for a random choice structure.
#' @param alpha univariate parameter for the one-parameter Dirichlet prior.
#' @param n_objects number of objects in the universe.
#' @export
#' @return a matrix of Dirichlet parameters with the same dimensions as a
#' count matrix for a universe of the same size.
RCD_prior_1 <- function(alpha, n_objects) {
  n_subsets = 2^n_objects-1
  (alpha/subset_card[1:n_subsets]) * member_table[1:n_subsets, 1:n_objects]
}

#' Moments of Dirichlet density values
#'
#' \code{moments_dDi} computes a vector of the first n raw moments of Dirichlet
#' density values, under that Dirichlet distribution.
#' @param alpha vector of Dirichlet parameters.
#' @param n_mu number of moments to compute.
#' @param log logical; if true return log moments.
#' @return vector of moments
dDir_moments <- function(beta, n_mu, log=FALSE) {
  ln_common = lgamma(sum(beta)) - sum(lgamma(beta))
  log_mu = rep(0, n_mu);
  for (j in 1:n_mu) {
    ln_j.factor = sum(lgamma((j+1)*beta-j))
    ln_j.factor = ln_j.factor - lgamma((j+1)*sum(beta)-length(beta)*j)
    log_mu[j] = (j+1)*ln_common + ln_j.factor
  }
  if (log) log_mu else exp(log_mu)
}

beta_HD_interval = function(alpha, beta, HD_probability) {
  hpd(function(x) dbeta(x, alpha, beta), c(0, 1),
      cdf = function(x) pbeta(x, alpha, beta), HD_probability)
}

#' Compute highest density (HD) region for a third order Dirichlet distribution
#'
#' This function computes a polygon approximating the highest density region
#' of a third order Dirichlet distribution. This can be used to compute highest
#' prior density and highest posterior density (HPD) regions.
#' @param alpha a vector of three (positive) Dirichlet parameters.
#' @param HD_probability probability of region to construct
#' @export
#' @return polygon approximation of HD region.
Dir3_HD_region = function(alpha, HD_probability) {
  # Grid of points
  p1 <- p2 <- seq(0, 1, by=0.001)
  # Evaluation of Di density on grid
  f <- outer(p1, p2, FUN=dDir3, alpha, log=FALSE)
  q.d = dDir3_quantile(1-HD_probability, alpha, FALSE)
  cl = contourLines(p1, p2, f, levels = q.d) # Contour
  nrow = length(cl[[1]]$x)
  region = matrix(c(cl[[1]]$x, cl[[1]]$y, 1-cl[[1]]$x-cl[[1]]$y),
                  byrow=FALSE, nrow=nrow, ncol=3)
}

#' Plot highest density region for a third order Dirichlet distribution
#'
#' This function plots the Dirichlet highest density region in barycentric
#' coordinates.
#' @param alpha vector of Dirichlet parameters
#' @param HD_probability probability of highest density region
#' @export
#' @examples
#' plot_HD_Dir_3(0.95, c(23, 13, 4))
plot_HD_Dir3 <- function(A, HD_probability) { # WJM: add permutation argument

  # Highest density region for ternary probability
  HD_region = Dir3_HD_region(A[7, ], HD_probability)
  polygon(tritrafo(HD_region), col='lightgreen')

  # Highest density region for (p1, p2)
  int <- hpd(function(x) dbeta(x, A[3, 1], A[3, 2]), c(0, 1),
             cdf = function(x) pbeta(x, A[3, 1], A[3, 2]), HD_probability)
  lines(tritrafo(c(int$lower, int$upper), c(1-int$lower,1-int$upper)), lwd=4)

  # Highest density region for (p2, p3)
  int <- hpd(function(x) dbeta(x, A[6, 2], A[6, 3]), c(0, 1),
             cdf = function(x) pbeta(x, A[6, 2], A[6, 3]), HD_probability)
  lines(tritrafo(c(0,0), c(int$lower,int$upper)), lwd=4)

  # Highest density region for (p1, p3)
  int <- hpd(function(x) dbeta(x, A[5, 1], A[5, 3]), c(0, 1),
             cdf = function(x) pbeta(x, A[5, 1], A[5, 3]), HD_probability)
  lines(tritrafo(c(int$lower, int$upper), c(0,0)), lwd=4)
}
