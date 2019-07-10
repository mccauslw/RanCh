# Quantile of density value for third order Dirichlet
#
# \code{dDir3_quantile} computes an approximation of the given
# quantile of a third order Dirichlet density value, under that Dirichlet
# distribution.
# @param quantile the quantile of the desired density value
# @param alpha a vector of Dirichlet parameters
# @param normalized binary; if TRUE, return the quantile as a fraction of
# the maximum density value; if FALSE, return the unnormalized quantile.
# @return The value of the quantile, normalized or not
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

#' Dirichlet density function
#'
#' \code{dDir} computes the Dirichlet density at a point \code{p} in the
#' regular simplex, for a vector \code{alpha} of Dirichlet parameters.
#' @param p vector of probabilities on the regular simplex
#' @param alpha vector of Dirichlet parameters
#' @param log logical; if TRUE, the log density is returned
#' @return density or log density value
#' @export
#' @examples
#' f = dDir(c(0.1, 0.3, 0.6), c(2.5, 0.5, 1.0))
dDir <- function(p, alpha, log=FALSE) {
  ln_f = lgamma(sum(alpha)) - sum(lgamma(alpha))
  ln_f = ln_f + sum((alpha-1)*log(p))
  if (log) ln_f else exp(ln_f)
}

dDir3 <- function(p1, p2, alpha, log=FALSE) {
  p3 = pmax(0, 1-p1-p2)
  ln_f = lgamma(sum(alpha)) - sum(lgamma(alpha))
  ln_f = ln_f + (alpha[1]-1)*log(p1) + (alpha[2]-1)*log(p2) + (alpha[3]-1)*log(p3)
  if (log) ln_f else exp(ln_f)
}

# Maximum density of a Dirichlet distribution
#
# \code{dDir_max} computes the maximum density value of a Dirichlet
# distribution as a function of the parameter vector alpha.
# @param alpha vector of Dirichlet parameters.
# @param log logical; if TRUE, the log maximum density is returned.
# @return Density or log density value.
dDir_max <- function(alpha, log=FALSE) {
  # Compute normalization constant
  ln_d_max = lgamma(sum(alpha)) - sum(lgamma(alpha))
  # Compute log density kernel at mode
  ln_d_max = ln_d_max + sum((alpha-1)*log(alpha-1))
  ln_d_max = ln_d_max - (sum(alpha)-length(alpha))*log(sum(alpha)-length(alpha))
  if (log) ln_d_max else exp(ln_d_max)
}

#' Log marginal likelihood for Dirichlet-multinomial model
#'
#' \code{log_ML_Dir_mult} computes the log marginal likelihood for a multinomial
#' data generating process and a Dirichlet prior over choice probabilities.
#' @param alpha vector of Dirichlet parameters
#' @param N vector of multinomial counts
#' @param log logical; if TRUE, return the log marginal likelihood; if FALSE,
#' the marginal likelihood. log=FALSE is usually not recommendend, as underflow
#' is likely.
#' @export
#' @return Marginal likelihood or log marginal likelihood
log_ML_Dir_mult <- function(alpha, N, log=TRUE) {
  # Compute prior and posterior normalization constants
  alpha = na.omit(alpha)
  N = na.omit(N)
  ln_prior_nc = lgamma(sum(alpha)) - sum(lgamma(alpha))
  ln_post_nc = lgamma(sum(alpha + N)) - sum(lgamma(alpha + N))
  ln_ML = ln_prior_nc - ln_post_nc
  if (log) ln_ML else exp(ln_ML)
}

#' Marginal likelihood for discrete choice experiment, Dirichlet-multinomial model
#'
#' \code{ML_DCE_Dir_mult} computes the marginal likelihood for a model
#' where choice count vectors are independent multinomial across choice sets
#' and choice probability vectors are independent Dirichlet across choice sets.
#' @param A matrix of Dirichlet parameters, each row giving the Dirichlet
#' distribution of the corresponding row of a random choice structure.
#' @param N count matrix with the same dimensions as A, pertaining to the same
#' universe of objects.
#' @param log logical; if TRUE, return the log Bayes factor
#' @export
log_ML_DCE_Dir_mult <- function(A, N, log=TRUE) {
  ln_ML = 0
  for (i in 1:nrow(A)) {
    if (subset_card[i] > 1) {
      ln_ML = ln_ML + ML_Dir_mult(A[i, ], N[i, ], log=TRUE)
    }
  }
  if (log) ln_ML else exp(ln_ML)
}

#' One-parameter Dirichlet prior for a RCS
#'
#' \code{prior_DCE_scalar_alpha} computes a matrix of Dirichlet parameters for a
#' one-parameter Dirichlet prior for a random choice structure.
#' @param alpha univariate parameter for the one-parameter Dirichlet prior.
#' @param n_objects number of objects in the universe.
#' @export
#' @return a matrix of Dirichlet parameters with the same dimensions as a
#' count matrix for a universe of the same size.
prior_DCE_scalar_alpha <- function(alpha, n_objects) {
  n_subsets = 2^n_objects-1
  (alpha/subset_card[1:n_subsets]) * member_table[1:n_subsets, 1:n_objects]
}

# Moments of Dirichlet density values
#
# \code{dDir_moments} computes a vector of the first \code{n_mu} raw
# moments of the Dirichlet density value.
# The given Dirichlet distribution induces a distribution of the (scalar)
# value of corresponding Dirichlet density evaluated at a random draw from
# that distribution.
# Computed moments are of the induced univariate distribution, not the
# multivariate Dirichlet distribution itself.
#
# @param alpha vector of Dirichlet parameters.
# @param n_mu number of moments to compute.
# @param log logical; if true return log moments of the density value.
# @return vector of moments
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

#' Highest Density (HD) region for a third order Dirichlet distribution
#'
#' \code{Dir3_HD_region} constructs a polygon approximating the highest density
#' region of a third order Dirichlet distribution.
#' The polygon is in a three-dimensional barycentric coordinate system.
#' This can be used to construct approximate highest prior density and
#' highest posterior density (HPD) regions for a Dirichlet-multinomial model.
#' @param alpha vector of three (positive) Dirichlet parameters.
#' @param HD_probability scalar in [0,1] giving the probability of the HD region
#' @export
#' @return matrix giving polygon approximation of HD region.
#' Each row gives a polygon vertex.
#' The three columns correspond to coordinates in a barycentric coordinate system.
Dir3_HD_region = function(alpha, HD_probability) {
  # Grid of points
  p1 <- p2 <- seq(0, 1, by=0.001)
  # Evaluation of Di density on grid
  f <- outer(p1, p2, FUN=dDir3, alpha, log=FALSE)
  q.d = dDir3_quantile(1-HD_probability, alpha, FALSE)
  cl = contourLines(p1, p2, f, levels = q.d) # Contour
  nrow = length(cl[[1]]$x)
  matrix(c(cl[[1]]$x, cl[[1]]$y, 1-cl[[1]]$x-cl[[1]]$y),
         byrow=FALSE, nrow=nrow, ncol=3)
}

#' Highest Density (HD) region for a second order Dirichlet distribution
#'
#' \code{Dir2_HD_region} constructs a line segment approximating the highest
#' density region of a second order Dirichlet distribution.
#' The line segment is in a second order barycentric coordinate system.
#' This can be used to compute highest prior density and highest posterior
#' density (HPD) regions for a second order Dirichlet-multinomial model
#' (i.e. a beta-binomial model).
#' @param alpha vector of three (positive) Dirichlet parameters.
#' @param HD_probability scalar in [0,1] giving the probability of the HD region
#' @export
#' @return matrix giving polygon approximation of HD region.
#' Each row gives a polygon vertex.
#' The three columns correspond to coordinates in a barycentric coordinate system.
Dir2_HD_region = function(alpha, HD_probability) {
  interval = hpd(function(x) dbeta(x, alpha[1], alpha[2]), c(0, 1),
                 cdf = function(x) pbeta(x, alpha[1], alpha[2]),
                 HD_probability)
  matrix(c(interval$lower, 1-interval$lower, interval$upper, 1-interval$upper),
         byrow=TRUE, nrow=2, ncol=2)
}

#' Transform 2nd order barycentric coordinates to 3rd order
#'
#' \code{binary2ternary} transforms the 2nd order barycentric coordinates of the
#' rows of \code{binary_points} into the 3rd order barycentric coordinates of the
#' rows of \code{ternary_points}.
#' This is useful for plotting points and line segments of binary choice
#' probabilities on the sides of the 3rd order barycentric coordinate system.
#' The two non-zero columns of the result are specified in the 2-vector
#' ternary_cols.
#' @examples
#' binary2ternary(matrix(c(0.6, 0.4), nrow=1), c(2,3)) # returns 1x3 matrix [0 0.6 0.4]
#' binary2ternary(matrix(c(0.6, 0.4), nrow=1), c(3,2)) # returns 1x3 matrix [0 0.4 0.6]
binary2ternary <- function(binary_points, ternary_cols)
{
  ternary_points <- matrix(0, nrow=nrow(binary_points), ncol=3)
  ternary_points[, ternary_cols[1]] <- binary_points[, 1]
  ternary_points[, ternary_cols[2]] <- binary_points[, 2]
  ternary_points
}

#' Plot highest density region for a third order Dirichlet distribution
#'
#' This function plots the Dirichlet highest density region in barycentric
#' coordinates.
#' @param alpha vector of Dirichlet parameters
#' @param HD_probability probability of highest density region
#' @export
#' @examples
#' plot_HD_Dir3(A, 0.95, c(1,2,3))
plot_HD_Dir3 <- function(A, HD_probability, selection)
{
  obj1 = selection[1]; obj2 = selection[2]; obj3 = selection[3]
  set_123 = set_index(c(obj1, obj2, obj3))
  set_12 = set_index(c(obj1, obj2))
  set_23 = set_index(c(obj2, obj3))
  set_13 = set_index(c(obj1, obj3))

  # Highest density region for ternary probability
  HD_region = Dir3_HD_region(A[set_123, c(obj1, obj2, obj3)], HD_probability)
  polygon(tritrafo(HD_region), col='lightgreen')

  # Highest density region for (p1, p2)
  HD12 = binary2ternary(Dir2_HD_region(A[set_12, c(obj1, obj2)], HD_probability), c(1, 2))
  lines(tritrafo(HD12), lwd=4)

  # Highest density region for (p2, p3)
  HD23 = binary2ternary(Dir2_HD_region(A[set_23, c(obj2, obj3)], HD_probability), c(2, 3))
  lines(tritrafo(HD23), lwd=4)

  # Highest density region for (p1, p3)
  HD13 = binary2ternary(Dir2_HD_region(A[set_13, c(obj1, obj3)], HD_probability), c(1, 3))
  lines(tritrafo(HD13), lwd=4)
}
