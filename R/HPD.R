#' Quantile of density value for third order Dirichlet
#'
#' \code{dDir3_quantile} computes an approximation of the given
#' quantile of a third order Dirichlet density value, under that Dirichlet
#' distribution.
#' @param quantile the quantile of the desired density value
#' @param alpha a vector of Dirichlet parameters
#' @param normalized binary; if TRUE, return the quantile as a fraction of
#' the maximum density value; if FALSE, return the unnormalized quantile.
#' @return The value of the quantile, normalized or not
#' @importFrom stats qbeta
dDir3_quantile <- function(quantile, alpha, normalized=FALSE) {
  ld_max <- dDir_max(alpha, log=TRUE)
  m <- exp(dDir_moments(alpha, 2, log=TRUE) - c(1,2)*ld_max)
  r <- m[2]/m[1]
  den <- r*(1-m[1]) - m[1]*(1-r)
  theta1 <- m[1]*(1-r)/den
  theta2 <- (1-m[1])*(1-r)/den
  q <- qbeta(quantile, theta1, theta2)
  if (normalized) q else q * exp(ld_max)
}

#' Dirichlet density function
#'
#' \code{dDirichlet} computes the Dirichlet density at a point \code{p} in the
#' regular simplex, for a vector \code{alpha} of Dirichlet parameters.
#' @param p vector of probabilities on the regular simplex
#' @param alpha vector of Dirichlet parameters
#' @param log logical; if TRUE, the log density is returned
#' @return density or log density value
#' @export
#' @examples
#' f <- dDirichlet(c(0.1, 0.3, 0.6), c(2.5, 0.5, 1.0))
dDirichlet <- function(p, alpha, log=FALSE) {
  ln_f <- lgamma(sum(alpha)) - sum(lgamma(alpha))
  ln_f <- ln_f + sum((alpha-1)*log(p))
  if (log) ln_f else exp(ln_f)
}

#' Dirichlet random variates
#'
#' \code{rDirichlet} draws from the Dirichlet distribution
#' @param n number of draws
#' @param alpha vector of Dirichlet parameters
#' @return matrix with n rows, each a draw from the Dirichlet distribution
#' @importFrom stats rgamma
#' @export
#' @examples
#' library(klaR)
#' p <- rDirichlet(1000, c(1, 1, 1)) # Uniform distribution on 2-simplex
#' triplot(label=c('x', 'y', 'z'))
#' plot(tritrafo(p))
rDirichlet <-function(n, alpha) {
  K <- length(alpha)
  p <- matrix(0, nrow=n, ncol=K)
  for (i in 1:K) p[, i] <- rgamma(n, alpha[i])
  p <- p/rowSums(p)
}

dDir3 <- function(p1, p2, alpha, log=FALSE) {
  p3 <- pmax(0, 1-p1-p2)
  ln_f <- lgamma(sum(alpha)) - sum(lgamma(alpha))
  ln_f <- ln_f + (alpha[1]-1)*log(p1) + (alpha[2]-1)*log(p2) + (alpha[3]-1)*log(p3)
  if (log) ln_f else exp(ln_f)
}

# Maximum density of a Dirichlet distribution
#
# \code{dDir_max} computes the maximum density value of a Dirichlet
# distribution as a function of the parameter vector alpha.
# @param alpha vector of Dirichlet parameters.
# @param log logical; if \code{TRUE}, the log maximum density is returned.
# @return Density or log density value.
dDir_max <- function(alpha, log=FALSE) {
  # Compute normalization constant
  ln_d_max <- lgamma(sum(alpha)) - sum(lgamma(alpha))
  # Compute log density kernel at mode
  ln_d_max <- ln_d_max + sum((alpha-1)*log(alpha-1))
  ln_d_max <- ln_d_max - (sum(alpha)-length(alpha))*log(sum(alpha)-length(alpha))
  if (log) ln_d_max else exp(ln_d_max)
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
  ln_common <- lgamma(sum(beta)) - sum(lgamma(beta))
  log_mu <- rep(0, n_mu);
  for (j in 1:n_mu) {
    ln_j.factor <- sum(lgamma((j+1)*beta-j))
    ln_j.factor <- ln_j.factor - lgamma((j+1)*sum(beta)-length(beta)*j)
    log_mu[j] <- (j+1)*ln_common + ln_j.factor
  }
  if (log) log_mu else exp(log_mu)
}

#' Highest Density (HD) region for a third order Dirichlet distribution
#'
#' \code{Dir3_HD_region} constructs a polygon approximating the highest density
#' region of a third order Dirichlet distribution.
#' The polygon is in a two-dimensional barycentric coordinate system.
#' This can be used to construct approximate highest prior density and
#' highest posterior density (HPD) regions for a Dirichlet-multinomial model.
#' @param alpha vector of three (positive) Dirichlet parameters.
#' @param HD_probability scalar in [0,1] giving the probability of the HD region
#' @return matrix giving polygon approximation of HD region.
#' Each row gives a polygon vertex.
#' The three columns correspond to coordinates in a barycentric coordinate system.
#' @importFrom grDevices contourLines
#' @export
Dir3_HD_region <- function(alpha, HD_probability) {
  # Grid of points
  p1 <- p2 <- seq(0, 1, by=0.001)
  # Evaluation of Di density on grid
  f <- outer(p1, p2, FUN=dDir3, alpha, log=FALSE)
  q.d <- dDir3_quantile(1-HD_probability, alpha, FALSE)
  cl <- contourLines(p1, p2, f, levels = q.d) # Contour
  nrow <- length(cl[[1]]$x)
  matrix(c(cl[[1]]$x, cl[[1]]$y, 1-cl[[1]]$x-cl[[1]]$y),
         byrow=FALSE, nrow=nrow, ncol=3)
}

#' Highest Density (HD) region for a second order Dirichlet distribution
#'
#' \code{Dir2_HD_region} constructs a line segment approximating the highest
#' density region of a second order Dirichlet (i.e. beta) distribution.
#' The line segment is in a first order barycentric coordinate system.
#' This can be used to compute highest prior density and highest posterior
#' density (HPD) regions for a second order Dirichlet-multinomial model
#' (i.e. a beta-binomial model).
#' @param alpha vector of two positive Dirichlet parameters.
#' @param HD_probability scalar in \eqn{[0,1]} giving the probability of the HD region
#' @return matrix giving line segment approximation of HD region.
#' Each row gives an endpoint.
#' The two columns correspond to coordinates in a one dimensional barycentric
#' coordinate system.
#' @importFrom HDInterval hdi
#' @importFrom stats qbeta
#' @export
Dir2_HD_region <- function(alpha, HD_probability) {
  interval <- hdi(qbeta, HD_probability, shape1=alpha[1], shape2=alpha[2])
  matrix(c(interval[['lower']], 1-interval[['lower']], interval[['upper']], 1-interval[['upper']]),
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
#' @param binary_points matrix of points, with each row a 2-vector on the 1-simplex
#' @param ternary_cols 2-vector specifying the columns of the result
#' corresponding to the two columns of \code{binary_points}
#' @return matrix of points, with each row a 3-vector on the 2-simplex. The first
#' element of \code{ternary_cols} gives the column of the result that takes the values
#' of the first column of \code{binary_points}; the second element gives the column of
#' the result taking the values of the second column of \code{binary_points}.
#' The remaining column of the result is set to zero.
#' @export
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

#' Compute all highest density regions for RCS with 3 objects
#'
#' This function computes the Dirichlet highest density (HD) regions in
#' barycentric coordinates for the three binary choice probability vectors and
#' one ternary choice probability vectors of a RCS with a multiple Dirichlet
#' distribution.
#' @param Alpha matrix of Dirichlet parameters specifying the distribution of
#' an RCD
#' @param HD_probability scalar giving the probability of the HD region
#' @param selection vector of length three specifying the three columns of
#' \code{Alpha} to use for plotting.
#' @export
#' @examples
#' library(klaR)
#' N_bce <- marginalize(MMS_2019_counts['Beer', , ], c(2, 3, 5)) # Counts for objects 2, 3, 5
#' prior_Alpha <- DirRC_constant_sum(3, 2.0) # Parameters of simple conjugate prior
#' post_Alpha <- prior_Alpha + N_bce         # Posterior parameters
#' HD3 <- Dir2_3_HD_region(post_Alpha, 0.9, c(1,2,3))
#' triplot(label=c('b', 'c', 'e'))               # Set up ternary plot
#' lines(tritrafo(HD3$HD12), lwd=4)              # Plot three binaries
#' lines(tritrafo(HD3$HD23), lwd=4)
#' lines(tritrafo(HD3$HD13), lwd=4)
#' polygon(tritrafo(HD3$HD123), border='lightgreen') # Plot ternary
Dir2_3_HD_region <- function(Alpha, HD_probability, selection=c(1, 2, 3))
{
  obj1 <- selection[1]; obj2 <- selection[2]; obj3 <- selection[3]
  set_123 <- set_index(c(obj1, obj2, obj3))
  set_12 <- set_index(c(obj1, obj2))
  set_23 <- set_index(c(obj2, obj3))
  set_13 <- set_index(c(obj1, obj3))

  # Highest density region for ternary probability
  HD123 <- Dir3_HD_region(Alpha[set_123, c(obj1, obj2, obj3)], HD_probability)

  # Highest density region for (p1, p2)
  HD12 <- binary2ternary(Dir2_HD_region(Alpha[set_12, c(obj1, obj2)],
                                        HD_probability), c(1, 2))

  # Highest density region for (p2, p3)
  HD23 <- binary2ternary(Dir2_HD_region(Alpha[set_23, c(obj2, obj3)],
                                        HD_probability), c(2, 3))

  # Highest density region for (p1, p3)
  HD13 <- binary2ternary(Dir2_HD_region(Alpha[set_13, c(obj1, obj3)],
                                        HD_probability), c(1, 3))
  list(HD123=HD123, HD12=HD12, HD23=HD23, HD13=HD13)
}
