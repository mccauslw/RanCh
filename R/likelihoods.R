## Basic likelihoods, as functions of RCS P or multiple Dirichlet Alpha

#' Log likelihood for multinomial model
#'
#' \code{log_L_multinomial} computes the log likelihood function for a
#' multinomial model
#' @param p vector of probabilities
#' @param n vector of counts
#' @param log logical; if \code{TRUE}, return the log likelihood;
#' if \code{FALSE}, the likelihood.
#' \code{log=FALSE} is usually not recommendend, as underflow is likely.
#' @importFrom stats na.omit
#' @export
#' @return log likelihood or likelihood
log_L_multinomial <- function(p, n, log=TRUE) {
  p <- na.omit(p)
  n <- na.omit(n)
  ln_L <- sum(n * log(p))
  if (log) ln_L else exp(ln_L)
}

#' Log likelihood for DCE, multiple multinomial model
#'
#' \code{log_L_DCE_multinomial} computes the log likelihood function for a count matrix
#' as a function of a RCS.
#' @param P matrix containing a random choice structure (RCS)
#' @param N matrix containing counts from a discrete choice experiment (DCE)
#' @param log logical; if \code{TRUE}, return the log likelihood;
#' if \code{FALSE}, the likelihood.
#' \code{log=FALSE} is usually not recommendend, as underflow is likely.
#' @export
#' @return log likelihood or likelihood
log_L_DCE_multinomial <- function(P, N, log=TRUE) {
  ln_L <- 0
  for (i in 1:nrow(P)) {
    if (subset_card[i] > 1) {
      ln_L <- ln_L + log_L_multinomial(P[i, ], N[i, ], log=TRUE)
    }
  }
  if (log) ln_L else exp(ln_L)
}

#' Log likelihood, Dirichlet-multinomial model
#'
#' \code{log_L_Dir_mult} computes the log marginal likelihood for a multinomial
#' data generating process and a Dirichlet prior over choice probabilities.
#' @param alpha vector of Dirichlet parameters
#' @param n vector of multinomial counts
#' @param log logical; if \code{TRUE}, return the log likelihood;
#' if \code{FALSE}, the likelihood.
#' \code{log=FALSE} is usually not recommendend, as underflow is likely.
#' @importFrom stats na.omit
#' @export
#' @return Marginal likelihood or log marginal likelihood
log_L_Dir_mult <- function(alpha, n, log=TRUE) {
  # Compute prior and posterior normalization constants
  alpha <- na.omit(alpha)
  n <- na.omit(n)
  ln_prior_nc <- lgamma(sum(alpha)) - sum(lgamma(alpha))
  ln_post_nc <- lgamma(sum(alpha + n)) - sum(lgamma(alpha + n))
  ln_L <- ln_prior_nc - ln_post_nc
  if (log) ln_L else exp(ln_L)
}

#' Log likelihood for DCE, multiple Dirichlet-multinomial model
#'
#' \code{log_ML_DCE_Dir_mult} computes the marginal likelihood for a model
#' where choice count vectors are independent multinomial across choice sets
#' and choice probability vectors are independent Dirichlet across choice sets.
#' @param Alpha matrix of Dirichlet parameters, each row giving the Dirichlet
#' distribution of the corresponding row of a random choice structure.
#' @param N count matrix with the same dimensions as \code{Alpha}, pertaining to the same
#' universe of objects.
#' @param log logical; if TRUE, return the log Bayes factor
#' @export
log_L_DCE_Dir_mult <- function(Alpha, N, log=TRUE) {
  ln_L <- 0
  for (i in 1:nrow(Alpha)) {
    if (subset_card[i] > 1) {
      ln_L <- ln_L + log_L_Dir_mult(Alpha[i, ], N[i, ], log=TRUE)
    }
  }
  if (log) ln_L else exp(ln_L)
}

## Basic RCS construction

#' P_uniform
#'
#' \code{P_uniform} constructs a random choice structure (RCS) where all
#' choice probabilities are discrete uniform
#' @param n_objects integer, number of objects in universe
#' @return RCS where all choice probabilities are discrete uniform
#' @export
P_uniform <- function(n_objects) {
  n_subsets = 2^n_objects - 1
  P = member_table[1:n_subsets, 1:n_objects]
  for (i in 1:n_subsets) {
    P[i, ] = P[i, ]/subset_card[i]
  }
  P
}

#' P_Luce
#'
#' \code{P_Luce} constructs a random choice structure (RCS) from a Luce
#' model.
#' @param v postive vector, Luce weights
#' @export
#' @return RCS with choice probabilities given by Luce weights
P_Luce <- function(v) {
  n_objects = length(v)
  n_subsets = 2^n_objects - 1
  P = member_table[1:n_subsets, 1:n_objects]
  for (i in 1:n_subsets) {
    P[i, ] = P[i, ] * v / sum(v * member_table[i, ], na.rm=TRUE)
  }
  P
}

## Basic Dirichlet Alpha construction

#' Uniform prior for a RCS
#'
#' \code{RCS_uniform_prior} constructs a matrix of Dirichlet parameters for the
#' uniform prior over a random choice structure.
#' @param n_objects number of objects in the universe.
#' @export
#' @return a matrix of Dirichlet parameters all set to one, so that all choice
#' probability vectors are uniformly distributed.
RCS_uniform_prior <- function(n_objects) {
  n_subsets <- 2^n_objects-1
  member_table[1:n_subsets, 1:n_objects]
}

#' Scalar alpha prior for a RCS
#'
#' \code{RCS_scalar_alpha_prior} constructs a matrix of Dirichlet parameters
#' for a one-parameter Dirichlet prior for a random choice structure.
#' @param alpha univariate parameter for the one-parameter Dirichlet prior.
#' @param n_objects number of objects in the universe.
#' @export
#' @return a matrix of Dirichlet parameters for the scalar alpha prior
RCS_scalar_alpha_prior <- function(alpha, n_objects) {
  n_subsets <- 2^n_objects-1
  Alpha = matrix(0, nrow=n_subsets, ncol=n_objects)
  for (i in 1:n_subsets) {
    Alpha[i, ] = alpha/subset_card[i]
  }
  Alpha * member_table[1:n_subsets, 1:n_objects]
}

#' Vector alpha prior for a RCS
#'
#' \code{RCS_vector_alpha_prior} computes a matrix of Dirichlet parameters
#' for a vector Dirichlet prior for a random choice structure.
#' @param alpha scalar, prior precision parameter
#' @param v vector, object weights for the vector Dirichlet prior.
#' @export
#' @return a matrix of Dirichlet parameters for the vector Dirichlet prior.
RCS_vector_alpha_prior <- function(alpha, v) {
  n_objects <- length(v)
  n_subsets <- 2^n_objects-1
  Alpha = matrix(0, nrow=n_subsets, ncol=n_objects)
  for (i in 1:n_subsets) {
    den <- sum(v[subset_vectors[[i]]])
    Alpha[i, subset_vectors[[i]]] = alpha * v[subset_vectors[[i]]]/den
  }
  Alpha * member_table[1:n_subsets, 1:n_objects]
}

#' Marginal likelihood for DCE, vector Dirichlet multinomial model
#'
#' \code{log_ML_DCE_vector_alpha_Dir_mult} computes the marginal likelihood for a model
#' where choice probability vectors \eqn{P_A} are mutually independent, and
#' \deqn{P_A(x_1,\ldots,x_{|A|}) \sim \mathrm{Di}(u_1,\ldots,u_|A|)},
#' and choices are conditionally independent multinomial.
#' @param u vector determining Dirichlet parameters
#' @param N count matrix
#' @param log logical; if \code{TRUE}, return the log marginal likelihood;
#' if \code{FALSE}, the marginal likelihood.
#' \code{log=FALSE} is usually not recommendend, as underflow is likely.
#' @export
log_ML_DCE_vector_alpha_Dir_mult <- function(u, N, log=TRUE) {
  Alpha_prior = prior_DCE_vector_alpha(u)
  ln_ML = log_ML_DCE_Dir_mult(Alpha_prior, N)
  if (log) ln_ML else exp(ln_ML)
}
