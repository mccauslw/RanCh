## Basic likelihoods, as functions of RCS P or multiple Dirichlet Alpha,
## as well as correponding simulation routines

#' Simulation for DCE, multiple multinomial model
#'
#' \code{sim_DCE_multinomial} draws random counts given probabilities in a RCS \code{P}.
#' Total counts for each choice subset are specified in the vector \code{N_total}.
#' @param n number of draws
#' @param P matrix containing a random choice structure (RCS)
#' @param N_total vector containing count totals (i.e. number of trials) for each
#' subset of the universe of choice objects.
#' @return a random count matrix
#' @examples
#' u = c(1, 2, 3); n_objects=3; n_subsets=2^n_objects-1;
#' P = P_Luce(u)
#' N_total = vector(mode="integer", length=n_subsets)
#' N_total[singletons[1:3]] = 0
#' N_total[doubletons[1:3]] = 10
#' N_total[tripletons[1]] = 10
#' N = sim_DCE_multinomial(5, P, N_total)
#' print(N[1,,], na.print='-') # Print first drawn count matrix
#' log_L_DCE_multinomial(P, N[1,,], log=TRUE)
#' @seealso \code{\link{log_L_DCE_multinomial}}, which computes the log-likelihood
#' for this model.
#' @importFrom stats rmultinom
#' @export
sim_DCE_multinomial <- function(n, P, N_total) {
  N <- array(dim=c(n, dim(P)), dimnames=c(list(NULL), dimnames(P)))
  for (i in 1:nrow(P)) {
    v <- subset_vectors[[i]]
    N[, i, v] <- t(rmultinom(n, N_total[i], P[i, v]))
  }
  N
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
#' @return log likelihood or likelihood
#' @examples
#' u = c(1, 2, 3); n_objects=3; n_subsets=2^n_objects-1;
#' P = P_Luce(u)
#' N_total = vector(mode="integer", length=n_subsets)
#' N_total[singletons[1:3]] = 0
#' N_total[doubletons[1:3]] = 10
#' N_total[tripletons[1]] = 10
#' N = sim_DCE_multinomial(1, P, N_total) # Random count matrix
#' print(N[1,,], na.print='-') # Print first drawn count matrix
#' log_L_DCE_multinomial(P, N[1,,], log=TRUE) # Evaluate log liklihood
#' @seealso \code{\link{sim_DCE_multinomial}} which simulates a count matrix under the
#' model, given the total number of trials for each choice subset.
#' @importFrom stats dmultinom
#' @export
log_L_DCE_multinomial <- function(P, N, log=TRUE) {
  n_objects <- ncol(P)
  ln_L <- 0
  for (i in 1:nrow(P)) {
    if (subset_card[i] > 1) {
      v <- subset_vectors[[i]]
      ln_L <- ln_L + dmultinom(N[i, v], prob=P[i, v], log=TRUE)
    }
  }
  if (log) ln_L else exp(ln_L)
}

#' Simulation for Dirichlet-multinomial model
#'
#' \code{sim_Dir_mult} draws from a Dirichlet-multinomial distribution
#' @param n number of random vectors to draw
#' @param alpha vector of Dirichlet parameters
#' @param n_total total count over all categories
#' @return a \code{n} by \code{length(alpha)} matrix of counts.
#' @examples
#' n = sim_Dir_mult(10, c(2.4, 1.5, 3.2), 100)
#' @importFrom stats rmultinom
#' @export
sim_Dir_mult <- function(n, alpha, n_total) {
  N = matrix(nrow=n, ncol=length(alpha))
  for (d in 1:n) { # WJM: avoid loop?
    p <- rDirichlet(1, alpha)
    N[d, ] = rmultinom(1, n_total, p)
  }
  N
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
#' @return Likelihood or log likelihood
#' @examples
#' log_L_Dir_mult(c(2.4, 1.5, 3.2), c(45, 20, 33))
#' @export
log_L_Dir_mult <- function(alpha, n, log=TRUE) {
  # Compute prior and posterior normalization constants
  ln_prior_nc <- lgamma(sum(alpha)) - sum(lgamma(alpha))
  ln_post_nc <- lgamma(sum(alpha + n)) - sum(lgamma(alpha + n))
  ln_L <- ln_prior_nc - ln_post_nc
  if (log) ln_L else exp(ln_L)
}

#' Simulation of RCSs under a multiple Dirichlet distribution
#'
#' \code{sim_RCS_Dirichlet} draws RCSs from a multiple Dirichlet distribution.
#' @param n number of random choice structures (RCSs) to draw.
#' @param Alpha matrix of Dirichlet parameters, each row giving the Dirichlet
#' distribution of the corresponding row of an RCS.
#' @return a \code{n} by \code{nrow(Alpha)} by \code{ncol{Alpha}} array.
#' If the returned matrix is \code{P}, \code{P[i,,]} is the \eqn{i}'th draw
#' of a RCS.
#' @examples
#' P <- sim_RCS_Dirichlet(10, RCS_uniform_prior(3)) # 10 draws, 3 objects in universe
#' @export
sim_RCS_Dirichlet <- function(n, Alpha) {
  P <- array(dim=c(n, dim(Alpha)), dimnames=c(list(NULL), dimnames(Alpha)))
  for (i in 1:nrow(Alpha)) {
    v <- subset_vectors[[i]]
    if (subset_card[i] > 1) {
      P[, i, v] <- rDirichlet(n, Alpha[i, v])
    } else {
      P[, i, v] <- 1.0
    }
  }
  P
}

#' Simulation of DCE, multiple Dirichlet-multinomial model
#'
#' \code{sim_DCE_Dir_mult} draws random counts given a multiple Dirichlet
#' prior distribution over the probabilities in an RCS.
#' Total counts for each choice subset are specified in the vector
#' \code{N_total}.
#' @param n number of count matrices to draw
#' @param Alpha matrix of Dirichlet parameters, each row giving the Dirichlet
#' distribution of the corresponding row of a random choice structure.
#' @param N_total vector containing count totals (i.e. number of trials) for
#' each subset of the universe of choice objects.
#' @return a random count matrix from the multiple Dirichlet-multinomial model
#' @examples
#' u <- c(2.2, 1.5, 3.3); n_objects = length(u); n_subsets=2^n_objects-1
#' Alpha <- RCS_vector_alpha_prior(5.0, u)
#' N_total <- vector(mode="integer", length=n_subsets)
#' N_total[singletons[1:3]] = 0
#' N_total[doubletons[1:3]] = 10
#' N_total[tripletons[1]] = 10
#' N = sim_DCE_Dir_mult(10, Alpha, N_total)
#' @seealso \code{\link{log_L_DCE_Dir_mult}} for the likelihood function for
#' this model.
#' @importFrom stats rmultinom
#' @export
sim_DCE_Dir_mult <- function(n, Alpha, N_total) {
  N <- array(dim=c(n, dim(Alpha)), dimnames=c(list(NULL), dimnames(Alpha)))
  for (i in 1:nrow(Alpha)) {
    v <- subset_vectors[[i]]
    if (subset_card[i] > 1) {
      for (draw in 1:n) { # Need fresh draw of p, saving memory at cost of speed
        p <- rDirichlet(1, Alpha[i, v])
        N[draw, i, v] <- rmultinom(1, N_total[i], p)
      }
    } else {
      N[, i, v] <- N_total[i]
    }
  }
  N
}

#' Log likelihood for DCE, multiple Dirichlet-multinomial model
#'
#' \code{log_L_DCE_Dir_mult} computes the marginal likelihood for a model
#' where choice count vectors are independent multinomial across choice sets
#' and choice probability vectors are independent Dirichlet across choice sets.
#' @param Alpha matrix of Dirichlet parameters, each row giving the Dirichlet
#' distribution of the corresponding row of a random choice structure.
#' @param N count matrix with the same dimensions as \code{Alpha}, pertaining to the same
#' universe of objects.
#' @param log logical; if TRUE, return the log Bayes factor
#' @return Likelihood or log likelihood value
#' @examples
#' Alpha = RCS_scalar_alpha_prior(2.0, 3)
#' N = T_1972_counts['Dots', 6,,]
#' log_L_DCE_Dir_mult(Alpha, N)
#' @seealso \code{\link{sim_DCE_Dir_mult}}, which simulates a count matrix under
#' this model, given the total number of trials for each choice subset.
#' @export
log_L_DCE_Dir_mult <- function(Alpha, N, log=TRUE) {
  ln_L <- 0
  for (i in 1:nrow(Alpha)) {
    if (subset_card[i] > 1) {
      v <- subset_vectors[[i]]
      ln_L <- ln_L + log_L_Dir_mult(Alpha[i, v], N[i, v], log=TRUE)
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
#' @examples
#' P = P_uniform(3)
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
#' @examples
#' P = P_Luce(c(1.2,2.5,3.3))
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
#' @return a matrix of Dirichlet parameters all set to one, so that all choice
#' probability vectors are uniformly distributed.
#' @examples
#' Alpha <- RCS_uniform_prior(3)
#' P <- sim_RCS_Dirichlet(10, Alpha)
#' @export
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
#' @return a matrix of Dirichlet parameters for the scalar alpha prior
#' @examples
#' Alpha <- RCS_scalar_alpha_prior(2.5, 3)
#' P <- sim_RCS_Dirichlet(10, Alpha)
#' @export
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
#' @return a matrix of Dirichlet parameters for the vector Dirichlet prior.
#' @examples
#' Alpha <- RCS_vector_alpha_prior(5.2, c(2.5, 1.2, 3.3))
#' P <- sim_RCS_Dirichlet(10, Alpha)
#' @export
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
