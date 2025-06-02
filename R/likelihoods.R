## Density and random generation for distributions of the RC model P and
## for distributions of RC count data N

#' Random generation of RC count data from a multiple multinomial model
#'
#' code{rmultinomDC} draws random counts given probabilities in a RCS \code{P}
#' and numbers of trials for each menu in the vector \code{N_total}.
#' Counts are independent across menus. Counts for each menu A are multinomial,
#' with number of trials N_total[A] and probability vector P[A,].
#'
#' @param n_draws number of draws
#' @param P matrix containing a random choice (RC) model
#' @param N_total vector containing numbers of trials, by menu.
#' @return a random count matrix (if \code{n_draws} = 1) or array (if \code{n_draws} > 1).
#' The first two dimensions are indexed by choice set A and object x.
#' @examples
#' u = c(1, 2, 3); n_objects=3; n_subsets=2^n_objects-1;
#' P = P_Luce(u)
#' N_total = vector(mode="integer", length=n_subsets)
#' N_total[singletons[1:3]] = 0
#' N_total[doubletons[1:3]] = 10
#' N_total[tripletons[1]] = 10
#' N = rmultinomDC(5, P, N_total)
#' print(N[, , 1], na.print='-') # Print first count matrix
#' dmultinomDC(P, N[, , 1], categorical=TRUE, log=TRUE)
#' @seealso \code{\link{dmultinomDC}}, which computes the density for this distribution.
#' @importFrom stats rmultinom
#' @export
rmultinomDC <- function(n_draws, P, N_total) {
  stopifnot(nrow(P)==length(N_total))
  N <- array(dim=c(dim(P), n_draws), dimnames=c(dimnames(P), list(NULL)))
  for (A in seq_len(nrow(P))) {
    v <- subset_vectors[[A]]
    N[A, v, ] <- rmultinom(n_draws, N_total[A], P[A, v])
  }
  N
}

#' Density of RC count data from a multiple multinomial model
#'
#' \code{dmultinomDC} evaluates the density of a count matrix given probabilities
#' in a RC model \code{P}.
#' @param P matrix containing a random choice (RC) model
#' @param N A count matrix or 3D array (multiple observations).
#' The first two dimensions are indexed by menu A and object x.
#' @param categorical logical; if \code{TRUE}, the likelihood is the for
#' the sequence of responses (categorical distribution) rather than for
#' the counts (multinomial distribution).
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
#' N = rmultinomDC(1, P, N_total) # Random count matrix
#' print(N[, , 1], na.print='-') # Print first count matrix
#' dmultinomDC(P, N[, , 1], categorical=FALSE, log=TRUE)
#' dmultinomDC(P, N[, , 1], categorical=TRUE, log=TRUE)
#' @seealso \code{\link{rmultinomDC}} which generates a random count matrix
#' under the multiple multinomial distribution.
#' @importFrom stats dmultinom
#' @export
dmultinomDC <- function(P, N, categorical=FALSE, log=TRUE) {
  stopifnot(identical(dim(P), dim(N)[1:2]))
  if (is.matrix(N)) dim(N) <- c(dim(N), 1)
  n_objects <- ncol(P)
  ln_L <- 0
  for (i in seq_len(dim(N)[3])) {
    for (A in 1:nrow(P)) {
      if (subset_card[A] > 1) {
        v <- subset_vectors[[A]]
        if (categorical)
          ln_L <- ln_L + sum(N[A, v, i] * log(P[A, v]))
        else
          ln_L <- ln_L + dmultinom(N[A, v, i], prob=P[A, v], log=TRUE)
      }
    }
  }
  if (log) ln_L else exp(ln_L)
}

#' Random generation for the Dirichlet-multinomial distribution
#'
#' \code{rDirMultinom} draws from a Dirichlet-multinomial distribution
#' @param n_draws number of random vectors to draw
#' @param alpha vector of Dirichlet parameters
#' @param n_trials number of trials
#' @return a \code{n} by \code{length(alpha)} matrix of counts.
#' @examples
#' n = rDirMultinom(10, c(2.4, 1.5, 3.2), 100)
#' @importFrom stats rmultinom
#' @export
rDirMultinom <- function(n_draws, alpha, n_trials) {
  N = matrix(nrow=length(alpha), ncol=n_draws)
  for (d in 1:n_draws) { # WJM: avoid loop?
    p <- rDirichlet(1, alpha)
    N[, d] = rmultinom(1, n_trials, p)
  }
  N
}

#' Density for the Dirichlet-multinomial distribution
#'
#' \code{dDirMultinom} evaluates the density for the Dirichlet-multinomial
#' distribution.
#' @param alpha vector of Dirichlet parameters
#' @param n vector of multinomial counts
#' @param categorical logical; if \code{TRUE}, the likelihood is the for
#' the sequence of responses (categorical distribution) rather than for
#' the counts (multinomial distribution).
#' @param log logical; if \code{TRUE}, return the log likelihood;
#' if \code{FALSE}, the likelihood.
#' \code{log=FALSE} is usually not recommendend, as underflow is likely.
#' @return Likelihood or log likelihood
#' @examples
#' dDirMultinom(c(2.4, 1.5, 3.2), c(45, 20, 33))
#' @export
dDirMultinom <- function(alpha, N, categorical=FALSE, log=TRUE) {
  if (is.vector(N)) N = matrix(N, ncol=1)
  stopifnot(length(alpha) == nrow(N))
  ln_L <- 0
  for (i in seq_len(ncol(N))) {
    # Compute prior and posterior normalization constants
    ln_prior_nc <- lgamma(sum(alpha)) - sum(lgamma(alpha))
    ln_post_nc <- lgamma(sum(alpha + N)) - sum(lgamma(alpha + N))
    ln_L <- ln_L + ln_prior_nc - ln_post_nc
    if (!categorical)
      ln_L <- ln_L + lfactorial(sum(N)) - sum(lfactorial(N))
  }
  if (log) ln_L else exp(ln_L)
}

#' Simulation of RC models under a multiple Dirichlet distribution
#'
#' \code{dDirichletRC} draws RC models from a multiple Dirichlet distribution.
#' @param n_draws number of draws to generate
#' @param Alpha matrix of Dirichlet parameters, each row giving the Dirichlet
#' distribution of the corresponding row of an RCS.
#' @return a \code{nrow(Alpha)} by \code{ncol{Alpha}} by \code{n} array.
#' If the returned matrix is \code{P}, \code{P[, , i]} is the \eqn{i}'th draw
#' of a RCS.
#' @examples
#' Alpha = RCS_uniform_prior(3)
#' P <- dDirichlet(10, Alpha) # 10 draws, 3 objects in universe
#' @export
dDirichletRC <- function(n_draws, Alpha) {
  P <- array(dim=c(dim(Alpha), n), dimnames=c(dimnames(Alpha), list(NULL)))
  for (A in 1:nrow(Alpha)) {
    x <- subset_vectors[[A]]
    if (subset_card[A] > 1) {
      P[A, x, ] <- t(rDirichlet(n, Alpha[A, x]))
    } else {
      P[A, x, ] <- 1.0
    }
  }
  P
}

#' Simulation of RC experimental data, multiple Dirichlet-multinomial model
#'
#' \code{rDirMultinomRC} draws random counts given a multiple Dirichlet
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
#' N = rDirMultinomRC(10, Alpha, N_total)
#' @seealso \code{\link{dDirMultinomRC}} for the likelihood function for
#' this model.
#' @importFrom stats rmultinom
#' @export
rDirMultinomRC <- function(n, Alpha, N_total) {
  stopifnot(nrow(Alpha)==length(N_total))
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
#' \code{dDirMultinomRC} computes the marginal likelihood for a model
#' where choice count vectors are independent multinomial across choice sets
#' and choice probability vectors are independent Dirichlet across choice sets.
#' @param Alpha matrix of Dirichlet parameters, each row giving the Dirichlet
#' distribution of the corresponding row of a random choice structure.
#' @param N count matrix with the same dimensions as \code{Alpha}, pertaining to the same
#' universe of objects.
#' @param categorical logical; if \code{TRUE}, the likelihood is the for
#' the sequence of responses (categorical distribution) rather than for
#' the counts (multinomial distribution).
#' @param log logical; if TRUE, return the log Bayes factor
#' @return Likelihood or log likelihood value
#' @examples
#' Alpha = RCS_scalar_alpha_prior(2.0, 3)
#' N = T_1972_counts['Dots', 6,,]
#' dDirMultinomRC(Alpha, N)
#' @seealso \code{\link{rDirMultinomRC}}, which simulates a count matrix under
#' this model, given the total number of trials for each choice subset.
#' @export
dDirMultinomRC <- function(Alpha, N, categorical=FALSE, log=TRUE) {
  stopifnot(identical(dim(Alpha), dim(N)))
  ln_L <- 0
  for (i in 1:nrow(Alpha)) {
    if (subset_card[i] > 1) {
      v <- subset_vectors[[i]]
      ln_L <- ln_L + dDirMultinom(Alpha[i, v], N[i, v], categorical, log=TRUE)
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
}Dir

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

#' Random Choice Structure from count proportions
#'
#' \code{proportions} takes a count matrix as input, and returns choice
#' proportions as a random choice structure.
#'
#' @param N A count matrix.
#' @return A random choice structure.
#' @export
#' @examples
#' P <- proportions(MMS_2019_counts[1,,])
proportions <- function(N) {
  n_objects <- ncol(N)
  P <- N/rowSums(N, na.rm = TRUE)
  for (i in 1:n_objects)
    P[singletons[i], i] <- 1.0
  P
}

## Basic construction of Dirichlet shape parameters for all menus of a universe

#' Dirichlet shape parameters for all menus of a universe, with constant values
#'
#' \code{DirRC_constant_shape} returns a matrix of Dirichlet shape
#' parameters where all individual shape parameters have the same value
#' \code{alpha}, within and across menus.
#' @param n_objects number of objects in the universe.
#' @return a matrix of Dirichlet shape parameters with all values set to
#' \code{alpha}. If \code{alpha} is one, the Dirichlet distribution on each
#' choice probability vector coincides with the uniform distribution.
#' @examples
#' Alpha <- DirDC_constant_shape(3)
#' P <- rDirichlet_DC(10, Alpha)
#' @export
DirDC_shape_constant_value <- function(n_objects, alpha, name_source = NULL) {
  n_subsets <- 2^n_objects-1
  Alpha <- alpha * member_table[1:n_subsets, 1:n_objects]
  dimnames(Alpha) <- copy_A_x_names(name_source, n_subsets, n_objects)
  Alpha
}

#' Dirichlet shape parameters for all menus of a universe, with constant menu sums
#'
#' \code{DirRC_constant_sum} returns a matrix of Dirichlet shape parameters
#' for a vector Dirichlet prior for a random choice structure.
#' @param alpha_sum scalar, sum of shape parameters, constant over all menus
#' @param v weights_spec, either a vector of object weights or the number of
#' equal weights. Gives the
#' @return a matrix of Dirichlet shape parameters where each row A gives
#' the parameter vector for menu A.
#' @examples
#' Alpha <- DirDC_constant_sum(5.2, c(2.5, 1.2, 3.3))
#' P <- rDirichlet_DC(10, Alpha)
#' @export
DirDC_constant_sum <- function(weights_spec, alpha_sum, name_source = NULL) {

  stopifnot(is.numeric(weights_spec))
  if (length(weights_spec)==1) weights_spec <- rep(1.0, weights_spec)

  n_objects <- length(weights_spec)
  n_subsets <- 2^n_objects-1
  Alpha = matrix(0, nrow=n_subsets, ncol=n_objects)
  for (A in 1:n_subsets) {
    den <- sum(weights_spec[subset_vectors[[A]]])
    Alpha[A, subset_vectors[[A]]] = alpha_sum * weights_spec[subset_vectors[[A]]]/den
  }
  Alpha <- Alpha * member_table[1:n_subsets, 1:n_objects]
  dimnames(Alpha) <- copy_A_x_names(name_source, n_subsets, n_objects)
  Alpha
}
