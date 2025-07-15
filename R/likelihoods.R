## Basic construction of Dirichlet shape parameters for all menus of a universe

#' Dirichlet shape parameters for all menus of a universe, with constant values
#'
#' \code{DirRC_constant_shape} returns a matrix of Dirichlet shape
#' parameters where all individual shape parameters have the same value
#' \code{alpha}, within and across menus.
#' @param n_objects number of objects in the universe
#' @param alpha the common value of the shape parameters
#' @param name_source universe or matrix to extract object and menu notes from
#' @return a matrix of Dirichlet shape parameters with all values set to
#' \code{alpha}. If \code{alpha} is one, the Dirichlet distribution on each
#' choice probability vector coincides with the uniform distribution.
#' @examples
#' Alpha <- DirRC_constant_shape(3, 2.0)
#' P <- rDirichletRC(10, Alpha)
#' @export
DirRC_constant_shape <- function(n_objects, alpha, name_source = NULL) {
  n_subsets <- 2^n_objects-1
  Alpha <- alpha * u_const$member_table[1:n_subsets, 1:n_objects]
  dimnames(Alpha) <- copy_A_x_names(name_source, n_subsets, n_objects)
  Alpha
}

#' Dirichlet shape parameters for all menus of a universe, with constant menu sums
#'
#' \code{DirRC_constant_sum} returns a matrix of Dirichlet shape parameters
#' for a vector Dirichlet prior for a random choice structure.
#' @param weights_spec either a vector of object weights or the number of
#' equal weights. Shape parameters are proportional to these weights, menu by
#' menu.
#' @param alpha_sum scalar, sum of shape parameters, constant over all menus
#' @param name_source universe or matrix to extract object and menu names from
#' @return a matrix of Dirichlet shape parameters where each row A gives
#' the parameter vector for menu A.
#' @examples
#' Alpha <- DirRC_constant_sum(c(2.5, 1.2, 3.3), 5.2)
#' P <- rDirichletRC(10, Alpha)
#' @export
DirRC_constant_sum <- function(weights_spec, alpha_sum, name_source = NULL) {

  stopifnot(is.numeric(weights_spec))

  if (length(weights_spec)==1) weights_spec <- rep(1.0, weights_spec)

  n_objects <- length(weights_spec)
  n_subsets <- 2^n_objects-1
  Alpha = matrix(0, nrow=n_subsets, ncol=n_objects)
  for (A in 1:n_subsets) {
    den <- sum(weights_spec[u_const$subset_vectors[[A]]])
    Alpha[A, u_const$subset_vectors[[A]]] =
      alpha_sum * weights_spec[u_const$subset_vectors[[A]]]/den
  }
  Alpha <- Alpha * u_const$member_table[1:n_subsets, 1:n_objects]
  dimnames(Alpha) <- copy_A_x_names(name_source, n_subsets, n_objects)
  Alpha
}

## Basic RCS construction

#' Random Choice Structure with all probabilities discrete uniform
#'
#' \code{P_uniform} constructs a Random Choice Structure (RCS) for a given
#' universe size, where all choice probabilities are discrete uniform.
#' @param n_objects integer, number of objects in universe
#' @return RCS where all choice probabilities are discrete uniform
#' @examples
#' P <- P_uniform(3)
#' @export
P_uniform <- function(n_objects) {
  n_subsets <- 2^n_objects - 1
  P <- u_const$member_table[1:n_subsets, 1:n_objects]
  for (i in 1:n_subsets) {
    P[i, ] <- P[i, ]/u_const$subset_card[i]
  }
  P
}

#' Construct a Luce model
#'
#' \code{P_Luce} constructs a random choice structure (RCS) from a Luce
#' model.
#' @param v vector specifying non-negative Luce weights for a collection of
#' objects.
#' If \code{v} is named, the names are interpreted as the names of the objects,
#' and row and column names of the output will be set accordingly.
#' @returns A Random Choice Structure with choice probabilities set in proportion
#' to Luce weights.
#' @export
#' @examples
#' P <- P_Luce(c(1,2,3))
#' P <- P_Luce(c(a=1, b=2, c=3)) # Same, but sets object names
#' @seealso [P_logit()]
P_Luce <- function(v) {
  stopifnot(all(v >= 0))
  n_objects <- length(v)
  n_subsets <- 2^n_objects - 1
  P <- u_const$member_table[1:n_subsets, 1:n_objects]
  for (i in 1:n_subsets) {
    P[i, ] <- P[i, ] * v / sum(v * u_const$member_table[i, ], na.rm=TRUE)
  }
  if (!is.null(names(v))) {
    rownames(P) <- menu_names(names(v))
    colnames(P) <- names(v)
    names(dimnames(P)) <- c("Menu", "Object")
  }
  P
}

#' Construct a logit model
#'
#' \code{P_logit} constructs a random choice structure (RCS) for a logit model.
#' @param x vector of logit values for a collection of objects.
#' If \code{x} is named, the names are interpreted as the names of the objects,
#' and the row and column names of the output will be set accordingly.
#'
#' @returns A Random Choice Structure with choice probabilities set in
#' proportion to the exponential of the logit values.
#' @export
#'
#' @examples
#' P <- P_logit(c(-2, 0, 1))
#' P <- P_logit(c(a=-2, b=0, c=1)) # Same, but sets object names
#' @seealso [P_Luce()]
P_logit <- function(x) {
  P <- P_Luce(exp(x))
}

#' Random Choice Structure from count proportions
#'
#' \code{P_frequencies} takes a count matrix as input, and returns choice
#' frequencies as a random choice structure.
#'
#' @param N A count matrix.
#' @return A random choice structure with choice probabilities equal to
#' empirical choice frequencies
#' @export
#'
#' @examples
#' P <- P_frequencies(MMS_2019_counts[1,,])
P_frequencies <- function(N) {
  n_objects <- ncol(N)
  P <- N/rowSums(N, na.rm = TRUE)
  for (i in 1:n_objects)
    P[u_const$singletons[i], i] <- 1.0
  names(dimnames(P)) <- c("Menu", "Object")
  P
}

# Density and random generation for distributions of the RC model P and
## for distributions of RC count data N

#' Compute multinomial coefficient
#'
#' Given a vector of counts, compute the multinomial coefficient. This is used
#' to compute the repeated categorial or
#' @param counts vector of positive integer counts
#' @param log logical; if \code{TRUE}, return the log multinomial coefficient;
#' if \code{FALSE}, the multinomial coefficient.
#'
#' @returns the multinomial coefficient or the log multinomial coefficient
#'
multinomial_coef <- function(counts, log = TRUE) {
  total <- sum(counts)
  ln_coeff <- lgamma(total + 1) - sum(lgamma(counts + 1))
  if (log) ln_coeff else exp(ln_coeff)
}

#' Random generation of RC count data from a multiple multinomial model
#'
#' code{rmultinomRC} draws random counts given probabilities in a RCS \code{P}
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
#' v <- c(a=1, b=2, c=3)
#' n_objects <- length(v)
#' n_subsets <- 2^n_objects-1;
#' P <- P_Luce(v)
#' xy <- set_index(c(1, 2))      # Menu with x and y
#' xyz <- set_index(c(1, 2, 3))  # Menu with x, y, and z
#' N_total <- vector(mode="integer", length=n_subsets)
#' N_total[xy] <- 10    # Number of trials for menu with x and y
#' N_total[xyz] <- 10   # Number of trials for menu with x, y, and z
#' N <- rmultinomRC(5, P, N_total)
#' print(N[, , 1], na.print='-') # Print first count matrix
#' dmultinomRC(P, N[, , 1], categorical=TRUE, log=TRUE)
#' @seealso \code{\link{dmultinomRC}}, which computes the density for this distribution.
#' @importFrom stats rmultinom
#' @export
rmultinomRC <- function(n_draws, P, N_total) {
  stopifnot(nrow(P)==length(N_total))
  N <- array(dim=c(dim(P), n_draws), dimnames=c(dimnames(P), list(NULL)))
  for (A in seq_len(nrow(P))) {
    v <- u_const$subset_vectors[[A]]
    N[A, v, ] <- stats::rmultinom(n_draws, N_total[A], P[A, v])
  }
  N
}

#' Density of RC count data from a multiple multinomial model
#'
#' \code{dmultinomRC} evaluates the density of a count matrix given probabilities
#' in a RC model \code{P}.
#' @inheritParams rmultinomRC
#' @param N A count matrix or 3D array (multiple observations).
#' The first two dimensions are indexed by menu A and object x.
#' @param categorical logical; if \code{TRUE}, the likelihood is the for
#' the sequence of responses (categorical distribution) rather than for
#' the counts (multinomial distribution).
#' @param log logical; if \code{TRUE}, return the log likelihood;
#' if \code{FALSE}, the likelihood.
#' \code{log=FALSE} is usually not recommendend, as underflow is likely.
#' @return value of log density or density
#' @examples
#' v <- c(x=2, y=5, z=3) # Vector of Luce weights, names of objects are x, y, z
#' n <-length(v)         # Number of objects
#' n_subsets <- 2^n-1;   # Number of non-zero subsets of {x,y,z}
#' P <- P_Luce(v)        # Random choice model with given Luce weights
#' xy <- set_index(c(1, 2))      # Choice set with x and y
#' xyz <- set_index(c(1, 2, 3))  # Choice set with x, y, and z
#' N_total <- vector(mode="integer", length=n_subsets) # Count totals by menu
#' N_total[xy] <- 10     # Count total for menu {x, y}
#' N_total[xyz] <- 10.   # Count total for menu {x, y, z}
#' N <- rmultinomRC(1, P, N_total) # Random count matrix
#' print(N[, , 1], na.print='-') # Print first count matrix
#' dmultinomRC(P, N[, , 1], categorical=FALSE, log=TRUE)
#' dmultinomRC(P, N[, , 1], categorical=TRUE, log=TRUE)
#' @seealso \code{\link{rmultinomRC}} which generates a random count matrix
#' under the multiple multinomial distribution.
#' @importFrom stats dmultinom
#' @export
dmultinomRC <- function(P, N, categorical=FALSE, log=TRUE) {
  stopifnot(identical(dim(P), dim(N)[1:2]))
  if (is.matrix(N)) dim(N) <- c(dim(N), 1) # Coerce to 3-D array
  ln_L <- 0
  for (i in seq_len(dim(N)[3])) {
    for (A in 1:nrow(P)) {
      if (u_const$subset_card[A] > 1) {
        v <- u_const$subset_vectors[[A]]
        v <- v[N[A, v, i] != 0]
        if (length(v) > 0) {
          if (categorical)
            ln_L <- ln_L + sum(N[A, v, i] * log(P[A, v]))
          else
            ln_L <- ln_L + stats::dmultinom(N[A, v, i], prob=P[A, v], log=TRUE)
        }
      }
    }
  }
  if (log) ln_L else exp(ln_L)
}

#' Simulation of RC models under a multiple Dirichlet distribution
#'
#' \code{rDirichletRC} draws RC models from a multiple Dirichlet distribution.
#' @inheritParams rmultinomRC
#' @param Alpha matrix of Dirichlet parameters, each row giving the Dirichlet
#' shape parameter vector for a given menu
#' @return a \code{nrow(Alpha)} by \code{ncol{Alpha}} by \code{n_draws} array.
#' If the returned matrix is \code{P}, \code{P[, , i]} is the \eqn{i}'th draw
#' of a RCS.
#' @examples
#' Alpha <- DirRC_constant_shape(3, 1.0) # 3 objects in universe
#' P <- rDirichletRC(10, Alpha) # 10 draws
#' @importFrom extraDistr rdirichlet
#' @export
rDirichletRC <- function(n_draws, Alpha) {
  P <- array(dim=c(dim(Alpha), n_draws), dimnames=c(dimnames(Alpha), list(NULL)))
  for (A in 1:nrow(Alpha)) {
    x <- u_const$subset_vectors[[A]]
    if (u_const$subset_card[A] > 1) {
      P[A, x, ] <- t(extraDistr::rdirichlet(n_draws, Alpha[A, x]))
    } else {
      P[A, x, ] <- 1.0
    }
  }
  P
}

#' Density of RC models under a multiple Dirichlet distribution
#'
#' \code{dDirichletRC} evaluates the density of a multiple Dirichlet distribution.
#' distribution of the corresponding row of an RCS.
#' @inheritParams rDirichletRC
#' @inheritParams dmultinomRC
#' @inherit dmultinomRC return
#' @examples
#' Alpha = DirRC_constant_shape(3, 1.0)
#' P <- rDirichletRC(10, Alpha) # 10 draws, 3 objects in universe
#' ln_L <- dDirichletRC(Alpha, P)
#' @importFrom extraDistr ddirichlet
#' @export
dDirichletRC <- function(Alpha, P, log=TRUE) {
  ln_L <- 0
  for (A in 1:nrow(Alpha)) {
    x <- u_const$subset_vectors[[A]]
    if (u_const$subset_card[A] > 1) {
      ln_L <- ln_L + extraDistr::ddirichlet(t(P[A, x, ]), Alpha[A, x], log=TRUE)
    }
  }
  if (log) ln_L else exp(ln_L)
}

#' Simulation of RC experimental data, multiple Dirichlet-multinomial model
#'
#' \code{rDirMultinomRC} draws random counts given a multiple Dirichlet
#' prior distribution over the probabilities in an RCS.
#' Total counts for each choice subset are specified in the vector
#' \code{N_total}.
#' @param n_draws number of count matrices to draw
#' @param Alpha matrix of Dirichlet parameters, each row giving the Dirichlet
#' distribution of the corresponding row of a random choice structure.
#' @param N_total vector containing count totals (i.e. number of trials) for
#' each subset of the universe of choice objects.
#' @return a random count matrix from the multiple Dirichlet-multinomial model
#' @examples
#' v <- c(2.2, 1.5, 3.3)
#' n_objects <- length(v)
#' n_subsets <- 2^n_objects-1
#' Alpha <- DirRC_constant_sum(v, 5.0)
#' N_total <- vector(mode="integer", length=n_subsets)
#' N_total[u_const$.singletons[1:3]] = 0
#' N_total[u_const$.doubletons[1:3]] = 10
#' N_total[u_const$.tripletons[1]] = 10
#' N = rDirMultinomRC(10, Alpha, N_total)
#' @seealso \code{\link{dDirMultinomRC}} for the likelihood function for
#' this model.
#' @importFrom extraDistr rdirmnom
#' @export
rDirMultinomRC <- function(n_draws, Alpha, N_total) {
  stopifnot(nrow(Alpha)==length(N_total))
  N <- array(dim=c(dim(Alpha), n_draws), dimnames=c(dimnames(Alpha), list(NULL)))
  for (A in seq_len(nrow(Alpha))) {
    v <- u_const$subset_vectors[[A]]
    if (u_const$subset_card[A] > 1) {
      N[A, v, ] <- t(extraDistr::rdirmnom(n_draws, N_total[A], Alpha[A, v]))
    } else {
      N[A, v, ] <- N_total[A]
    }
  }
  N
}

#' Log likelihood for multiple Dirichlet-multinomial model
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
#' Alpha <- DirRC_constant_sum(3, 2.0)
#' N <- T_1972_counts['Dots', 6, , ]
#' dDirMultinomRC(Alpha, N)
#' @seealso \code{\link{rDirMultinomRC}}, which simulates a count matrix under
#' this model, given the total number of trials for each choice subset.
#' @importFrom extraDistr ddirmnom
#' @export
dDirMultinomRC <- function(Alpha, N, categorical=FALSE, log=TRUE) {
  stopifnot(identical(dim(Alpha), dim(N)[1:2]))
  if (is.matrix(N)) dim(N) <- c(dim(N), 1) # Coerce to 3-D array
  ln_L <- 0
  for (i in seq_len(dim(N)[3])) {
    for (A in 1:nrow(Alpha)) {
      if (u_const$subset_card[A] > 1) {
        v <- u_const$subset_vectors[[A]]
        ln_L <- ln_L + ddirmnom(N[A, v, i], sum(N[A, v, i]), Alpha[A, v], log=TRUE)
        if (categorical) {
          ln_L <- ln_L - multinomial_coef(N[A, v, i], log=TRUE)
        }
      }
    }
  }
  if (log) ln_L else exp(ln_L)
}
