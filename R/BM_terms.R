#' Helper function for random_utility and BM_terms, below.
#'
#' \code{ru} tests whether or not a random choice structure satisfies random utility
#' @param P matrix, a random choice structure
#' @param compute_Q, a logical value indicating whether or not to compute
#' \eqn{Q}, the matrix of Block-Marschak terms corresponding to \code{P}.
#' If compute_Q is true, output is a list with elements Q and is_ru, where is_ru is a
#' boolean value that is TRUE or FALSE according to whether or not P satisfies random utility
#' If compute_Q is FALSE, output is TRUE or FALSE according to whether or not P satisfies
#' random utility.
#' @return A logical value indicating whether \code{P} satisfies random utility.
ru <- function(P, compute_Q) {
  n <- ncol(P)
  n_sets <- bitwShiftL(1, n)-1 # Number of subsets of {1,...,n}, excluding empty set
  if (compute_Q) {
    is_ru <- TRUE
    Q <- P*0 # Inherit dimension, row and column names from P, set to zero
  }

  # Precompute the cardinality of all subsets of {1,...,n}
  card <- vector('integer', n_sets)
  for (A in 1:n_sets) {
    for (S in bitwShiftL(1, 0:(n-1))) {
      if (bitwAnd(S, A) > 0) {
        card[A] <- card[A]+1
      }
    }
  }

  for (T in 1:n_sets) { # Run through all non-empty subsets T of {1,...,n}
    for (s in 1:n) {    # Run through all element of {1,...,n}
      Ss <- bitwShiftL(1, s-1)   # Ss is {s}, the singleton set containing s
      if (bitwAnd(Ss, T) > 0) { # Make sure s is in T
        BM <- 0.0       # Initialize Block-Marshak polynomial for (s, T) to 0
        for (U in T:n_sets) { # Run through supersets U of T
          if (bitwAnd(U, T) == T) { # Make sure U is indeed a superset
            if (U==T || (card[U-T] %% 2 == 0)) {
              BM <- BM + P[U, s] # Cardinality of U\T is even
            }
            else {
              BM <- BM - P[U, s] # Cardinality of U\T is odd
            }
          }
        }
        if (compute_Q) {
          Q[T, s] <- BM
          if (BM < 0.0) is_ru <- FALSE
        }
        else {
          if (BM < 0.0) return(FALSE) # Violation of random utility found
        }
      }
    }
  }
  if (compute_Q) list(Q=Q, is_ru=is_ru) else TRUE
}

#' Check if random choice structure satisfies random utility
#'
#' \code{random_utility} returns TRUE if the random choice structure P satisfies
#' random utility and FALSE otherwise.
#' @param P A random choice structure
#' @return A logical value indicating whether \code{P} satisfies random utility.
#' @export
#' @examples
#' P <- create_P3(0.7, 0.6, 0.8, 0.6, 0.3, names = c('x', 'y', 'z'))
#' random_utility(P)
#'
random_utility <- function(P) {
  ru(P, FALSE)
}

#' Compute Block-Marschack terms from a random choice structure
#'
#' \code{BM_terms} returns a matrix of the same dimensions as the input matrix, a
#' random choice structure \code{P}.
#' The non-NA terms are all non-negative if and only if the random choice structure
#' \code{P} satisfies random utility.
#' @param P A random choice structure. Element \code{P[A,x]} is the probability
#' that \eqn{x} is chosen when choice set \eqn{A} is presented.
#' @return A matrix of Block-Marschak terms.
#' If \code{Q} is the result, then for all random rankings of the elements of the
#' universe of objects than induce the random choice structure \eqn{P},
#' element \code{Q[A,x]} is the probability that \eqn{x} is ranked higher than
#' any other element of choice set \eqn{A} and lower than any element in the
#' complement of \eqn{A} in the universe of objects.
#' If \eqn{x \notin A}, then \code{Q[A,x]} has value \code{NA}.
#' The RCS \eqn{P} satisfies random utility if and only if for all
#' \eqn{x \in A \subseteq T}, \eqn{Q_A(x) \geq 0}
#' @export
#' @examples
#' P <- create_P3(0.7, 0.6, 0.8, 0.6, 0.3, names = c('x', 'y', 'z'))
#' Q <- BM_terms(P)
#' Q
BM_terms <- function(P) {
  ru(P, TRUE)$Q
}
