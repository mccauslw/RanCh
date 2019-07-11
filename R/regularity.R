#' Check if random choice structure satisfies regularity
#'
#' \code{regularity} returns TRUE if the random choice structure P satisfies
#' regularity and FALSE otherwise.
#' @param P A random choice structure
#' @return A logical value indicating whether \code{P} satisfies regularity.
#' @export
#' @examples
#' P = create_P3(0.7, 0.6, 0.8, 0.6, 0.3, names = c('x', 'y', 'z'))
#' regularity(P)
#'
regularity = function(P) {
  n = ncol(P)    # Number of objects in master set
  n.sets = 2^n-1 # Number of subsets of {1,...,n}, excluding empty set
  for (A in 1:n.sets) { # Run through all non-empty subsets A of {1,...,n}
    for (a in 1:n) {  # Run through all elements of {1,...,n}
      Sa = bitwShiftL(1, a-1)   # Sa is {a}, the singleton set containing a
      if (bitwAnd(Sa, A) > 0) { # Make sure a is in A
        for (b in 1:n) { # Run through all elements of {1,...,n}
          Sb = 2^(b-1) # Sb is {b}
          if (bitwAnd(Sb, A) > 0 && (a!=b)) { # Make sure b is in A, is not a
            if (P[A,a] > P[A-Sb,a]) return(FALSE)
          }
        }
      }
    }
  }
  return(TRUE)
}
