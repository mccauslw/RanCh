#' Marginalize a count matrix or random choice structure
#'
#' This function takes as input a count matrix or random choice structure on a
#' universe of objects and returns a marginalization of it to a universe that is a
#' subset of the original universe.
#' @param input A count matrix or
#' @param objects A vector of objects to retain
#' @return A count matrix or RCS
#' @export
#' @examples
#' N_bce <- marginalize(PC_counts['Beer',,], c(2, 3, 5)) # Marginalize data first
#' P_bce_1 <- proportions(N_bce)                         # then compute proportions
#' P <- proportions(PC_counts['Beer',,])   # Compute proportions first
#' P_bce_2 <- marginalize(P, c(2, 3, 5))   # then marginalize. Gives same result.
marginalize <- function(input, objects) {
  d <- dim(input)
  n <- d[2]               # Number of objects in universe
  T <- 2^n-1              # Universe of objects in binary set notation
  input_subsets <- 1:T    # Vector of non-empty subsets of T
  A <- sum(2^(objects-1)) # User provided subset of objects to retain
  Ac <- T-A               # Complement of A in T

  # Form vector of those subsets of T with an empty intersection with Ac
  output_subsets <- input_subsets[bitwAnd(input_subsets, Ac) == 0]
  # Select elements of input
  output <- input[output_subsets, objects]
}
