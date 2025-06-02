#' Vectorize a choice count matrix
#'
#' Create a list with two vectors of choice counts
#'
#' @inheritParams compute_pi_ln_like
#' @param choice_counts matrix of choice counts indexed by choice set (row) and
#'        choice object (column)
#'
#' @return List with the elements
#' \describe{
#'   \item{by_Ax}{Vector of choice counts, indexed by Ax, a flat index coding
#'   both choice set and choice object}
#'   \item{by_A}{Vector of total choice counts, indexed by choice set A
#'   (aggregates by_Ax)}
#' }
#' @export
#'
#' @examples
#' n <- 5
#' u <- create_universe(n)
#' N <- vectorize_counts(u, RanCh::MMS_2019_counts[1, , ])
#'
vectorize_counts <- function(u, choice_counts) {
  N <- list(by_Ax = rep.int(0, u$n_probs),
            by_A = rep.int(0, u$n_subsets))
  for (Ax in 1:u$n_probs) {
    A <- u$Ax_table[Ax,'A']
    x <- u$Ax_table[Ax,'x']
    N$by_Ax[Ax] <- choice_counts[A, x]
    N$by_A[A] <- N$by_A[A] + choice_counts[A, x]
  }
  names(N$by_Ax) <- u$Ax_strings
  names(N$by_A) <- u$A_strings
  N
}

#' Vectorize a matrix indexed by set A and object x from universe u
#'
#' Create a flat vector without NA values and values for singleton choice sets.
#'
#' @inheritParams compute_pi_ln_like
#' @param Ax_array A matrix indexed by set A and object x from universe u.
#'
#' @return Vector with non-degenerate elements
#' @export
#'
#' @examples
#' n <- 5
#' u <- create_universe(n)
#' Nv <- vectorize(u, RanCh::MMS_2019_counts[1, , ])
#'
vectorize <- function(u, Ax_array) {
  dims <- dim(Ax_array)
  d1 <- dims[1]
  d2 <- dims[2]
  slice_dims <- dims[-(1:2)]
  n_slices <- prod(slice_dims)

  Ax_matrix <- matrix(Ax_array, nrow = d1*d2, ncol = n_slices)
  result <- vapply(seq_len(n_slices),
                   function(i) Ax_matrix[u$Ax_table[,3], i],
                   numeric(u$n_probs))
  dim(result) <- c(u$n_probs, slice_dims)
  result
}

#' Vectorize a matrix indexed by set A and object x from universe u
#'
#' Create a flat vector without NA values and values for singleton choice sets.
#'
#' @inheritParams compute_pi_ln_like
#' @param Ax_vector A vector indexed by a flat Ax index
#'
#' @return Matrix indexed by set A and object x
#' @export
#'
#' @examples
#' n <- 5
#' u <- create_universe(n)
#' Nv <- vectorize(u, RanCh::MMS_2019_counts[1, , ])
#' N <- unvectorize(u, Nv)
#'
unvectorize <- function(u, Ax_vector) {
  d1 <- u$n_subsets
  d2 <- u$n
  slice_dims <- dim(Ax_vector)[-1]
  n_slices <- prod(slice_dims)

  # Flatten Ax_vector and create full matrix
  Ax_vector_flat <- matrix(Ax_vector, nrow = u$n_probs, ncol = n_slices)
  result <- matrix(NA, nrow = d1 * d2, ncol = n_slices)

  # Insert values at indexed positions
  result[u$Ax_table[,3], ] <- Ax_vector_flat
  result[u$singleton_is, ] <- 1.0

  # Reshape back to full array
  result <- array(result, dim = c(d1, d2, slice_dims))
  result
}
