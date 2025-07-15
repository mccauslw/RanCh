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
  slice_names <- dimnames(Ax_array)[-(1:2)]
  n_slices <- prod(slice_dims)

  Ax_matrix <- matrix(Ax_array, nrow = d1*d2, ncol = n_slices)
  result <- vapply(seq_len(n_slices),
                   function(i) Ax_matrix[u$Ax_table[,3], i],
                   numeric(u$n_probs))
  d <- c(u$n_probs, slice_dims)
  if (length(d) == 1) {
    result <- as.vector(result)
    names(result) <- u$Ax_strings
    return(result)
  }
  dimnames(result) <- c(list(Ax=u$Ax_strings), slice_names)
  if (length(d) == 2)
    return(as.matrix(result, nrow=d[1], ncol=d[2]))
  dim(result) <- d
}

#' Vectorize a matrix indexed by set A and object x from universe u
#'
#' Create a flat vector without NA values and values for singleton choice sets.
#'
#' @inheritParams compute_pi_ln_like
#' @param Ax_vector A vector indexed by a flat Ax index
#' @param singleton_value Value to insert for cases (A, x) where A = {x}. Should
#' be 1.0 for choice probabilities. Recommended value is 0 for count structures
#' and 1.0 for Dirichlet parameter structures.
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
unvectorize <- function(u, Ax_vector, singleton_value) {
  d1 <- u$n_subsets
  d2 <- u$n
  slice_dims <- dim(Ax_vector)[-1]
  slice_names <- dimnames(Ax_vector)[-1]
  n_slices <- prod(slice_dims)

  # Flatten Ax_vector and create full matrix
  Ax_vector_flat <- matrix(Ax_vector, nrow = u$n_probs, ncol = n_slices)
  result <- matrix(NA, nrow = d1 * d2, ncol = n_slices)

  # Insert values at indexed positions
  result[u$Ax_table[,3], ] <- Ax_vector_flat
  result[u$singleton_is, ] <- singleton_value

  # Reshape back to full array
  result <- array(result, dim = c(d1, d2, slice_dims))
  dimnames(result) <- c(list(Menu=u$A_strings, Object=u$object_names), slice_names)
  result
}
