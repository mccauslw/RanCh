#' Permute the objects in an array indexed by set A and object x
#'
#' @param M_in A (2^n-1) x n matrix indexed by set A and object x
#' @param perm A vector consisting of the permutation of the integers 1,...,n
#'
#' @returns A (2^n-1) x n matrix indexed by set A and object x, containing the
#' same information as `M_in` but with objects permutated.
#' @export
#'
#' @examples
permute_A_x_matrix <- function(M_in, perm) {
  M_out <- M_in
  x_names_in <- colnames(M_in)
  x_names_out <- x_names_in[perm]
  A_names_out <- rownames(M_in)
  for (A_out in seq.int(nrow(M_in))) {
    v_out <- subset_vectors[[A_out]]
    v_in <- perm[v_out]
    A_in <- set_index(v_in)
    M_out[A_out, v_out] <- unname(M_in[A_in, v_in])
    A_names_out[A_out] <- paste0(x_names_in[v_in], collapse = "")
  }
  colnames(M_out) <- x_names_out
  rownames(M_out) <- A_names_out
  M_out
}
