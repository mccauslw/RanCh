#' Random Choice Structure from count proportions
#'
#' \code{proportions} takes a count matrix as input, and returns choice
#' proportions as a random choice structure.
#'
#' @param N A count matrix.
#' @return A random choice structure.
#' @export
#' @examples
#' PC_P = proportions(PC_counts[1,,])
proportions <- function(N) {
  n_objects = ncol(N)
  P = N/rowSums(N, na.rm = TRUE)
  for (i in 1:n_objects)
    P[singletons[i], i] = 1.0
  P
}
