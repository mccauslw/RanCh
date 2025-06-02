#' Copy subset and object names from an array or universe
#'
#' @param name_source an array whose first two dimensions are indexed by subset A and
#' object x
#' @param n_subsets expected number of subsets
#' @param n_objects expected number of objects
#'
#' @returns If names can be reliably copied from \code{name_source}, a list with two
#' elements, a vector of subset names and a vector of object names.
#' Otherwise, NULL
#' @export
#'
#' @examples
#' # Get names from an array with counts
#' N = RanCh::MMS_2019_counts[1, , ]
#' names_from_N <- copy_A_x_names(N, 31, 5)
#'
#' # Get names from a universe
#' u <- create_universe(5, letters[1:5])
#' names_from_u <- copy_A_x_names(N, 31, 5)
copy_A_x_names <- function(name_source, n_subsets, n_objects) {
  if (is.null(name_source)) return(name_source)
  if (is.array(name_source) &&
      dim(name_source)[1] == n_subsets && dim(name_source)[2] == n_objects)
    return(dimnames(name_source)[1:2])
  if (is.list(name_source) && !is.null(names(name_source)) &&
      all(c("A_strings", "object_names") %in% names(name_source)) &&
      length(name_source$A_strings) == n_subsets &&
      length(name_source$object_names) == n_objects)
    return(list(Subset=name_source$A_strings, Object=name_source$object_names))
  NULL
}

#' Copy names of (subset, object) pairs from a vector or array
#'
#' @param name_source a vector indexed by a flat Ax index encoding a pair (A, x)
#' of subset and object
#' @param n_probs expected length of vector
#'
#' @returns If names can be reliably copied from \code{name_source}, a vector of
#' names of (subset, object) pairs. Otherwise, NULL.
#' @export
#'
#' @examples
copy_Ax_names <- function(name_source, n_probs) {
  if (is.null(name_source)) return(name_source)
  if (is.vector(name_source) && length(name_source) == n_probs)
    return(names(name_source))
  if (is.array(name_source) && dim(name_source)[1] == n_probs)
    return(dimnames(name_source)[1])
  if (is.list(name_source) && !is.null(names(name_source)) &&
      ("A_strings" %in% names(name_source)) && length(name_source$Ax_strings) == n_probs)
    return(list(Ax = name_source$Ax_strings))
  NULL
}
