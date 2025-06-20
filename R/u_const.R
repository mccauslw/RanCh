#' Internal constants for universe construction
#'
#' A named list of constants (scalars, vectors, and arrays) used throughout the RanCh package.
#'
#' @name universe_constants
#' @aliases u_const
#' @format A named list of scalars, vectors and arrays.
#' @details
#' \describe{
#'   \item{n_subsets}{Vector giving the number of non-empty subsets for each set size}
#'   \item{n_orders}{Vector giving the number of preference orders for each set size}
#'   \item{n_Ax}{Vector giving the number of Ax indices for each set size.}
#'   \item{object_names}{Vector of default object names.}
#'   \item{subset_names}{Vector of default subset names.}
#'   \item{subset_card}{Cardinality of each subset.}
#'   \item{subset_vectors}{List giving, for each subset, a vector of its elements.}
#'   \item{n_singletons}{Number of singleton subsets, for each set size}
#'   \item{singletons}{Vector of singleton subsets.}
#'   \item{singleton_names}{Names of singleton subsets.}
#'   \item{n_doubletons}{Number of doubleton subsets, for each set size.}
#'   \item{doubletons}{Vector of doubleton subsets.}
#'   \item{doubleton_names}{Names of doubleton subsets.}
#'   \item{n_tripletons}{Number of tripleton subsets, for each set size.}
#'   \item{tripletons}{Vector of tripleton subsets.}
#'   \item{tripleton_names}{Names of tripleton subsets.}
#'   \item{RP_table}{Array of revealed preference indicators.}
#'   \item{member_table}{Matrix of set membership indicators}
#' }
#'
#' The value at index (subset, object, doubleton) of RP_table is 1, 0, or -1
#'   1 if object is first object in the doubleton set, which is a subset of subset.
#'  -1 if object is second object in the doubleton set, which is a subset of subset.
#'   0 otherwise
#' Names correspond to \code{\link{object_names}}
#'
#' Value at index (subset, object) of member_table is 1 if object is an element of subset, NA otherwise
#'
#' The object is stored in `u_const`, and is not exported. To access it manually:
#' \code{RanCh:::u_const}
#'
#' @keywords internal
NULL
