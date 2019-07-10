#' Compute set index
#'
#' \code{set_index} takes a vector of object indices and constructs a set index,
#' which is an index for the set of objects whose indices appear in the given vector.
#' Each digit (or bit) in the binary representation of the resulting set index is
#' an inclusion indicator: object i is in the set if and only if the i'th digit
#' from the right is 1. For example, the set with objects 1, 3, and 4 has index
#' equal to binary 1101, or decimal 13.
#' The set with elements 1, 3 and 4 can be specified as the input vector c(1,3,4) or
#' any permutation thereof, such as c(3,4,1).
#' @param v vector of object indices, in any order
#' @return a integer index corresponding to the set of objects whose indices are in \code{v}
#' @export
#' @details Note that the singleton set with object i is represented as 2^(i-1).
#' The bitwise "or" (\code{bitwAnd} in the package \code{bitops}) of the set indices of
#' two sets gives the set index of the union; the bitwise "and", the intersection.

#' @examples
#' A = set_index(1,3,4) # Returns (decimal) 13, equal to binary 1101.
set_index <- function(v) {
  set = sum(bitShiftL(1, v-1))
}
