#' Compute set index
#'
#' \code{set_index} takes a vector \code{v} of object indices and constructs a
#' set index, an index for the set of objects whose indices are elements of \code{v}.
#' Each digit (or bit) in the binary representation of the resulting set index is
#' an inclusion indicator: object \eqn{i} is in the set if and only if the \eqn{i}'th
#' digit from the right is 1.
#' For example, the set with objects 1, 3, and 4 has index
#' equal to binary 1101, or decimal 13.
#' The set with elements 1, 3 and 4 can be specified as the input vector c(1,3,4) or
#' any permutation thereof, such as c(3,4,1).
#'
#' Note that the singleton set with object \eqn{i} is represented as \eqn{2^{i-1}}.
#' The bitwise "or" (\code{bitwOr}) of the set indices of two sets is the set
#' index of their union; the bitwise "and" (\code{bitwAnd}), the set index
#' of their intersection.
#'
#' @param v vector of object indices, in any order
#' @return An integer index corresponding to the set of objects whose indices are
#' elements of \code{v}.
#' @export
#' @examples
#' A <- set_index(c(1,3,4)) # Returns (decimal) 13, equal to binary 1101.
set_index <- function(v) {
  set <- sum(bitwShiftL(1, v-1))
}

#' Compute menu names given object names
#'
#' @param obj_names vector of object names
#'
#' @returns a vector of menu names
#' @export
#'
#' @examples
#' menus_as_strings <- menu_names(c('a', 'b', 'c'))
menu_names <- function(obj_names) {
  n <- length(obj_names)
  menu_names <- vector(mode='character', length=u_const$n_subsets[n])
  for (menu in seq_along(menu_names)) {
    name <- ''
    for (i in seq(n)) {
      if (bitwAnd(menu, bitwShiftL(1, i-1))) {
        name <- paste(name, obj_names[i], sep='')
      }
    }
    menu_names[menu] <- name
  }
  menu_names
}
