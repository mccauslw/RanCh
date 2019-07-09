#' Compute a cross section of the regularity region
#'
#' \code{regularity_X3} computes the region (a triangle or the empty set) of
#' ternary probabilities consistent with given binary probabilities and the
#' regularity condition.
#' @param P A random choice structure.
#' @return If the region is empty, the output is NULL. Otherwise, a 3x3 matrix
#'   where each row gives one of the three vertices in barycentric coordinates.
#' @export
#' @keywords axioms regions
#' @examples
#' P = create_P(0.7, 0.6, 0.8, 0.6, 0.3, 0.1, names = c('x', 'y', 'z'))
#' reg_region = regularity_X3(P)
regularity_X3 <- function(P) {
  P_max = c(min(P[3, 1], P[5, 1]), min(P[3, 2], P[6, 2]), min(P[5, 3], P[6, 3]))
  if (sum(P_max) < 1) {
    regularity = NULL
  } else {
    regularity = matrix(c(
      P_max[1], P_max[1], 1-P_max[2]-P_max[3],  # 1st barycentric coordinate of three vertices
      P_max[2], 1-P_max[1]-P_max[3], P_max[2],  # 2nd
      1-P_max[1]-P_max[2], P_max[3], P_max[3]), # 3rd
      nrow=3, ncol=3, byrow=FALSE)
  }
}
