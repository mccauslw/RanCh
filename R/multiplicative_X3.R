#' Compute a cross section of the multiplicative inequality region
#'
#' \code{multiplicative_X3} computes the region (a triangle) of ternary
#' probabilities consistent with given binary probabilities and the
#' multiplicative inequality.
#' @param P A random choice structure
#' @return A 3x3 matrix where each row gives one of the three vertices, in
#' barycentric coordinates, of the triangular region where the multiplicative
#' inequality holds.
#' @export
#' @examples
#' P = create_P3(0.7, 0.6, 0.8, 0.6, 0.3, 0.1, names = c('x', 'y', 'z'))
#' multiplicative_X3(P)
multiplicative_X3 <- function(P) {
  P_min = c(P[3, 1] * P[5, 1], P[3, 2] * P[6, 2], P[5, 3] * P[6, 3])
  multiplicative = matrix(c(
    P_min[1], P_min[1], 1-P_min[2]-P_min[3],
    P_min[2], 1-P_min[1]-P_min[3], P_min[2],
    1-P_min[1]-P_min[2], P_min[3], P_min[3]),
    nrow=3, ncol=3, byrow=FALSE)
}
