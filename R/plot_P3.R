#' Plot a Random Choice Structure in barycentric coordinates
#'
#' \code{plot_P3} plots four points specifying a Random Choice Structure for
#' a universe of three objects.
#' @param P A random choice structure for a universe of three objects
#' @param perm A permutation of (1, 2, 3) specifying which objects in the universe
#' correspond to the bottom left, top, and bottom right vertex, respectively of
#' the ternary plot.
#' @param binary_pch Plotting character (pch) for binary choice probabilities. Defaults
#' to a hollow circle.
#' @param ternary_pch Plotting character (pch) for ternary choice probability. Defaults
#' to a solid circle. The convention established with the defaults for binary_pch and
#' ternary_pch allow one to distinguish between a binary choice probability and a ternary
#' choice probability that happens to be on the boundary of the triangle.
#' @export
#' @examples
#' P = create_P(0.7, 0.6, 0.8, 0.6, 0.3, 0.1, names = c('x', 'y', 'z'))
#' plot_P3(P)
plot_P3 = function(P, perm=c(1, 2, 3), binary_pch = 1, ternary_pch = 20) {
  # Object and subset shortcuts
  x = perm[1]; y = perm[2]; z = perm[3]
  xy = sum(bitShiftL(1, c(x, y)-1))
  yz = sum(bitShiftL(1, c(y, z)-1))
  xz = sum(bitShiftL(1, c(x, z)-1))
  xyz = 7

  # Sides of triangle
  tripoints(P[xy, x], P[xy, y], 0.0, pch=binary_pch)
  tripoints(0.0, P[yz, y], P[yz, z], pch=binary_pch)
  tripoints(P[xz, x], 0.0, P[xz, z], pch=binary_pch)

  # Centre of triangle
  tripoints(P[xyz, x], P[xyz, y], P[xyz, z], pch=ternary_pch)
}
