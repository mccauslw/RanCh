#' Compute various cross sections of similarity effect regions
#'
#' \code{similarity_X3} computes six regions associated with similary effects
#' @param pxz Binary choice probability, where x is one of the similar objects and
#' $z$ is the dissimilar object
#' @param pyz Binary choice probability, where y is the other similar object and z
#' is the dissimilar object
#' @return A list of six regions in barycentric coordinates. Rows are vertices, columns give
#' ternary probabilities for objects x, y and z, respectively.
#' \describe{
#' \item{\code{Sxyz}}{region where there is a similarity effect with object x as target,
#' y as decoy and z as competitor}
#' \item{\code{Syxz}}{region where there is a similarity effect with object x as target,
#' y as decoy and z as competitor}
#' \item{\code{So}}{region where there is neither similarity effect}
#' \item{\code{Sx}}{region with only the x effect}
#' \item{\code{Sy}}{region with only the y effect}
#' \item{\code{Sxy}}{region with both effects}
#' }
#' @export
#' @examples
#' S = similarity_X3(0.5, 0.6)
similarity_X3 <- function(pxz, pyz) {
  # Vertices of barycentric coordinate system
  x <- c(1, 0, 0); y <- c(0, 1, 0); z <- c(0, 0, 1)

  # Binary choice probabilities pyz and pxz as ternary probabilities xp and yp
  xp <- c(0, pyz, 1-pyz); yp <- c(pxz, 0, 1-pxz)

  # Interior point
  A = matrix(c(1-pxz, 0, -pxz,
               0, 1-pyz, -pyz,
               1,     1,    1), nrow=3, ncol=3, byrow=TRUE)
  w = solve(A, c(0, 0, 1))

  # Create regions
  Sxyz = matrix(c(y, yp, z), nrow=3, ncol=3, byrow=TRUE)
  Syxz = matrix(c(x, xp, z), nrow=3, ncol=3, byrow=TRUE)
  So = matrix(c(x, y, w), nrow=3, ncol=3, byrow=TRUE)
  Sx = matrix(c(y, w, xp), nrow=3, ncol=3, byrow=TRUE)
  Sy = matrix(c(x, w, yp), nrow=3, ncol=3, byrow=TRUE)
  Sxy = matrix(c(w, xp, z, yp), nrow=4, ncol=3, byrow=TRUE)
  list(Sxyz=Sxyz, Syxz=Syxz, So=So, Sx=Sx, Sy=Sy, Sxy=Sxy)
}
