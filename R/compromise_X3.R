#' Compute various cross sections of compromise effect regions
#'
#' \code{compromise_X3} computes six regions associated with similary effects
#' @param pyx Binary choice probability, where y is the between, or compromise object and
#' x is one of the extreme objects.
#' @param pyz Binary choice probability, where y is the between, or compromise, object and
#' z is the other extreme object.
#' @return A list of six regions in barycentric coordinates. Rows are vertices, columns give
#' ternary probabilities for objects x, y and z, respectively.
#' \describe{
#' \item{\code{Cxyz}}{region where there is a similarity effect with object y as target,
#' x as competitor and z as decoy}
#' \item{\code{Czyx}}{region where there is a similarity effect with object y as target,
#' z as competitor and x as decoy}
#' \item{\code{Co}}{region where there is neither similarity effect}
#' \item{\code{Cx}}{region with only the x effect}
#' \item{\code{Cz}}{region with only the z effect}
#' \item{\code{Cxz}}{region with both effects}
#' }
#' @export
#' @keywords "context effects" regions
#' @examples
#' C = compromise_X3(0.5, 0.6)
compromise_X3 <- function(pyx, pyz) {
  # Vertices of barycentric coordinate system
  x <- c(1, 0, 0); y <- c(0, 1, 0); z <- c(0, 0, 1)

  # Binary choice probabilities pyz and pxz as ternary probabilities xp and yp
  xp <- c(0, pyz, 1-pyz); zp <- c(1-pyx, pyx, 0)

  # Interior point
  A = matrix(c(-pyx, 1-pyx, 0,
               0, 1-pyz, -pyz,
               1,     1,    1), nrow=3, ncol=3, byrow=TRUE)
  w = solve(A, c(0, 0, 1))

  # Create regions
  Cxyz = matrix(c(y, z, zp), nrow=3, ncol=3, byrow=TRUE)
  Czyx = matrix(c(x, z, zp), nrow=3, ncol=3, byrow=TRUE)
  Co = matrix(c(w, x, z), nrow=3, ncol=3, byrow=TRUE)
  Cx = matrix(c(w, xp, z), nrow=3, ncol=3, byrow=TRUE)
  Cz = matrix(c(w, x, zp), nrow=3, ncol=3, byrow=TRUE)
  Cxz = matrix(c(w, xp, y, zp), nrow=4, ncol=3, byrow=TRUE)
  list(Cxyz=Cxyz, Czyx=Czyx, Co=Co, Cx=Cx, Cz=Cz, Cxz=Cxz)
}
