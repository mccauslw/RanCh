#' Compromise effect regions
#'
#' \code{compromise_X3} constructs six regions (all polygons) associated with
#' binary-ternary compromise effects.
#' Below, \eqn{x} and \eqn{z} are the extreme choice objects and \eqn{y} is the between
#' (or compromise) object.
#' The interior of each polygon gives the set of ternary choice probabilities
#' consistent with (1) the two specified binary choice probabilities and (2)
#' one of six compromise effect conditions.
#' @param pyx scalar, probability \eqn{p(y,x)} of choosing \eqn{y} when
#' presented with choice set \eqn{\{x,y\}}.
#' @param pyz scalar, probability \eqn{p(y,z)} of choosing \eqn{y} when
#' presented with choice set \eqn{\{y,z\}}.
#' @return named list of six polygons in a barycentric coordinate system.
#' Each polygon is a matrix, with one row for each polygon vertex and one column
#' for each of the three choice objects \eqn{x}, \eqn{y} and \eqn{z}.
#' Element \eqn{i,j} gives the probability of choosing \eqn{j} when presented with
#' choice set \eqn{\{x,y,z\}}, at the \eqn{i}'th polygon vertex.
#' The six polygons---triangles except for \code{Cxz}, a quadrilateral---are:
#' \describe{
#' \item{\code{Cxyz}}{region where there is a compromise effect with \eqn{y} as target,
#' \eqn{x} as competitor and \eqn{z} as decoy}
#' \item{\code{Czyx}}{region where there is a compromise effect with \eqn{y} as target,
#' \eqn{z} as competitor and \eqn{x} as decoy}
#' \item{\code{Co}}{region where there is neither compromise effect}
#' \item{\code{Cx}}{region with only the \eqn{x} effect}
#' \item{\code{Cz}}{region with only the \eqn{z} effect}
#' \item{\code{Cxz}}{region with both effects}
#' }
#' @export
#' @examples
#' C = compromise_X3(0.5, 0.6)
#' @seealso \code{\link{similarity_X3}} for an analogous function for the
#' similarity effect.
compromise_X3 <- function(pyx, pyz) {

  # Vertices of barycentric coordinate system
  x <- c(1, 0, 0); y <- c(0, 1, 0); z <- c(0, 0, 1)

  # Binary choice probabilities pyz and pxz as ternary probabilities xp and yp
  xp <- c(0, pyz, 1-pyz); zp <- c(1-pyx, pyx, 0)

  # Compute interior point w
  A = matrix(c(-pyx, 1-pyx, 0,
               0, 1-pyz, -pyz,
               1,     1,    1), nrow=3, ncol=3, byrow=TRUE)
  w = solve(A, c(0, 0, 1))

  # Construct regions
  Cxyz = matrix(c(y, z, zp), nrow=3, ncol=3, byrow=TRUE)
  Czyx = matrix(c(x, z, zp), nrow=3, ncol=3, byrow=TRUE)
  Co = matrix(c(w, x, z), nrow=3, ncol=3, byrow=TRUE)
  Cx = matrix(c(w, xp, z), nrow=3, ncol=3, byrow=TRUE)
  Cz = matrix(c(w, x, zp), nrow=3, ncol=3, byrow=TRUE)
  Cxz = matrix(c(w, xp, y, zp), nrow=4, ncol=3, byrow=TRUE)
  list(Cxyz=Cxyz, Czyx=Czyx, Co=Co, Cx=Cx, Cz=Cz, Cxz=Cxz)
}
