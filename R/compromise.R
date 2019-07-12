#' Check if choice probabilities exhibit compromise effect
#'
#' \code{compromise} returns TRUE if the specified binary and ternary
#' choice probabilities in random choice structure \code{P} are in the specified
#' compromise effect region.
#' @param P matrix, random choice structure
#' @param target index of target object
#' @param competitor index of competitor object
#' @param decoy index of decoy object
#' @param two_sided logical value indicating whether the compromise inequality
#' is to hold also for competitor and decoy exchanged.
#' @return A logical value indicating whether the appropriate choice probabilities are
#' in the similarity effect region.
#' @export
#' @examples
#' P <- create_P3(0.7, 0.6, 0.8, 0.6, 0.3, names = c('x', 'y', 'z'))
#' compromise(P, target=1, competitor=2, decoy=3)
#'
compromise <- function(P, target, competitor, decoy, two_sided=FALSE) {
  # Notation in Tversky and Simonson (1993)
  x <- competitor; y <- target; z <- decoy

  # Construct ternary set T = {competitor, target, decoy}
  T <- set_index(c(x, y, z))

  # Construct two binary sets {t, d} and {c, d}
  xy <- set_index(c(x, y))
  yz <- set_index(c(y, z))

  # Evaluate indicators for two compromise effects
  com_1 <- P[xy, y] < P[T, y]/(P[T, x] + P[T, y])
  com_2 <- P[yz, y] < P[T, y]/(P[T, y] + P[T, z])

  if (two_sided) (com_1 & com_2) else com_1
}
