#' Check if choice probabilities exhibit similarity effect
#'
#' \code{similarity} returns TRUE if the specified binary and ternary
#' choice probabilities in random choice structure \code{P} are in the
#' specified similarity effect region.
#' @param P matrix, random choice structure
#' @param target index of target object
#' @param competitor index of competitor object
#' @param decoy index of decoy object
#' @param two_sided logical value indicating whether the similarity inequality
#' is to hold also for target and decoy exchanged.
#' @return A logical value indicating whether the appropriate choice
#' probabilities are in the similarity effect region.
#' @export
#' @examples
#' P <- create_P3(0.7, 0.6, 0.8, 0.6, 0.3, names = c('x', 'y', 'z'))
#' similarity(P, target=1, competitor=2, decoy=3)
#'
similarity <- function(P, target, competitor, decoy, two_sided=FALSE) {
  # Notation in Tversky (1972)
  x <- target; y <- decoy; z <- competitor

  # Construct ternary set T = {target, competitor, decoy}
  T <- set_index(c(x, y, z))

  # Construct two binary sets {t, d} and {c, d}
  xz <- set_index(c(x, z))
  yz <- set_index(c(y, z))

  # Contruct relevant probabilities
  sim_1 <- P[xz, x] > P[T, x]/(P[T, x] + P[T, z])
  sim_2 <- P[yz, y] > P[T, y]/(P[T, y] + P[T, z])

  if (two_sided) (sim_1 & sim_2) else sim_1
}
