#' Check if binary-ternary choice probabilities are in similarity effect region
#'
#' \code{similarity_2_3} returns TRUE if the specified binary and ternary
#' choice probabilities in random choice structure P are in the specified
#' similarity effect region.
#' @param P A random choice structure
#' @param target Index of target object
#' @param competitor Index of competitor object
#' @param decoy Index of decoy object
#' @param weak Logical, whether similarity effect is weak or not
#' @two_sided A logical value indicating whether the similarity inequality is to hold
#' also for target and competitor exchanged.
#' @return A logical value indicating whether the appropriate choice probabilities are
#' in the similarity effect region.
#' @export
#' @keywords axioms indicators
#' @examples
#' P = create_P(0.7, 0.6, 0.8, 0.6, 0.3, 0.1, names = c('x', 'y', 'z'))
#' similarity_2_3(P, target=1, competitor=2, decoy=3)
#'
similarity_2_3 = function(P, target, competitor, decoy, weak=TRUE, two_sided=FALSE) {
  # Construct ternary set U = {target, competitor, decoy}
  T = sum(bitShiftL(1, c(target, competitor, decoy)-1))

  # Construct two binary sets {t, d} and {c, d}
  td = sum(bitShiftL(1, c(target, decoy)-1))
  cd = sum(bitShiftL(1, c(competitor, decoy)-1))

  #
}
