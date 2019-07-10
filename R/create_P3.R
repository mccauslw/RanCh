#' Random Choice Structure for a three-object universe
#'
#' \code{create_P3} creates a random choice structure for a three-object universe
#' from
#' @param p12 Probability of chosing object 1 when presented with objects 1 and 2
#' @param p23 Probability of chosing object 2 when presented with objects 2 and 3
#' @param P13 Probability of chosing object 1 when presented with objects 1 and 3
#' @param P1 Probability of chosing object 1 when presented with objects 1, 2 and 3
#' @param P2 Probability of chosing object 2 when presented with objects 1, 2 and 3
#' @param names character vector giving names to the three objects
#' @return A Random Choice Structure
#' @export
#' @examples
#' P = create_P3(21/40, 37/40, 28/40, 19/40, 15/40, names=c('Red', 'Purple', 'Pink'))
#' P
create_P3 <- function(p12, p23, p13, P1, P2, names=c('x', 'y', 'z')) {
  p21 = 1 -p12
  p32=1-p23
  p31=1-p13
  s1 = c(1, 0, 0)
  s2 = c(0, 1, 0)
  s3 = c(0, 0, 1)
  b12 = c(p12, 1-p12, 0)
  b23 = c(0, p23, 1-p23)
  b13 = c(p13, 0, 1-p13)
  T = c(P1, P2, 1-P1-P2)
  P = matrix(c(s1, s2, b12, s3, b13, b23, T), byrow=TRUE, nrow=7, ncol=3)
  n12 = paste(names[1], names[2], sep='')
  n13 = paste(names[1], names[3], sep='')
  n23 = paste(names[2], names[3], sep='')
  n123 = paste(names[1], names[2], names[3], sep='')
  rownames(P) = c(names[1], names[2], n12, names[3], n13, n23, n123)
  colnames(P) = names
  P
}
