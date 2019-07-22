#' Choice count data from the Tversky (1972) experiment
#'
#' A four dimensional array, 3 by 8 by 7 by 3, of count data.
#' Element \code{T_1972_counts[h,i,j,k]} gives the number of times
#' subject \code{i} chose object
#' \code{k} when presented with choice set \code{j} of domain \code{h}.
#' Domain names are "Dots", "Gambles" and "Applicants", explained in the paper.
#' The choice set index \code{j=1,...,7} encodes a non-empty subset of the
#' universe of the three choice objects \code{x}, \code{y}, and \code{z}, numbered 1, 2, 3.
#' Whenever \code{k} is not an element of \code{j}, the value is \code{NA}.
#' @seealso{\code{\link{RanCh}}}, under \code{Data sets} for a description of the
#' experiment.
#' \code{\link{set_index}} describes how the choice set index encodes the choice set.
"T_1972_counts"
