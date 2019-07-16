#' Choice count data from the Tversky (1969) experiment
#'
#' An 8 by 31 by 5 array of count data.
#' Element \eqn{i,j,k} gives the number of times subject \eqn{i} chose object
#' \eqn{k} when presented with choice set \eqn{j}.
#' The choice set index \eqn{j \in \{1,...,7\}} encodes a non-empty subset of the
#' universe of the three choice objects \eqn{x}, \eqn{y}, and \eqn{z}, numbered 1, 2, 3.
#' Whenever \eqn{k} is not an element of \eqn{j}, the value is \code{NA}.
#' @seealso{\code{\link{RanCh}}}, under \code{Datasets} for a description of the experiment.
#' \code{\link{set_index}} describes how the choice set index encodes the choice set.
"T_1969_counts"
