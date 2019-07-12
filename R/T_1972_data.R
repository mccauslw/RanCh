#' Choice count data from the Tversky (1972) experiment
#'
#' A four dimensional array, 3 by 8 by 7 by 3, of count data.
#' Element \eqn{h,i,j,k} gives the number of times subject \eqn{i} chose object
#' \eqn{k} when presented with choice set \eqn{j} of domain \eqn{h}.
#' Domain names are "Dots", "Gambles" and "Applicants", explained in the paper.
#' The choice set index \eqn{j \in \{1,...,7\}} encodes a non-empty subset of the
#' universe of the three choice objects x, y, and z, numbered 1, 2, 3.
#' Each digit (or bit) in the binary representation of \eqn{j} is an inclusion
#' indicator: object \eqn{k} is in the set if and only if the \eqn{k}'th digit from
#' the right is 1. For example, the set \eqn{\{y, z\}} is encoded as binary 110
#' (decimal 6).
#' Whenever \eqn{k} is not an element of \eqn{j}, the value is \code{NA}.
#' @seealso{\code{\link{RanCh}}}, under \code{Datasets} for a description of the experiment.
"T_1972_counts"
