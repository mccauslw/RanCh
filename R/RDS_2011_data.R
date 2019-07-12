#' Choice count data from the Regenwetter, Dana and Davis-Stober (2011) experiment
#'
#' A four dimensional array, 3 by 18 by 31 by 5, of count data.
#' Element \eqn{h,i,j,k} gives the number of times subject \eqn{i} chose object
#' \eqn{k} when presented with choice set \eqn{j} of domain \eqn{h}.
#' Domain names are "Cash 1", "Cash 2" and "Noncash", explained in the paper.
#' The choice set index \eqn{j \in \{1,...,31\}} encodes a non-empty subset of the
#' universe of the four choice objects a, b, c, d, and e, numbered 1, 2, 3, 4, 5.
#' Each digit (or bit) in the binary representation of \eqn{j} is an inclusion
#' indicator: object \eqn{k} is in the set if and only if the \eqn{k}'th digit from
#' the right is 1. For example, the set \eqn{\{b, c, d\}} is encoded as binary 01110
#' (decimal 14).
#' Whenever \eqn{k} is not an element of \eqn{j}, the value is \code{NA}.
#' @seealso{\code{\link{RanCh}}}, under \code{Datasets} for a description of the experiment.
"RDS_2011_counts"
