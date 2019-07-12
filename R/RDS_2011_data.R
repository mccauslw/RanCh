#' Choice count data from the Regenwetter, Dana and Davis-Stober (2011) experiment
#'
#' A 3 by 18 by 31 by 5 array of count data.
#' Element \eqn{h,i,j,k} gives the number of times subject \eqn{i} chose object
#' \eqn{k} when presented with choice set \eqn{j} of domain \eqn{h}.
#' Domain names are "Cash 1", "Cash 2" and "Noncash", explained in the paper.
#' The choice set index \eqn{j \in \{1,...,31\}} encodes a non-empty subset of the
#' universe of the four choice objects a, b, c, d, and e, numbered 1, 2, 3, 4, 5.
#' Whenever \eqn{k} is not an element of \eqn{j}, the value is \code{NA}.
#' @seealso{\code{\link{RanCh}}}, under \code{Datasets} for a description of the experiment.
#' \code{\link{set_index}} describes how the choice set index encodes the choice set.
"RDS_2011_counts"
