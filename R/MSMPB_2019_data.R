#' Table of choice trial data from the Multiple Choice experiment
#'
#' @format A tibble with 18 variables
#' \describe{
#' \item{\code{subject}}{subject identifier}
#' \item{\code{trial}}{trial identifier (gives the order in which a subject sees choice sets)}
#' \item{\code{set}}{factor, name of choice set presented: 'ab', 'cde', etc., with objects
#' in alphabetical order}
#' \item{\code{choice}}{factor, choice made by subject: 'a', 'b', 'c', 'd' or 'e'}
#' \item{\code{set_perm}}{factor, order of presentation of objects on screen, left to right}
#' \item{\code{set_card}}{integer, cardinality of choice set (i.e. number of available options)}
#' \item{\code{set_index}}{binary representation of choice set (binary digits indicate object membership in choice set)}
#' \item{\code{choice_int}}{integer, code for chosen object: a=1, b=2, ..., e=5}
#' \item{\code{ab}, \code{ac}, \ldots, \code{de}}{revealed preference indicator:
#' taking column ab as an example, the value is 1 if a is revealed preferred to b,
#' -1 if b is revealed preferred to a, 0 otherwise.}}
#' @seealso \code{\link{RanCh}}, under \code{Datasets} for a description of the experiment.
#' Other data objects for the experiment include
#' \code{\link{MSMPB_2019_counts}}, an array of choice count data, and
# \code{\link{MSMPB_2019_demographics}}, a table of demographic information and
#' \code{\link{MSMPB_2019_raw}}, a table of raw data.
"MSMPB_2019_trials"

#' Choice counts in multiple choice experiment
#'
#' A 141 by 31 by 5 array of count data.
#' Element \eqn{i,j,k} gives the number of times subject \eqn{i} chose object \eqn{k} when
#' presented with choice set \eqn{j}.
#' The choice set index j=1,...,31 encodes a non-empty subset of the universe
#' of the five choice objects a, b, c, d and e, numbered 1, 2, 3, 4, 5.
#' Whenever \eqn{k} is not an element of \eqn{j}, the value is \code{NA}.
#' @seealso \code{\link{RanCh}}, under \code{Datasets} for a description of the experiment.
#' Other data objects for the experiment include
#' \code{\link{MSMPB_2019_trials}}, a table of choice trial data, and
# \code{\link{MSMPB_2019_demographics}}, a table of demographic information and
#' \code{\link{MSMPB_2019_raw}}, a table of raw data.
#' \code{\link{set_index}} describes how the choice set index encodes the choice set.
"MSMPB_2019_counts"

#' Multiple Choice experiment data
#'
#' Raw trial-by-trial data from Multiple Choice experiment
#'
#' @format A data frame with 53 variables generated in (WJM)
#' We do not provide documentation, although the variable names are suggestive.
#' @seealso \code{\link{RanCh}}, under \code{Datasets} for a description of the experiment.
#' Other data objects for the experiment include
#' \code{\link{MSMPB_2019_trials}}, a table of choice trial data, and
#' \code{\link{MSMPB_2019_counts}}, an array of choice count data.
"MSMPB_2019_raw"
