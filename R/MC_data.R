#' Multiple Choice experiment data
#'
#' Raw trial-by-trial data from Multiple Choice experiment
#'
#' @format A data frame with 53 variables generated in (WJM)
#' We do not provide documentation, although the variable names are suggestive.
"MC_raw"
#' Tibble recording every choice trial in the multiple choice experiment
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
#' \item{\code{set_bin}}{binary representation of choice set (binary digits indicate object membership in choice set)}
#' \item{\code{choice_int}}{integer, code for chosen object: a=1, b=2, ..., e=5}
#' \item{\code{ab}, \code{ac}, \ldots, \code{de}}{revealed preference indicator:
#' taking column ab as an example, the value is 1 if a is revealed preferred to b,
#' -1 if b is revealed preferred to a, 0 otherwise.}}
"MC_trials"
#' Choice counts in multiple choice experiment
#'
#' A three dimensional array, 141 by 31 by 5, of count data.
#' Element i,j,k gives the number of times subject i chose object k when
#' presented with choice set j.
#' The choice set index j=1,...,31 encodes a non-empty subset of the universe
#' of the five choice objects a, b, c, d and e, numbered 1, 2, 3, 4, 5.
#' Each digit (or bit) in the binary representation of j is an inclusion indicator: object k
#' is in the set if and only if the k'th digit from the right is 1.
#' For example, the set with b, c, d and e (but not a) is encoded as binary 11110 (decimal 30).
#' Whenever k is not an element of j, the value is NA.
#'
"MC_counts"
