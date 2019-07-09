#' Population Choice experiment data
#'
#' Record of every choice made by every respondant.
#'
#' @format A data frame with 17 variables:
#' \describe{
#' \item{\code{design}}{}
#' \item{\code{gender}}{Sex of respondant: 1 for male, 2 for female}
#' }
"MC_raw"
#' Table of choice trials, multiple choice experiment
#'
#' @format A tibble with 18 variables
#' \describe{
#' \item{\code{subject}}{subject identifier}
#' \item{\code{trial}}{trial identifier (gives the order in which a subject sees choice sets)}
#' \item{\code{set}}{factor, name of choice set presented: 'ab', 'cde', etc., with objects
#' in alphabetical order}
#' \item{\code{choice}}{factor, choice made by subject: 'a', 'b', 'c', 'd' or 'e'}
#' \item{\code{set_perm}}{factor, order of presentation of objects on screen, left to right}
#' \item{\code{set_card}}{Integer, cardinality of choice set (i.e. number of available options)}
#' \item{\code{set_bin}}{Binary representation of choice set (binary digits indicate object membership in choice set)}
#' \item{\code{choice_int}}{Integer code for chosen object: a=1, b=2, ..., e=5}
#' \item{\code{ab}, \code{ac}, \ldots \code{de}}{revealed preference indicator: taking column ab as an example,
#' value is 1 if a is revealed preferred to b, -1 if b is revealed preferred to a, 0 otherwise.}
#' }
"MC_trials"
#' Counts
#'
#' A 141x26x5 matrix with count data.
#'
"MC_counts"
