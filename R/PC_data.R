#' Population Choice experiment data
#'
#' Record of every choice made by every respondant.
#'
#' @format A data frame with 17 variables:
#' \describe{
#' \item{\code{design}}{}
#' \item{\code{gender}}{Sex of respondant: 1 for male, 2 for female}
#' }
"PC_raw"
#' Demographic information for subjects
#'
#' @format A data frame with demographic information on subjects
#' \describe{
#' \item{\code{sex}}{Sex of subject }
#' \item{\code{age}}{Age of subject in years}
#' \item{\code{location}}{Province or territory in Canada}
#' }
"PC_demographics"
#' Record of all choice trials
#'
#' @format A data frame with 14 variables
#' \describe{
#' \item{\code{subj}}{Subject identifier}
#' \item{\code{domain}}{Factor indicating choice domain}
#' \item{\code{trial}}{Trial identifier (gives the order in which a subject sees choice sets)}
#' \item{\code{subs}}{Factor indicating the choice subset presented: 'ab', 'cde', etc., objects
#' always in alphabetical order}
#' \item{\code{choice}}{Factor indicating the choice made: 'a', 'b', 'c', 'd' or 'e'}
#' \item{\code{subs_conf}}{Subset configuration, the order objects appear on the screen: 'ba', 'ecd', etc.,
#' objects not necessarily in alphabetical order}
#' \item{\code{subs_bin}}{Code for subset where digits of binary representation indicate object membership}
#' \item{\code{choice_int}}{Integer code for chosen object}
#' \item{\code{ab}}{Revealed preference indicator: 1 for a revealed preferred to b,
#' -1 for b revealed preferred to a, 0 otherwise.
#' This is the first of ten revealed preference columns, each pertaining to a particular
#' doubleton set.}
#' }
"PC_trials"
#' Counts
#'
#' A 32x26x5 matrix with count data.
#'
"PC_counts"
