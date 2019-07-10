#' YouGov Experiment data
#'
#' Raw trial-by-trial data from YouGov experiment
#'
#' @format A data frame with 17 variables:
#' \describe{
#' \item{\code{design}}{}
#' \item{\code{card}}{}
#' \item{\code{domain}}{}
#' \item{\code{combo}}{}
#' \item{\code{perm}}{}
#' \item{\code{choiceset}}{Choice set as a character string}
#' \item{\code{option_1}}{Object presented in first position: 1, 2, 3 or 4}
#' \item{\code{option_2}}{Object presented in second position}
#' \item{\code{option_3}}{Object presented in third position}
#' \item{\code{option_4}}{Object presented in fourth position}
#' \item{\code{response}}{Object chosen: 1, 2, 3 or 4}
#' \item{\code{order}}{}
#' \item{\code{gender}}{Sex of respondant: 1 for male, 2 for female}
#' \item{\code{educ}}{Education of respondant: 1 for No high school, 2 for High school graduate,
#' 3 for Some college, 4 for 2-year college, 5 for 4-year college, 6 for post-graduate}
#' \item{\code{region}}{Region of respondant: 1 for northeast, 2 for midwest, 3 for south, 4 for west}
#' \item{\code{race}}{Race of respondant: 1 for White, 2 for Black, 3 for Hispanic, 4 for Asian,
#' 5 for Native American, 6 for Mixed, 7 for Other, 8 for Middle Eastern}
#' \item{\code{age_cross}}{Age category of respondant: 1 for 18-34, 2 for 35-54, 3 for 55 and over}
#' }
"YG_raw"

#' Demographic information for subjects
#'
#' @format A data frame with demographic information on subjects
#' \describe{
#' \item{\code{sex}}{Sex of subject}
#' \item{\code{educ}}{Educational attainment by subject}
#' \item{\code{region}}{Region of subject's residence in US}
#' \item{\code{race}}{Race of subject}
#' \item{\code{age_range}}{Age range of subject}
#' }
"YG_demographics"

#' Table of choice trials, population choice experiment
#'
#' @format A tibble with 17 variables
#' \describe{
#' \item{\code{domain}}{factor, name of choice domain}
#' \item{\code{subject}}{subject identifier}
#' \item{\code{block}}{block, equal to 1 or 2, identifying the first or second pass a subject makes through the domains.}
#' \item{\code{trial}}{trial identifier (gives the order in which a subject sees choice sets)}
#' \item{\code{duration}}{duration of trial in seconds}
#' \item{\code{set}}{factor, name of choice set presented: 'ab', 'bcd', etc., with objects
#' in alphabetical order}
#' \item{\code{choice}}{factor, choice made by subject: 'a', 'b', 'c', or 'd'}
#' \item{\code{set_perm}}{factor, order of presentation of objects on screen, left to right}
#' \item{\code{set_card}}{Integer, cardinality of choice set (i.e. number of available options)}
#' \item{\code{set_bin}}{Binary representation of choice set (binary digits indicate object membership in choice set)}
#' \item{\code{choice_int}}{Integer code for chosen object: a=1, b=2, ..., d=4}
#' \item{\code{ab}, \code{ac}, \ldots, \code{cd}}{revealed preference indicator: taking column ab as an example,
#' value is 1 if a is revealed preferred to b, -1 if b is revealed preferred to a, 0 otherwise.}
#' }
"YG_trials"

#' Choice counts in YouGov experiment
#'
#' A four dimensional array, 16 by 2 by 15 by 4, of count data.
#' Element h,i,j,k gives the number of subjects who chose object k when
#' presented with choice set j of domain h, in their i'th block of 16 trials.
#' The choice set index j=1,...,15 encodes a non-empty subset of the universe
#' of the four choice objects a, b, c, and d, numbered 1, 2, 3, 4.
#' Each digit (or bit) in the binary representation of j is an inclusion indicator: object k
#' is in the set if and only if the k'th digit from the right is 1.
#' For example, the set with b, c, and d (but not a) is encoded as binary 1110 (decimal 14)
#' Whenever k is not an element of j, the value is NA.
#'
"YG_counts"
