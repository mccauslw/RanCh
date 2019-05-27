#' YouGov Experiment data
#'
#' Record of every choice made by every respondant.
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

#' Record of all choice trials
#'
#' @format A data frame with 14 variables
#' \describe{
#' \item{\code{subj}}{Subject identifier}
#' \item{\code{domain}}{Factor indicating choice domain}
#' \item{\code{trial}}{Trial identifier (gives the order in which a subject sees choice sets)}
#' \item{\code{subs}}{Factor indicating the choice subset presented: 'ab', 'cde', etc.}
#' \item{\code{choice}}{Factor indicating the choice made: 'a', 'b', 'c' or 'd'}
#' \item{\code{subs_conf}}{Subset configuration, the order objects appear on the screen}
#' \item{\code{subs_bin}}{Code for subset where digits of binary representation indicate object membership}
#' \item{\code{choice_int}}{Integer code for chosen object}
#' \item{\code{ab}}{Revealed preference indicator: 1 for a revealed preferred to b, -1 for b revealed preferred to a, 0 otherwise}
#' }
"YG_trials"

#' Counts
#'
#' A 3x16x15x4 matrix with count data.
#'
"YG_counts"
