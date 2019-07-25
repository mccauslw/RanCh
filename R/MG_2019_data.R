#' Table of choice trials data from McCausland and Gerringer (2019)
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
#' \item{\code{set_index}}{Binary representation of choice set (binary digits indicate object membership in choice set)}
#' \item{\code{choice_int}}{Integer code for chosen object: a=1, b=2, ..., d=4}
#' \item{\code{ab}, \code{ac}, \ldots, \code{cd}}{revealed preference indicator: taking column ab as an example,
#' value is 1 if a is revealed preferred to b, -1 if b is revealed preferred to a, 0 otherwise.}
#' }
#' @seealso \code{\link{RanCh}}, under \code{Datasets} for a description of the experiment.
#' Other data objects for the experiment include
#' \code{\link{MG_2019_counts}}, an array of choice count data,
#' \code{\link{MG_2019_demographics}}, a table of demographic information, and
#' \code{\link{MG_2019_raw}}, a table of raw data.
"MG_2019_trials"

#' Table of choice domains from McCausland and Gerringer (2019)
#'
#' @format A tibble with 7 variables
#' \describe{
#' \item{\code{Q}}{character string, question posed to respondents to elicit
#' their response for this domain}
#' \item{\code{response_a}}{character string, response for object 'a'}
#' \item{\code{response_b}}{character string, response for object 'b'}
#' \item{\code{response_c}}{character string, response for object 'c'}
#' \item{\code{response_d}}{character string, response for object 'd'}
#' \item{\code{domain_name}}{name of domain, agreeing with names in
#' \code{\link{MG_2019_trials}} and \code{\link{MG_2019_counts}}.
#' \item{\code{object_labels}}{character vector of length four, short
#' versions of object descriptions suitable for labels in graphs.}
"MG_2019_domains"

#' Array of choice count data from McCausland and Gerringer (2019)
#'
#' A four dimensional array, 16 by 2 by 15 by 4, of count data.
#' Element \eqn{h,i,j,k} gives the number of subjects who chose object \eqn{k} when
#' presented with choice set \eqn{j} of domain \eqn{h}, in their \eqn{i}'th block
#' of 16 trials.
#' The choice set index \eqn{j=1,...,15} encodes a non-empty subset of the universe
#' of the four choice objects a, b, c, and d, numbered 1, 2, 3, 4.
#' Whenever \eqn{k} is not an element of \eqn{j}, the value is \code{NA}.
#' @seealso{\code{\link{RanCh}}}, under \code{Datasets} for a description of the experiment.
#' Other data objects for the experiment include
#' \code{\link{MG_2019_trials}}, a table of choice trial data,
#' \code{\link{MG_2019_demographics}}, a table of demographic information, and
#' \code{\link{MG_2019_raw}}, a table of raw data.
#' \code{\link{set_index}} describes how the choice set index encodes the choice set.
"MG_2019_counts"

#' Table of demographic information from McCausland and Gerringer (2019)
#'
#' @format A data frame with demographic information on subjects
#' \describe{
#' \item{\code{sex}}{Sex of subject}
#' \item{\code{educ}}{Educational attainment by subject}
#' \item{\code{region}}{Region of subject's residence in US}
#' \item{\code{race}}{Race of subject}
#' \item{\code{age}}{Age range of subject}
#' }
#' @seealso{\code{\link{RanCh}}}, under \code{Datasets} for a description of the experiment.
#' Other data objects for the experiment include
#' \code{\link{MG_2019_trials}}, a table of choice trial data,
#' \code{\link{MG_2019_counts}}, a matrix of choice count data, and
#' \code{\link{MG_2019_raw}}, a table of raw data.
"MG_2019_demographics"

#' Table of choice trial data from McCausland and Gerringer (2019)
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
#' @seealso{\code{\link{RanCh}}}, under \code{Datasets} for a description of the experiment.
#' Other data objects for the experiment include
#' \code{\link{MG_2019_trials}}, a table of choice trial data,
#' \code{\link{MG_2019_counts}}, a matrix of choice count data, and
#' \code{\link{MG_2019_demographics}}, a table of demographic information
"MG_2019_raw"
