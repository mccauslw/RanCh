#' Table of choice trial data from McCausland, Marley and Davis-Stober (2019)
#'
#' @format A tibble with 20 variables
#' \describe{
#' \item{\code{domain}}{factor, name of choice domain}
#' \item{\code{subject}}{subject identifier}
#' \item{\code{trial}}{trial identifier (gives the order in which a subject sees choice sets)}
#' \item{\code{duration}}{duration of trial in seconds}
#' \item{\code{set}}{factor, name of choice set presented: 'ab', 'cde', etc., with objects
#' in alphabetical order}
#' \item{\code{choice}}{factor, choice made by subject: 'a', 'b', 'c', 'd' or 'e'}
#' \item{\code{set_perm}}{factor, order of presentation of objects on screen, left to right}
#' \item{\code{set_card}}{Integer, cardinality of choice set (i.e. number of available options)}
#' \item{\code{set_index}}{Binary representation of choice set (binary digits indicate object membership in choice set)}
#' \item{\code{choice_int}}{Integer code for chosen object: a=1, b=2, ..., e=5}
#' \item{\code{ab}, \code{ac}, \ldots, \code{de}}{revealed preference indicator: taking column ab as an example,
#' value is 1 if a is revealed preferred to b, -1 if b is revealed preferred to a, 0 otherwise.}
#' }
#' @seealso \code{\link{RanCh}}, under \code{Datasets} for a description of the experiment.
#' Other data objects for the experiment include
#' \code{\link{MMS_2019_counts}}, an array of choice count data,
#' \code{\link{MMS_2019_demographics}}, a table of demographic information, and
#' \code{\link{MMS_2019_raw}}, a table of raw data.
"MMS_2019_trials"

#' Array of choice count data from McCausland, Marley and Davis-Stober (2019)
#'
#' A 32 by 31 by 5 array of count data.
#' Element \eqn{i,j,k} gives the number of subjects that chose object \eqn{k} when
#' presented with choice set \eqn{j} of domain \eqn{i}.
#' The choice set index j=1,...,31 encodes a non-empty subset of the universe
#' of the five choice objects a, b, c, d and e, numbered 1, 2, 3, 4, 5.
#' Whenever \eqn{k} is not an element of \eqn{j}, the value is \code{NA}.
#'
#' @seealso{\code{\link{RanCh}}}, under \code{Datasets} for a description of the experiment.
#' Other data objects for the experiment include
#' \code{\link{MMS_2019_trials}}, a table of choice trial data,
#' \code{\link{MMS_2019_demographics}}, a table of demographic information, and
#' \code{\link{MMS_2019_raw}}, a table of raw data.
#' \code{\link{set_index}} describes how the choice set index encodes the choice set.
"MMS_2019_counts"

#' Table of demographic information from McCausland, Marley and Davis-Stober (2019)
#'
#' @format A data frame with demographic information on subjects
#' \describe{
#' \item{\code{sex}}{Sex of subject: 1 for male, 2 for female}
#' \item{\code{age}}{Age of subject in years}
#' \item{\code{location}}{Province or territory in Canada, 1=Alberta, 2=British Columbia,
#' 3=Manitoba, 4=New Brunswick, 5=Newfoundland/Labrador, 6=Northwest Territories,
#' 7=Nova Scotia, 8=Ontario, 9=Prince Edward Island, 10=Quebec, 11=Saskatchewan, 12=Yukon}
#' }
#' @seealso{\code{\link{RanCh}}}, under \code{Datasets} for a description of the experiment.
#' Other data objects for the experiment include
#' \code{\link{MMS_2019_trials}}, a table of choice trial data,
#' \code{\link{MMS_2019_counts}}, a matrix of choice count data, and
#' \code{\link{MMS_2019_raw}}, a table of raw data.
"MMS_2019_demographics"

#' Table of raw data from McCausland, Marley and Davis-Stober (2019)
#'
#' @format A data frame with 23 variables
#' \describe{
#' \item{\code{responseid}}{Unused subject identifier}
#' \item{\code{gender}}{Sex of subject: 1 for male, 2 for female}
#' \item{\code{age}}{Age of subject in years}
#' \item{\code{location}}{Province or territory in Canada, 1=Alberta, 2=British Columbia,
#' 3=Manitoba, 4=New Brunswick, 5=Newfoundland/Labrador, 6=Northwest Territories,
#' 7=Nova Scotia, 8=Ontario, 9=Prince Edward Island, 10=Quebec, 11=Saskatchewan, 12=Yukon}
#' \item{\code{set}}{}
#' \item{\code{block}}{Index (1,...,1024) of pre-constructed random design assigned to subject}
#' \item{\code{consent}}{Whether or not subjects gives consent (1) or not(2). All subjects give consent.}
#' \item{\code{domain}}{Index of choice domain}
#' \item{\code{counts}}{Cardinality of choice set presented}
#' \item{\code{obj1}}{Object in position 1}
#' \item{\code{obj2}}{Object in position 2}
#' \item{\code{obj3}}{Object in position 3, possibly NA}
#' \item{\code{obj4}}{Object in position 4, possibly NA}
#' \item{\code{obj5}}{Object in position 5, possibly NA}
#' \item{\code{obj1_text}}{Complete text describing object 1}
#' \item{\code{obj2_text}}{Complete text describing object 2}
#' \item{\code{obj3_text}}{Complete text describing object 3}
#' \item{\code{obj4_text}}{Complete text describing object 4}
#' \item{\code{obj5_text}}{Complete text describing object 5}
#' \item{\code{choice}}{Position of object chosen}
#' \item{\code{expdur}}{Trial duration in ms}
#' \item{\code{intdur}}{Survey duration in minutes}
#' \item{\code{feedback}}{Subject's response to "Please provide any additional feedback about the survey you have just completed."}
#' }
#' @seealso{\code{\link{RanCh}}}, under \code{Datasets} for a description of the experiment.
#' Other data objects for the experiment include
#' \code{\link{MMS_2019_trials}}, a table of choice trial data,
#' \code{\link{MMS_2019_counts}}, a matrix of choice count data, and
#' \code{\link{MMS_2019_demographics}}, a table of demographic information
"MMS_2019_raw"

