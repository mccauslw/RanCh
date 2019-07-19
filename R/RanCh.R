#' RanCh: A package for abstract discrete Random Choice
#'
#' The RanCh package provides data, graphical tools and inference tools for
#' abstract discrete random choice analysis.
#'
#' A central data structure is the Random Choice Structure (RCD), which organizes all
#' choice probabities on some universe of choice objects.
#' If \eqn{n} is the number of choice objects in the universe, the RCD is a
#' \eqn{2^n-1 \times n} matrix of probabilities.
#' Rows are indexed by non-empty subset (the help for the \code{set_index}
#' describes how vectors of elements map to set indices) and columns are indexed
#' by choice object.
#' Element \eqn{i,j} of an RCS gives the probability that object \eqn{j} is chosen
#' when choice subset \eqn{i} is presented; if the object indexed by \eqn{j} is
#' not an element of the set indexed by \eqn{i}, then the value is NA, by convention.
#' The function \code{\link{marginalize}} can be used to marginalize a RCS to a lower
#' dimensional RCS.
#'
#' Special RCSs can be created using functions like \code{\link{P_uniform}} and
#' \code{\link{P_Luce}}.
#' RCSs repesenting frequencies can be constructed from choice data using
#' \code{\link{proportions}}.
#'
#' Another important data structure is the count matrix, which organizes choice counts
#' from an experiment. A count matrix for a given universe of objects has the same
#' dimensions as an RCD for the same universe of objects.
#' Here, element \eqn{i,j} is the number of times object \eqn{j} was chosen in the
#' experiment when choice subset \eqn{i} was presented; again, impossible counts have
#' the value NA.
#'
#' @section Data sets
#'
#' The package provides many data sets to users.
#' Some are from experiments in the context effects literature
#' (e.g. \code{\link{T_1972_counts}})
#' and the stochastic intransitivity literature
#' (e.g. \code{\link{RDS_2011_counts}}).
#' For more details, see the help provided for the data set.
#'
#' The package includes datasets from three relatively new experiments where, by
#' experimental design, we observe choices from all doubleton or larger subsets of some
#' universe (or universes) of choice objects.
#'
#' @section Axioms
#'
#' Functions such as \code{\link{regularity}} and \code{\link{random_utility}} test whether
#' or not a given RCS satisfies some probabilistic choice axiom.
#'
#' Functions such as \code{\link{regularity_X3}} and \code{\link{multiplicative_X3}} return
#' polygons in barycentric coordinates indicating the values of the ternary
#' choice probability vector that are compatible with some probabilistic choice
#' axiom and given binary choice probabilities.
#'
#' @section Context effects
#'
#' Functions such as \code{\link{compromise}} and \code{\link{similarity}} test whether or not
#' a given RCS exhibits some context effect.
#'
#' Functions such as \code{\link{compromise_X3}} and \code{\link{similarity_X3}} return
#' polygons in barycentric coordinates indicating the values of the ternary
#' choice probability vector that are compatible with some context effect
#' and given binary choice probabilities.
#'
#' @section Inference
#'
#' @docType package
#' @name RanCh
NULL
