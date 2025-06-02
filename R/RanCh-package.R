#' RanCh: A package for abstract discrete Random Choice
#'
#' The RanCh package provides data, graphical tools and inferential tools for
#' abstract discrete random choice analysis.
#'
#' A central data structure is the Random Choice Structure (RCS), which organizes all
#' choice probabities on some universe of choice objects.
#' If \eqn{n} is the number of choice objects in the universe, the RCS is a
#' \eqn{2^n-1} by \eqn{n} matrix of probabilities.
#' Rows are indexed by non-empty subset (the help for the \code{\link{set_index}}
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
#' Random RCSs can be constructed using multiple Dirichlet priors (including
#' the uniform distribution) using \code{\link{sim_RCS_Dirichlet}}.
#'
#' Another important data structure is the count matrix, which organizes choice counts
#' from an experiment. A count matrix for a given universe of objects has the same
#' dimensions as an RCS for the same universe of objects.
#' Here, element \eqn{i,j} is the number of times object \eqn{j} was chosen in the
#' experiment when choice subset \eqn{i} was presented; again, impossible counts have
#' the value NA.
#'
#' @section Data sets:
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
#' @section Graphical tools:
#'
#' This package makes slight use of the package \code{klaR} for diplaying points,
#' lines and polygons in barycentric coordinates; the main vignette uses \code{klaR}
#' more extensively in examples.
#' Users of \code{RanCh} can easily use other packages for plotting in barycentric
#' coordinates, such as \code{ternary} and \code{ggtern}.
#' The graphical tools in \code{RanCh} generate points and polygons in 2-D and 3-D
#' barycentric coordinates.
#' It is left to the user to plot these using whatever package is most convenient.
#'
#' @section Axioms:
#'
#' Functions such as \code{\link{regularity}} and \code{\link{random_utility}} test whether
#' or not a given RCS satisfies some probabilistic choice axiom.
#'
#' Functions such as \code{\link{regularity_X3}} and \code{\link{multiplicative_X3}} return
#' polygons in barycentric coordinates indicating the values of the ternary
#' choice probability vector that are compatible with some probabilistic choice
#' axiom and given binary choice probabilities.
#'
#' @section Context effects:
#'
#' Functions such as \code{\link{compromise}} and \code{\link{similarity}} test whether or not
#' a given RCS exhibits some context effect.
#'
#' Functions such as \code{\link{compromise_X3}} and \code{\link{similarity_X3}} return
#' polygons in barycentric coordinates indicating the values of the ternary
#' choice probability vector that are compatible with some context effect
#' and given binary choice probabilities.
#'
#' @section Inference:
#'
#' The function \code{\link{log_L_DCE_multinomial}} computes the probability of
#' the realized data in a count matrix, as a function of a RCS.
#' Likelihood functions for many different kinds of models can be evaluated by
#' first mapping parameter values into a RCS then evaluating
#' \code{\link{log_L_DCE_multinomial}}.
#' For example, \code{log_L_DCE_multinomial(P_Luce(c(3, 1.4, 4.0)), N)}
#' evaluates the likelihood function of Luce's choice model for data \code{N}.
#'
#' The function \code{\link{log_L_DCE_Dir_mult}} adds a level of indirection using
#' a multiple Dirichlet prior distribution for the RCS.
#' It computes the probability of the realized data in a count matrix, as a function
#' of a multiple Dirichlet parameter matrix.
#' Likelihood functions for many different kinds of models can be evaluated
#' by first mapping parameter values into a Dirichlet parameter matrix then
#' evaluating \code{\link{log_L_DCE_Dir_mult}}.
#' For example, \code{log_L_DCE_Dir_mult(RCS_vector_alpha_prior(10.0, c(3, 1.4, 4.0)), N)}
#' evaluates the likelihood function for a Luce-like model.
#' The \code{alpha} parameter of \code{\link{RCS_vector_alpha_prior}} controls the
#' prior precision of the implicit mixing RCS; in the limit as \code{alpha} goes
#' to infinity, we get the Luce model.
#'
#' The function \code{\link{sim_RCS_Dirichlet}} can be used for prior and posterior
#' simulation, for a model where the RCS has a multiple Dirichlet prior.
#' High posterior density (HPD) regions for binary and ternary choice probabilities
#' can be computed using \code{\link{Dir2_HD_region}} and \code{\link{Dir3_HD_region}},
#' respectively.
#'
#' @name RanCh
#' @keywords package
"_PACKAGE"
