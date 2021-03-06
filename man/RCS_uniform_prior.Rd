% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/likelihoods.R
\name{RCS_uniform_prior}
\alias{RCS_uniform_prior}
\title{Uniform prior for a RCS}
\usage{
RCS_uniform_prior(n_objects)
}
\arguments{
\item{n_objects}{number of objects in the universe.}
}
\value{
a matrix of Dirichlet parameters all set to one, so that all choice
probability vectors are uniformly distributed.
}
\description{
\code{RCS_uniform_prior} constructs a matrix of Dirichlet parameters for the
uniform prior over a random choice structure.
}
\examples{
Alpha <- RCS_uniform_prior(3)
P <- sim_RCS_Dirichlet(10, Alpha)
}
