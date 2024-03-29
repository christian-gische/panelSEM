% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auxiliary_model.R
\name{auxiliary_model}
\alias{auxiliary_model}
\title{Set up auxiliary models}
\usage{
auxiliary_model(
  time_varying_variables,
  time_invariant_variables,
  linear,
  heterogeneity,
  use_open_mx,
  verbose
)
}
\arguments{
\item{time_varying_variables}{List of character vectors containing names of the time-varying
variables. The number entries in the list corresponds to the number of univariate time-series.
Each character vector contains the time-ordered variable names of a univariate time-series
starting with the first measurement occasion.}

\item{time_invariant_variables}{List of character vectors containing names of the time-invariant
variables. List must have the same length as list in argument \code{time_varying_variables}.}

\item{linear}{Logical (TRUE = default / FALSE) indicating if the model is linear in observed variables (TRUE).}

\item{heterogeneity}{Character vector indicating the type of unobserved heterogeneity. Admissible values are \code{"homogeneous"}, \code{"additive"}, \code{"autoregressive"}, and \code{"cross-lagged"} (or any non-conflicting combination).}

\item{use_open_mx}{Logical (TRUE / FALSE default) indicating if \code{lavaan} (FALSE) or \code{OpenMx} (TRUE)
should be used.}

\item{verbose}{Integer number describing the verbosity of console output.
Admissible values: 0: no output (default), 1: user messages,
2: debugging-relevant messages.}
}
\value{
An object of class \code{panelSEM}.
}
\description{
Set up an auxiliary model from that differs from the target model.
}
\references{
Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible
Framework for Studying Causal Effects Using Linear Models. Psychometrika 87,
868–901. https://doi.org/10.1007/s11336-021-09811-z

Gische, C., West, S.G., & Voelkle, M.C. (2021) Forecasting Causal
 Effects of Interventions versus Predicting Future Outcomes, Structural
 Equation Modeling: A Multidisciplinary Journal, 28:3, 475-492,
 DOI: 10.1080/10705511.2020.1780598
}
\keyword{external}
