% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fill_in_info_model.R
\name{fill_in_info_model}
\alias{fill_in_info_model}
\title{Get Model Information}
\usage{
fill_in_info_model(internal_list)
}
\arguments{
\item{internal_list}{A list with various information extracted from the
model.}
}
\value{
The inputted internal_list with several slots filled in:
\tabular{lll}{
 \code{..$info_parameters$RAM}: \code{list}  \tab \tab List with RAM matrices. \cr
 \code{..$info_parameters$parameter_table}: \code{data.frame}  \tab \tab Parameter table. \cr
 \code{..$info_parameters$algebras}: \code{data.frame}  \tab \tab Any algebra added to the model. \cr
 }
}
\description{
Provide information about the model parameters and model matrices of the dynamic panel data model.
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
