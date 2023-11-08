## Changelog:
# CG 0.0.2 2023-05-24: replaced arguments homogeneity and additive by heterogeneity
# CG 0.0.1 2023-03-23: initial programming

## Documentation
#' @title Set up auxiliary models
#' @description Set up an auxiliary model from that differs from the target model.
#' @param time_varying_variables List of character vectors containing names of the time-varying
#'  variables. The number entries in the list corresponds to the number of univariate time-series.
#'  Each character vector contains the time-ordered variable names of a univariate time-series
#'  starting with the first measurement occasion.
#' @param time_invariant_variables List of character vectors containing names of the time-invariant
#'  variables. List must have the same length as list in argument \code{time_varying_variables}.
#' @param linear Logical (TRUE = default / FALSE) indicating if the model is linear in observed variables (TRUE).
#' @param heterogeneity Character vector indicating the type of unobserved heterogeneity. Admissible values are \code{"homogeneous"}, \code{"additive"}, \code{"autoregressive"}, and \code{"cross-lagged"} (or any non-conflicting combination).
#' @param use_open_mx Logical (TRUE / FALSE default) indicating if \code{lavaan} (FALSE) or \code{OpenMx} (TRUE)
#' should be used.
#' @param verbose Integer number describing the verbosity of console output.
#' Admissible values: 0: no output (default), 1: user messages,
#' 2: debugging-relevant messages.
#' @return An object of class \code{panelSEM}.
#' @references Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible
#' Framework for Studying Causal Effects Using Linear Models. Psychometrika 87,
#' 868–901. https://doi.org/10.1007/s11336-021-09811-z
#' @references Gische, C., West, S.G., & Voelkle, M.C. (2021) Forecasting Causal
#'  Effects of Interventions versus Predicting Future Outcomes, Structural
#'  Equation Modeling: A Multidisciplinary Journal, 28:3, 475-492,
#'  DOI: 10.1080/10705511.2020.1780598
#' @keywords external
#' @export

auxiliary_model <- function(time_varying_variables,
                            time_invariant_variables,
                            linear,
                            heterogeneity,
                            use_open_mx,
                            verbose){


  # print console output
  if(verbose >= 2) logger::log_info('Start.')

  # create empty list
  internal_list <- create_empty_list(verbose = verbose)

  # assign class causalSEM to internal list
  class(internal_list) <- "panelSEM"

  # fill in user-specified information about the model into the list
  internal_list <- fill_in_info_variables(internal_list = internal_list,
                                          time_varying_variables = time_varying_variables,
                                          time_invariant_variables = time_invariant_variables,
                                          linear = linear,
                                          heterogeneity  = heterogeneity,
                                          use_open_mx = use_open_mx)

  # fill in user-specified information about the model into the list
  internal_list <- fill_in_info_model(internal_list = internal_list)

  # fill in model syntax to the list
  internal_list <- fill_in_model_specification(internal_list = internal_list)

  auxiliary_model <- internal_list

  # print console output
  if(verbose >= 2) logger::log_info('End.')

  # return output
  return(auxiliary_model)

}
