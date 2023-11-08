## Changelog:
# CG 0.0.2 2023-05-24: replaced arguments homogeneity and additive by heterogeneity
# CG 0.0.1 2023-03-23: initial programming

## Documentation
#' @title Fit Dynamic Panel Data Models
#' @description Fit a model from the class of dynamic panel data models to longitudinal data.
#' Models include linear and nonlinear cross-lagged panel models with additive or nonadditive
#' unobserved heterogeneity.
#' @param data A \code{data.frame} containing the data with named columns. Data MUST be in wide format. If set to NULL,
#' only the lavaan or OpenMx model will be returned. This model can be used to simulate data or check the
#' specification without providing data.
#' @param time_varying_variables List of character vectors containing names of the time-varying
#'  variables. The number entries in the list corresponds to the number of univariate time-series.
#'  Each character vector contains the time-ordered variable names of a univariate time-series
#'  starting with the first measurement occasion.
#' @param time_invariant_variables List of character vectors containing names of the time-invariant
#'  variables. List must have the same length as list in argument \code{time_varying_variables}.
#' @param linear Logical (TRUE = default / FALSE) indicating if the model is linear in observed variables (TRUE).
#' @param heterogeneity Character vector indicating the type of unobserved heterogeneity. Admissible values are \code{"homogeneous"}, \code{"additive"}, \code{"autoregressive"}, and \code{"cross-lagged"} (or any non-conflicting combination).
#' @param use_resamples Logical (TRUE / FALSE = default) indicating if a resampling procedure is used for the computation of starting values and model diagnostics.
#' @param use_open_mx Logical (TRUE / FALSE default) indicating if \code{lavaan} (FALSE) or \code{OpenMx} (TRUE)
#' should be used.
#' @param use_definition_variables When setting linear = FALSE, there are two ways to add interactions
#'  between the previous observation and the time invariant predictors (e.g., y2 = (c_y_x + c_y_x_z2*z2)*x1 + ...)).
#'  First, definition variables can be used (currently only supported by OpenMx). Second, product terms
#'  can be added explicitly. When setting use_definition_variables = FALSE, panelSEM will compute the product terms (e.g., z2*x1).
#'  These will then be used to predict the time varying variables (e.g., y2). Additionally, panelSEM will add
#'  all covariances between product terms (e.g., prod_z2_x1 ~~ prod_z3_x1), product terms and exogenous predictors (e.g., prod_z2_x1 ~~ z3),
#'  and product terms and time varying variables (e.g., prod_z2_x1 ~~ y5). This will result in fairly large models and long run times.
#' @param lbound_variances should variances be assigned a lower bound of 1e-4? This will be set to TRUE for OpenMx models and FALSE for
#' lavaan models. In lavaan, bounds can increase the run time substantially.
#' @param verbose Integer number describing the verbosity of console output.
#' Admissible values: 0: no output (default), 1: user messages,
#' 2: debugging-relevant messages.
#' @return An object of class \code{panelSEM} for which several methods
#' are available including \code{\link{summary.panelSEM}} and
#' \code{\link{print.panelSEM}}.
#' @references Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible
#' Framework for Studying Causal Effects Using Linear Models. Psychometrika 87,
#' 868–901. https://doi.org/10.1007/s11336-021-09811-z
#' @references Gische, C., West, S.G., & Voelkle, M.C. (2021) Forecasting Causal
#'  Effects of Interventions versus Predicting Future Outcomes, Structural
#'  Equation Modeling: A Multidisciplinary Journal, 28:3, 475-492,
#'  DOI: 10.1080/10705511.2020.1780598
#' @keywords external
#' @export
#' @examples
#' library(panelSEM)
#' set.seed(23)
#'
#' time_points <- 10
#'
#' ############################################
#' # set values of population parameters SET 1
#' ############################################
#'
#'
#' # covariance-matrix of the latent traits
#' A_sigma_eta <- matrix(nrow = 2, ncol = 2, c(1, 0.5, 0.5, 1))
#' sigma_epsilon_eta <- t(A_sigma_eta) %*% A_sigma_eta
#'
#' # covariance-matrix of the epsilon_z variables
#' A_sigma_z <- matrix(nrow = 3, ncol = 3, c(1, 0.75, 0.3, 0.75, 1, 0.25,
#'                                           0.3, 0.25, 1))
#' sigma_epsilon_z <- t(A_sigma_z) %*% A_sigma_z
#'
#' # covariance matrix of the initial residuals
#' A_sigma_eps_init <- matrix(nrow = 2, ncol = 2, c(1, 2, 2, 1))
#' sigma_epsilon_init <- t(A_sigma_eps_init) %*% A_sigma_eps_init
#'
#' population_parameters <- data.frame(
#'   N = NA,
#'   # directed effects
#'   ## latent traits
#'   c_x1_etax = -4,
#'   c_x1_etay = 8,
#'
#'   c_y1_etax = -12,
#'   c_y1_etay = 19,
#'
#'   c_x_etax = 1,
#'   c_y_etay = 1,
#'
#'   ## time independent predictors
#'   c_x1_z1 = -5,
#'   c_x1_z2 = -1,
#'   c_x1_z3 = 4,
#'
#'   c_y1_z1 = - 8,
#'   c_y1_z2 = 2,
#'   c_y1_z3 = 6,
#'
#'   c_x_z1 = 0.5,
#'   c_x_z2 = 2,
#'
#'   c_y_z2 = 1.5,
#'   c_y_z3 = 2,
#'
#'   ## autoregressive and cross-lagged effects
#'   c_x_x = 0.05,
#'   c_x_y = 0.4,
#'   c_y_x = -0.6,
#'   c_y_y = 1.2,
#'
#'   # undirected effects
#'   ## trait
#'   psi_etax_etax = sigma_epsilon_eta[1,1],
#'   psi_etax_etay = sigma_epsilon_eta[1,2],
#'   psi_etay_etay = sigma_epsilon_eta[2,2],
#'
#'   ## observed predictors
#'   psi_z1_z1 = sigma_epsilon_z[1,1],
#'   psi_z1_z2 = sigma_epsilon_z[1,2],
#'   psi_z1_z3 = sigma_epsilon_z[1,3],
#'   psi_z2_z2 = sigma_epsilon_z[2,2],
#'   psi_z2_z3 = sigma_epsilon_z[2,3],
#'   psi_z3_z3 = sigma_epsilon_z[3,3],
#'
#'   ## residuals
#'   ### initial time point
#'   psi_x1_x1 = sigma_epsilon_init[1,1],
#'   psi_x1_y1 = sigma_epsilon_init[1,2],
#'   psi_y1_y1 = sigma_epsilon_init[2,2],
#'
#'   ### subsequent time points
#'   psi_x_x = 1,
#'   psi_y_y = 1
#' )
#'
#' population_parameters$N <- 1000
#'
#' population_parameters$time_points <- time_points
#'
#' data <- do.call(what = simulate_data,
#'                 args = population_parameters)
#'
#' model <- fit_panel_sem(data = data,
#'                        time_varying_variables = list(paste0("x", 1:time_points),
#'                                                      paste0("y", 1:time_points)),
#'                        time_invariant_variables = list(c("z1", "z2"),
#'                                                        c("z2", "z3")),
#'                        heterogeneity = "additive")
fit_panel_sem <- function(data,
                          time_varying_variables,
                          time_invariant_variables,
                          linear = TRUE,
                          heterogeneity,
                          use_resamples = FALSE,
                          use_open_mx = TRUE,
                          use_definition_variables = TRUE,
                          lbound_variances = use_open_mx,
                          verbose = 0
                          ){

  initialize_logger()

  # print console output
  if(verbose >= 2) logger::log_info('Start.')

  # check if all arguments are valid
  check_panel_sem_specification(
    list(
      data = data,
      time_varying_variables = time_varying_variables,
      time_invariant_variables = time_invariant_variables,
      linear = linear,
      heterogeneity = heterogeneity,
      use_resamples = use_resamples,
      use_open_mx = use_open_mx,
      use_definition_variables = use_definition_variables,
      verbose = verbose
    )
  )

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
                                          use_open_mx = use_open_mx,
                                          use_definition_variables = use_definition_variables)

  # fill in user-specified data to the list
  internal_list <- fill_in_data(internal_list = internal_list,
                                data = data)

  # fill in user-specified information about the model into the list
  internal_list <- fill_in_info_model(internal_list = internal_list)

  # fill in model syntax to the list
  internal_list <- fill_in_model_specification(internal_list = internal_list,
                                               lbound_variances = lbound_variances)

  # fill in starting values to the list
  #internal_list <- starting_values(internal_list = internal_list)

  internal_list$model <- set_starting_values(internal_list)

  # TODO: decide on default settings for (i) when to include resampling and
  # (ii) the default settings of the resampling procedure
  # if (use_resamples == TRUE){

  # fill in estimation results to the list
  #  internal_list <- fill_in_resample(internal_list = internal_list)

  #}

  # fill in estimation results to the list
  #internal_list <- fill_in_estimates(internal_list = internal_list)

  # fill in diagnostic results into the list
  #internal_list <- fill_in_diagnostics(internal_list = internal_list)

  # fill in warning messages into the list
  #internal_list <- fill_in_warnings(internal_list = internal_list)

  # fill in outputs for generic functions to the list
  #internal_list$tables <- fill_in_print_table(internal_list = internal_list)

  # prepare output
  panelSEM_object <- internal_list

  # console output
  if(verbose >= 2) logger::log_info('End.')

  # return output
  return(panelSEM_object)

}


