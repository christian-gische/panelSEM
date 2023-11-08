#' fit_homogeneous
#'
#' Fit a homogeneous model to longitudinal data.
#' @param data A \code{data.frame} containing the data with named columns. Data MUST be in wide format. If set to NULL,
#' only the lavaan or OpenMx model will be returned. This model can be used to simulate data or check the
#' specification without providing data.
#' @param time_varying_variables List of character vectors containing names of the time-varying
#'  variables. The number entries in the list corresponds to the number of univariate time-series.
#'  Each character vector contains the time-ordered variable names of a univariate time-series
#'  starting with the first measurement occasion.
#' @param time_invariant_variables List of character vectors containing names of the time-invariant
#'  variables. List must have the same length as list in argument \code{time_varying_variables}.
#' @param use_resamples Logical (TRUE / FALSE = default) indicating if a resampling procedure is used for the computation of starting values and model diagnostics.
#' @param use_open_mx Logical (TRUE / FALSE default) indicating if \code{lavaan} (FALSE) or \code{OpenMx} (TRUE)
#' should be used.
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
#' @export
fit_homogeneous <- function(data,
                            time_varying_variables,
                            time_invariant_variables,
                            use_resamples = FALSE,
                            use_open_mx = TRUE,
                            lbound_variances = use_open_mx,
                            verbose = 0){
  linear <- TRUE
  heterogeneity <- "homogeneous"
  # the following argument is not actually used in this model, but must be specified anyway:
  use_definition_variables <- TRUE

  return(
    fit_panel_sem(data = data,
                  time_varying_variables = time_varying_variables,
                  time_invariant_variables = time_invariant_variables,
                  linear = linear,
                  heterogeneity = heterogeneity,
                  use_resamples = use_resamples,
                  use_open_mx = use_open_mx,
                  use_definition_variables = use_definition_variables,
                  lbound_variances = lbound_variances,
                  verbose = verbose
    )
  )
}

#' fit_linear_additive
#'
#' Fit a linear model with additive heterogeneity to longitudinal data.
#' @param data A \code{data.frame} containing the data with named columns. Data MUST be in wide format. If set to NULL,
#' only the lavaan or OpenMx model will be returned. This model can be used to simulate data or check the
#' specification without providing data.
#' @param time_varying_variables List of character vectors containing names of the time-varying
#'  variables. The number entries in the list corresponds to the number of univariate time-series.
#'  Each character vector contains the time-ordered variable names of a univariate time-series
#'  starting with the first measurement occasion.
#' @param time_invariant_variables List of character vectors containing names of the time-invariant
#'  variables. List must have the same length as list in argument \code{time_varying_variables}.
#' @param use_resamples Logical (TRUE / FALSE = default) indicating if a resampling procedure is used for the computation of starting values and model diagnostics.
#' @param use_open_mx Logical (TRUE / FALSE default) indicating if \code{lavaan} (FALSE) or \code{OpenMx} (TRUE)
#' should be used.
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
#' @export
fit_linear_additive <- function(data,
                                time_varying_variables,
                                time_invariant_variables,
                                use_resamples = FALSE,
                                use_open_mx = TRUE,
                                lbound_variances = use_open_mx,
                                verbose = 0){
  linear <- TRUE
  heterogeneity <- "additive"
  use_definition_variables <- TRUE

  return(
    fit_panel_sem(data = data,
                  time_varying_variables = time_varying_variables,
                  time_invariant_variables = time_invariant_variables,
                  linear = linear,
                  heterogeneity = heterogeneity,
                  use_resamples = use_resamples,
                  use_open_mx = use_open_mx,
                  use_definition_variables = use_definition_variables,
                  lbound_variances = lbound_variances,
                  verbose = verbose
    )
  )

}

#' fit_nonlinear_nonadditive
#'
#' Fit a non-linear model with additive and cross-lagged heterogeneity to longitudinal data.
#' @param data A \code{data.frame} containing the data with named columns. Data MUST be in wide format. If set to NULL,
#' only the lavaan or OpenMx model will be returned. This model can be used to simulate data or check the
#' specification without providing data.
#' @param time_varying_variables List of character vectors containing names of the time-varying
#'  variables. The number entries in the list corresponds to the number of univariate time-series.
#'  Each character vector contains the time-ordered variable names of a univariate time-series
#'  starting with the first measurement occasion.
#' @param time_invariant_variables List of character vectors containing names of the time-invariant
#'  variables. List must have the same length as list in argument \code{time_varying_variables}.
#' @param use_resamples Logical (TRUE / FALSE = default) indicating if a resampling procedure is used for the computation of starting values and model diagnostics.
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
#' @export
fit_nonlinear_nonadditive <- function(data,
                                      time_varying_variables,
                                      time_invariant_variables,
                                      use_resamples = FALSE,
                                      verbose = 0){
  linear <- FALSE
  heterogeneity <- c("additive", "cross-lagged")
  use_definition_variables <- TRUE
  use_open_mx <- TRUE
  lbound_variances <- TRUE

  return(
    fit_panel_sem(data = data,
                  time_varying_variables = time_varying_variables,
                  time_invariant_variables = time_invariant_variables,
                  linear = linear,
                  heterogeneity = heterogeneity,
                  use_resamples = use_resamples,
                  use_open_mx = use_open_mx,
                  use_definition_variables = use_definition_variables,
                  lbound_variances = lbound_variances,
                  verbose = verbose
    )
  )

}
