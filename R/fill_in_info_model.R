## Changelog:
# CG 0.0.2 2023-05-24: replaced arguments homogeneity and additive by heterogeneity
# CG 0.0.1 2023-02-20: initial programming

## Documentation
#' @title Get Model Information
#' @description Provide information about the model parameters and model matrices of the dynamic panel data model.
#' @param internal_list A list with various information extracted from the
#'    model.
#' @return The inputted internal_list with several slots filled in:
#' \tabular{lll}{
#'  \code{..$info_parameters$RAM}: \code{list}  \tab \tab List with RAM matrices. \cr
#'  \code{..$info_parameters$parameter_table}: \code{data.frame}  \tab \tab Parameter table. \cr
#'  \code{..$info_parameters$algebras}: \code{data.frame}  \tab \tab Any algebra added to the model. \cr
#'  }
#' @references Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible
#' Framework for Studying Causal Effects Using Linear Models. Psychometrika 87,
#' 868–901. https://doi.org/10.1007/s11336-021-09811-z
#' @references Gische, C., West, S.G., & Voelkle, M.C. (2021) Forecasting Causal
#'  Effects of Interventions versus Predicting Future Outcomes, Structural
#'  Equation Modeling: A Multidisciplinary Journal, 28:3, 475-492,
#'  DOI: 10.1080/10705511.2020.1780598
fill_in_info_model <- function(internal_list){

  # print console output
  if(internal_list$control$verbose >= 2) logger::log_info('Start.')

  # TODO: Argument checks

  # Create Parameter Table
  parameter_table <- rbind(
    add_autoregressive_cross_lagged(internal_list = internal_list),
    add_process_residual_variances(internal_list = internal_list),
    add_latent_residual(internal_list = internal_list),
    add_time_invariant_predictors(internal_list = internal_list),
    add_homogeneous_covariances(internal_list = internal_list),
    add_observed_exogenous_covariances(internal_list = internal_list),
    add_product_covariances(internal_list = internal_list)
  )

  internal_list$info_parameters$parameter_table <- parameter_table

  internal_list$info_parameters$has_algebras <- any(parameter_table$algebra != "")

  # print console output
  if(internal_list$control$verbose >= 2) logger::log_info('End.')

  # return internal list
  return( internal_list )
}
