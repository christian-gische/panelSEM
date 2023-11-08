## Changelog:
# CG  0.0.3 2023-06-06:  changed labels of product terms to "prod_x_y"
# CG  0.0.2 2023-05-24: replaced arguments homogeneity and additive by heterogeneity
#						added argument add_product_variables and corresponding section
# CG  0.0.1 2023-02-06 initial programming

## Documentation
#' @title fill_in_data
#' @description Fill Data Into List
#' @param data A data.frame containing the data in the wide format.
#' @param internal_list A list with various information extracted from the
#'    model.
#' @return The inputted list with several slots in \code{..$info_data} filled
#' in.
#' @references Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible
#' Framework for Studying Causal Effects Using Linear Models. Psychometrika 87,
#' 868–901. https://doi.org/10.1007/s11336-021-09811-z
#' @references Gische, C., West, S.G., & Voelkle, M.C. (2021) Forecasting Causal
#'  Effects of Interventions versus Predicting Future Outcomes, Structural
#'  Equation Modeling: A Multidisciplinary Journal, 28:3, 475-492,
#'  DOI: 10.1080/10705511.2020.1780598

fill_in_data <- function(data,
                         internal_list){

  # print console output
  if(internal_list$control$verbose >= 2) logger::log_info('Start.')

  ## check if argument model is supported
  if(!methods::is(object = internal_list, class2 = "panelSEM"))
    stop("Model of class ", class(internal_list),
         " is not supported. Fit objects must be of class panelSEM")

  if(is.null(data)){
    internal_list$info_data$has_data <- FALSE
    return(internal_list)
  }else{
    internal_list$info_data$has_data <- TRUE
  }

  #TODO: check data argument

  internal_list$info_data$data <- data
  internal_list$info_data$n_obs <- nrow(data)
  internal_list$info_data$n_var <- ncol(data)
  internal_list$info_data$var_names <- colnames(data)

  # add product variables in case of nonlinear models
  if(!internal_list$info_model$use_definition_variables)
    internal_list <- add_product_terms(internal_list = internal_list)

  # print console output
  if(internal_list$control$verbose >= 2) logger::log_info('End.')

  # return internal list
  return(internal_list)

}
