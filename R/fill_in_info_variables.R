## Changelog:
# CG 0.0.3 2023-06-05: split function into fill_in_info_variables
# CG 0.0.2 2023-05-24: replaced arguments homogeneity and additive by heterogeneity
# CG 0.0.1 2023-02-20: initial programming

## Documentation
#' @title Get Information about Variables
#' @description Provide information about the variables in the dynamic panel data model.
#' @param internal_list A list with various information extracted from the
#'    model.
#' @param time_varying_variables List of character vectors containing names of the time-varying
#'  variables. Within each vector, the variable names must be time-ordered starting with the first
#'  measurement ocassion.
#' @param time_invariant_variables List of character vectors containing names of the time-invariant
#'  variables. List must have the same length as list in argument \code{time_varying_variables}.
#' @param linear Logical (TRUE = default / FALSE) indicating if the model is linear in observed variables (TRUE).
#' @param heterogeneity Character vector indicating the type of unobserved heterogeneity. Admissible values are \code{"homogeneous"}, \code{"additive"}, \code{"autoregressive"}, and \code{"cross-lagged"} (or any non-conflicting combination).
#' @param use_open_mx Logical (TRUE / FALSE default) indicating if \code{lavaan} (FALSE) or \code{OpenMx} (TRUE)
#' should be used.
#' @param use_definition_variables When setting linear = FALSE, there are two ways to add interactions
#'  between the previous observation and the time invariant predictors (e.g., y2 = (c_y_x + c_y_x_z2*z2)*x1 + ...)).
#'  First, definition variables can be used (currently only supported by OpenMx). Second, product terms
#'  can be added explicitly. When setting use_definition_variables = FALSE, panelSEM will compute the product terms (e.g., z2*x1).
#'  These will then be used to predict the time varying variables (e.g., y2). Additionally, panelSEM will add
#'  all covariances between product terms (e.g., prod_z2_x1 ~~ prod_z3_x1), product terms and exogenous predictors (e.g., prod_z2_x1 ~~ z3),
#'  and product terms and time varying variables (e.g., prod_z2_x1 ~~ y5). This will result in fairly large models and long run times.
#' @return The inputted internal_list with several slots filled in:
#' \tabular{lll}{
#'  \code{..$n_ocassions}: \code{int(0)}  \tab \tab Number of measurement occasions. \cr
#'  \code{..$n_processes}: \code{int(0)}  \tab \tab Number of dynamic processes. \cr
#'  \code{..$generic_names_time_varying}: \code{data.frame} \tab \tab Table with generic variable names of time-varying variables.\cr
#'  \code{..$user_names_time_varying}: \code{data.frame} \tab \tab Table with user-specified variable names of time-varying variables.\cr
#'  \code{..$generic_names_time_invariant}: \code{data.frame} \tab \tab Table with generic variable names of time-invariant variables.\cr
#'  \code{..$user_names_time_invariant}: \code{data.frame} \tab \tab Table with user-specified variable names of time-invariant variables.}
#' @references Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible
#' Framework for Studying Causal Effects Using Linear Models. Psychometrika 87,
#' 868–901. https://doi.org/10.1007/s11336-021-09811-z
#' @references Gische, C., West, S.G., & Voelkle, M.C. (2021) Forecasting Causal
#'  Effects of Interventions versus Predicting Future Outcomes, Structural
#'  Equation Modeling: A Multidisciplinary Journal, 28:3, 475-492,
#'  DOI: 10.1080/10705511.2020.1780598


# function definition
fill_in_info_variables <- function(internal_list,
                                   time_varying_variables,
                                   time_invariant_variables,
                                   linear,
                                   heterogeneity,
                                   use_open_mx,
                                   use_definition_variables){

  # console output
  if(internal_list$control$verbose >= 2) logger::log_info('Start.')

  # TODO: Argument checks
  # - give warning if homogeneous and additive are both set to conflicting values
  # TODO: DELETE THIS LINE: internal_list <- vector(mode = "list")

  ###########################
  # GENERAL MODEL INFORMATION
  ###########################
  ## extract model information from the arguments
  internal_list$info_model$n_occasions <- length(time_varying_variables[[1]])
  internal_list$info_model$n_processes <- length(time_varying_variables)
  internal_list$info_model$n_time_invariant <- time_invariant_variables |>
    unlist() |>
    unique() |>
    length()
  internal_list$info_model$linear <- linear
  internal_list$info_model$heterogeneity  <- heterogeneity
  internal_list$info_model$use_open_mx <- use_open_mx

  internal_list$info_model$use_definition_variables <- use_definition_variables

  ####################
  # OBSERVED VARIABLES
  ####################
  ## time-varying variables
  ### user-specified names time-varying variables
  table_user_names <- matrix(data = unlist(time_varying_variables),
                             ncol = internal_list$info_model$n_occasions,
                             nrow = internal_list$info_model$n_processes,
                             byrow = TRUE,
                             dimnames = list(
                               paste0("process_",LETTERS[1:internal_list$info_model$n_processes]),
                               as.character(1:internal_list$info_model$n_occasions)
                             ))

  internal_list$info_variables$user_names_time_varying <- table_user_names

  ### names of processes
  user_names_processes <- vector("character",
                                 length = internal_list$info_model$n_processes)

  #TODO: Change back to Christian's implementation
  for (i in 1:internal_list$info_model$n_processes){

    common_substring <- find_common_substring(internal_list$info_variables$user_names_time_varying[i,])

    if(common_substring == ""){
      warning("Could not find a good name for the process of the following variables: ",
              paste0(internal_list$info_variables$user_names_time_varying[i,], collapse = ", "),
              ". Using process_", LETTERS[i])
      common_substring <- paste0("process_", LETTERS[i])
    }

    user_names_processes[i] <- common_substring
  }

  table_names_processes <- matrix(ncol = internal_list$info_model$n_processes,
                                  nrow = 1,
                                  dimnames = list("user_names", NULL))

  table_names_processes[1,] <- user_names_processes

  internal_list$info_variables$names_processes <- table_names_processes

  ## time-invariant variables
  user_names_time_invariant_variables <- time_invariant_variables
  names(user_names_time_invariant_variables) <-
    rownames(table_user_names)

  internal_list$info_variables$info_time_invariant_variables <-
    user_names_time_invariant_variables

  # console output
  if(internal_list$control$verbose >= 2) logger::log_info('End.')

  # return internal list
  return( internal_list )
}

## test/development


