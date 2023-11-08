## Changelog:
# CG  0.0.6 2023-06-07: defined objects variables_tv and variables_ti for
#                       the function call of auxiliary models
# CG  0.0.5 2023-05-24: removed OpenMx == TRUE part for linear additive model
# CG  0.0.4 2023-05-24: replaced arguments homogeneity and additive by heterogeneity
# CG  0.0.3 2023-04-24 change argument to internal_list
# CDM 0.0.2 2023-03-01 add OpenMx part for certain starting values
# CG  0.0.1 2023-02-06 initial programming

## Documentation
#' @title Compute Starting Values
#' @description Compute starting values based on simplified auxiliary models
#' (e.g., models without latent variables) which are similar to the target model
#' of interest.
#' @param internal_list A list with various information extracted from the
#'    model.
#' @return The inputted list with the column starting values filled in to the
#' parameter following lists: \code{..$info_parameters$parameter_list_C} and
#' \code{..$info_parameters$parameter_list_Psi}.
#' @references Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible
#' Framework for Studying Causal Effects Using Linear Models. Psychometrika 87,
#' 868–901. https://doi.org/10.1007/s11336-021-09811-z
#' @references Gische, C., West, S.G., & Voelkle, M.C. (2021) Forecasting Causal
#'  Effects of Interventions versus Predicting Future Outcomes, Structural
#'  Equation Modeling: A Multidisciplinary Journal, 28:3, 475-492,
#'  DOI: 10.1080/10705511.2020.1780598

starting_values <- function(internal_list){

  # print console output
  verbose <- internal_list$control$verbose
  if(verbose >= 2) logger::log_info('Start.')

  # extract model information from the arguments
  # TODO: check if we need to define all these objects or if we can directly
  # refer to the internal list
  data <- internal_list$info_data$data

  n_occasions <- internal_list$info_model$n_occasions

  n_processes <- internal_list$info_model$n_processes

  info_time_invariant_variables <-
    internal_list$info_variables$info_time_invariant_variables
  labels_time_varying_variables <- c(info_time_invariant_variables)

  user_names_time_varying <-
    internal_list$info_variables$user_names_time_varying
  labels_time_invariant_variables <- c(info_time_invariant_variables)

  linear <- internal_list$info_model$linear
  heterogeneity <- internal_list$info_model$heterogeneity
  use_open_mx <- internal_list$info_model$use_open_mx

  parameter_table <- internal_list$info_parameters$parameter_table
  parameter_table$start <- NA
  internal_list$info_parameters$parameter_table$start <- NA

  #------------------------------------------
  # linear homogeneous model
  #------------------------------------------

  if(linear == TRUE &&
     identical("homogeneous", sort(heterogeneity)) ){

    # print console output
    if(verbose >= 2) logger::log_info('End.')

    # return output
    return(internal_list)
  }

  #------------------------------------------
  # linear model with additive heterogeneity
  #------------------------------------------

  if(linear == TRUE &&
     identical("additive", sort(heterogeneity))){

    if (is.null(labels_time_invariant_variables)){

      # This part is for the model WITHOUT Z-variables. Let's fill in the
      # "else"-part (i.e., the model WITH Z-variables) below first and
      # then copy paste parts of it into the "if" part.

    } else {

      # One of the most difficult part of the model to get good starting values
      # for are the parameters relating to the initial occasion. The idea in the
      # following is to:
      # 1. compute manifest variables representing the latent traits by computing
      #    means over all time varying variables of later occasions *after* controlling
      #    for the time invariant predictors
      # 2. Based on these manifest proxy variables, we can approximate variances and
      #    covariances for the latent variables
      # 3. Using the manifest proxy and the time invariant predictors,
      #    we can predict the observations at the initial occasion, providing
      #    starting values for the regression coefficients

      # We remove the influence of the z-variables from the observations:
      residualized_data <- residualize_time_varying(user_names_time_varying = user_names_time_varying,
                                                    info_time_invariant_variables = info_time_invariant_variables,
                                                    data = data)

      # Compute manifest proxy for the latent variables:
      latent_traits <- find_latent_traits(parameter_table = parameter_table,
                                          user_names_time_varying = user_names_time_varying)

      latent_proxy <- create_latent_proxy(residualized_data = residualized_data,
                                          user_names_time_varying = user_names_time_varying,
                                          latent_traits = latent_traits)

      ## Approximate variances and covariances of latent traits:
      parameter_table <- starting_values_trait_variances(latent_proxy = latent_proxy,
                                                         parameter_table = parameter_table)

      ## Approximate prediction of initial occasions with time invariant predictors
      # and latent proxy variables
      parameter_table <- starting_values_initial_occasion(data = data,
                                                          user_names_time_varying = user_names_time_varying,
                                                          info_time_invariant_variables = info_time_invariant_variables,
                                                          latent_proxy = latent_proxy,
                                                          parameter_table = parameter_table)

      internal_list$info_parameters$parameter_table <- parameter_table
      # print console output
      if(verbose >= 2) logger::log_info('End.')

      # return output
      return(internal_list)
    }
  }

  # return unchanged list if none of the settings approach matched
  return(internal_list)
}

#' residualize_time_varying
#'
#' Predicts all time varying variables with the time invariant predictors and
#' creates a new data set containing only the residuals of these predictions.
#' @param user_names_time_varying matrix with names of user defined time varying
#' variables
#' @param info_time_invariant_variables list with names of user defined time invariant
#' variables
#' @param data data.frame with data
#' @returns data.frame with residualized data
#' @keywords internal
residualize_time_varying <- function(user_names_time_varying,
                                     info_time_invariant_variables,
                                     data){
  all_z <- info_time_invariant_variables |>
    unlist() |>
    unique()

  residualized <- data
  residualized[,c(user_names_time_varying)] <- NA

  for(i in seq_len(nrow(user_names_time_varying))){
    for(j in seq_len(ncol(user_names_time_varying))){
      if(j == 1){
        # variables at initial occasion are predicted with all z-variables
        residuals_of_time_varying <- lm(paste0(user_names_time_varying[i,j],
                                               "~",
                                               paste0(all_z, collapse = " + ")),
                                        data = data) |>
          residuals()
      }else{
        residuals_of_time_varying <- lm(paste0(user_names_time_varying[i,j],
                                               "~",
                                               paste0(info_time_invariant_variables[[rownames(user_names_time_varying)[i]]], collapse = " + ")),
                                        data = data) |>
          residuals()
      }
      residualized[,user_names_time_varying[i,j]] <- residuals_of_time_varying
    }
  }
  return(residualized)
}


#' find_latent_traits
#'
#' Extracts the names of the latent traits for each of the processes. The initial
#' occasion is not taken into account.
#' @param parameter_table data.frame with parameters
#' @param user_names_time_varying matrix with names of time varying variables
#' @returns vector with names of latent traits for each of the processes
#' @keywords internal
find_latent_traits <- function(parameter_table,
                               user_names_time_varying){

  latents <- c()

  for(process in seq_len(nrow(user_names_time_varying))){

    latents_process <-
      parameter_table$outgoing[
        (parameter_table$incoming %in% user_names_time_varying[process,]) &
          (parameter_table$op == "=~") &
          (parameter_table$value == 1)] |>
      unique()
    if(length(latents_process) != 1)
      stop("Error in number of latent processes")

    latents <- c(latents,
                 latents_process)

  }

  names(latents) <- rownames(user_names_time_varying)
  return(latents)

}

#' create_latent_proxy
#'
#' Computes the rowmeans of the data for all processes using the
#' time varying variables (except for the first occasion). These rowmeans
#' are then used as proxy for the latent traits
#' @param residualized_data residualized data
#' @param user_names_time_varying matrix with names of time varying variables
#' @param latent_traits vector with names of the latent traits
#' @returns matrix with proxy for latent traits
#' @keywords internal
create_latent_proxy <- function(residualized_data,
                                user_names_time_varying,
                                latent_traits){

  latent_proxy <- matrix(NA,
                         nrow = nrow(residualized_data),
                         ncol = nrow(user_names_time_varying),
                         dimnames = list(NULL,
                                         latent_traits))
  # latent proxy are computed as means over the observations (except for
  # occasion 1)

  for(i in seq_len(nrow(user_names_time_varying))){
    latent_proxy[,i] <- apply(
      # select all time varying variables, except for the first one
      residualized_data[,c(user_names_time_varying[i,2:ncol(user_names_time_varying)])],
      # for each row
      1,
      # compute the mean, while removing NAs
      mean,
      na.rm = TRUE)
  }

  return(latent_proxy)
}


#' starting_values_trait_variances
#'
#' Create starting values for the latent traits.
#' @param latent_proxy matrix with proxy variables for latent traits
#' @param parameter_table parameter table
#' @returns parameter_table with added starting values for latent trait variances
#' and covariances
#' @keywords internal
starting_values_trait_variances <- function(latent_proxy,
                                            parameter_table){
  trait_covs <- stats::cov(latent_proxy)
  for(i in rownames(trait_covs)){
    for(j in colnames(trait_covs)){
      loc_in_parameter_table <- which(parameter_table$op == "~~" &
                                        parameter_table$outgoing == i &
                                        parameter_table$incoming == j)
      if(length(loc_in_parameter_table) != 0){
        parameter_table$start[loc_in_parameter_table] <- trait_covs[i,j]
      }
    }
  }
  return(parameter_table)
}

#' starting_values_initial_occasion
#'
#' Create starting values for the initial occasion
#' @param data data.frame with data
#' @param user_names_time_varying matrix with names of user defined time varying
#' variables
#' @param info_time_invariant_variables list with names of user defined time invariant
#' variables
#' @param latent_proxy matrix with proxy variables for latent traits
#' @param parameter_table parameter table
#' @returns parameter_table with added starting values for initial occasion
#' @keywords internal
starting_values_initial_occasion <- function(data,
                                             user_names_time_varying,
                                             info_time_invariant_variables,
                                             latent_proxy,
                                             parameter_table){

  # we compute separate regressions for each of the initial observations
  extended_data <- cbind(data,
                         latent_proxy)

  all_z <- info_time_invariant_variables |>
    unlist() |>
    unique()

  all_latent <- latent_proxy |>
    colnames() |>
    unique()

  all_predictors <- c(all_latent,
                      all_z)

  initial_observations <- user_names_time_varying[,1]

  regressions <- matrix(NA,
                        nrow = length(initial_observations),
                        ncol = length(all_predictors),
                        dimnames = list(initial_observations,
                                        all_predictors))

  residuals <- matrix(NA,
                      nrow = nrow(data),
                      ncol = length(initial_observations),
                      dimnames = list(NULL,
                                      initial_observations))
  covariances <- matrix(NA,
                        nrow = length(initial_observations),
                        ncol = length(initial_observations),
                        dimnames = list(initial_observations,
                                        initial_observations))

  for(initial_observation in initial_observations){

    lm_result <- lm(paste0(initial_observation,
                           "~",
                           paste0(all_predictors, collapse = " + ")),
                    data = extended_data)


    regressions[initial_observation, all_predictors] <- coef(lm_result)[all_predictors]
    residuals[,initial_observation] <- residuals(lm_result)

  }

  for(i in rownames(regressions)){
    for(j in colnames(regressions)){
      loc_in_parameter_table <- which(parameter_table$op %in% c("~", "=~") &
                                        parameter_table$outgoing == j &
                                        parameter_table$incoming == i)
      if(length(loc_in_parameter_table) != 0){
        parameter_table$start[loc_in_parameter_table] <- regressions[i,j]
      }
    }
  }

  initial_covs <- stats::cov(residuals)

  for(i in rownames(initial_covs)){
    for(j in colnames(initial_covs)){
      loc_in_parameter_table <- which(parameter_table$op == "~~" &
                                        parameter_table$outgoing == i &
                                        parameter_table$incoming == j)
      if(length(loc_in_parameter_table) != 0){
        parameter_table$start[loc_in_parameter_table] <- initial_covs[i,j]
      }
    }
  }

  return(parameter_table)
}
