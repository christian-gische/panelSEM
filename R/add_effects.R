#' add_autoregressive_cross_lagged
#'
#' Creates autoregressive and cross-lagged effects for the observed variables
#' @param internal_list internal list with info_variables
#' @returns data.frame with autoregressive and cross-lagged effects
#' @keywords internal
add_autoregressive_cross_lagged <- function(internal_list){

  observed      <- internal_list$info_variables$user_names_time_varying
  process_names <- internal_list$info_variables$names_processes["user_names",]
  linear        <- internal_list$info_model$linear
  use_definition_variables <- internal_list$info_model$use_definition_variables


  time_invariant_variables     <- internal_list$info_variables$info_time_invariant_variables

  effects <- data.frame()

  # We have to iterate over all time points but the last one and predict the
  # next time point (e.g., x1 -> x2)
  for(i in 1:(ncol(observed)-1)){
    outgoing <- observed[,i]
    incoming <- observed[,i+1]

    for(out in outgoing){
      for(inc in incoming){
        # If the effect of the time_invariant_variables is linear, all we have to do
        # is add an effect of out on inc (out -> inc). The same is true if it is
        # an autoregressive effect or if there are no time invariant variables.
        if(linear |
           (which(incoming == inc) == which(outgoing == out)) |
           is.null(time_invariant_variables[[which(incoming == inc)]])){

          label    <- paste0("c_",
                             process_names[[which(incoming == inc)]],
                             "_",
                             process_names[[which(outgoing == out)]])
          algebra  <- ""
          effects <- rbind(effects,
                           data.frame(
                             outgoing = out,
                             incoming = inc,
                             type     = "directed",
                             op       = "~",
                             location = "C",
                             label    = label,
                             value   = 0.0,
                             algebra = algebra,
                             free    = algebra == ""
                           ))
          next
        }else{
          # If the effect of the time_invariant_variables is non-linear and it is
          # not an autoregressive effect, there is a main effect of the previous
          # observation that is moderated by the time_invariant_variables
          # y2 = (c_y_x + c_y_x_z2*z2)*x1 + ...

          # There are two ways to handle such cases: (1) with definition variables,
          # and (2) with product terms added to the data set
          if(use_definition_variables){
            label <- paste0("c_", process_names[[which(incoming == inc)]], "_", out)

            algebra <- paste0(
              # main effect of previous occasion
              paste0("c_", process_names[[which(incoming == inc)]], "_", process_names[[which(outgoing == out)]]),
              " + ",
              # interaction effect with time invariant variables
              paste0(paste0("c_", process_names[[which(incoming == inc)]], "_", process_names[[which(outgoing == out)]],
                            "_", time_invariant_variables[[which(incoming == inc)]],
                            " * data.", time_invariant_variables[[which(incoming == inc)]]),
                     collapse = " + ")
            )
            effects <- rbind(effects,
                             data.frame(
                               outgoing = out,
                               incoming = inc,
                               type     = "directed",
                               op       = "~",
                               location = "C",
                               label    = label,
                               value   = 0.0,
                               algebra = algebra,
                               free    = algebra == ""
                             ))
          }else{

            # the product terms are already added to the data set and
            # can be used directly

            # add main effect
            effects <- rbind(effects,
                             data.frame(
                               outgoing = out,
                               incoming = inc,
                               type     = "directed",
                               op       = "~",
                               location = "C",
                               label    = paste0("c_", process_names[[which(incoming == inc)]],
                                                 "_", process_names[[which(outgoing == out)]]),
                               value   = 0.0,
                               algebra = "",
                               free    = TRUE
                             ))

            # add interactions
            for(ti in time_invariant_variables[[which(incoming == inc)]]){
              label <- paste0("c_", process_names[[which(incoming == inc)]], "_", process_names[[which(outgoing == out)]],
                              "_", ti)
              out_and_z <- paste0("prod_", ti, "_", out)
              effects <- rbind(effects,
                               data.frame(
                                 outgoing = out_and_z,
                                 incoming = inc,
                                 type     = "directed",
                                 op       = "~",
                                 location = "C",
                                 label    = label,
                                 value   = 0.0,
                                 algebra = "",
                                 free    = TRUE
                               ))
            }
          }
        }
      }
    }
  }

  rownames(effects) <- NULL
  return(effects)

}

#' add_process_residual_variances
#'
#' Creates residual variances for the observed variables
#' @param internal_list internal list with info_variables
#' @returns data.frame with residual variances
#' @keywords internal
add_process_residual_variances <- function(internal_list){

  observed      <- internal_list$info_variables$user_names_time_varying
  process_names <- internal_list$info_variables$names_processes["user_names",]
  effects       <- data.frame()

  for(i in 1:ncol(observed)){
    outgoing <- observed[,i]
    incoming <- outgoing

    if(i == 1){
      # treat first occasion differently
      for(out_index in 1:length(outgoing)){
        for(inc_index in out_index:length(outgoing)){
          # we add all variances and covariances
          effects <- rbind(effects,
                           expand.grid(
                             outgoing = outgoing[[out_index]],
                             incoming = incoming[[inc_index]],
                             type     = "undirected",
                             op       = "~~",
                             location = "Psi",
                             label    = paste0("psi_",
                                               incoming[[inc_index]],
                                               "_",
                                               outgoing[[out_index]]),
                             value   = ifelse(outgoing[[out_index]] == incoming[[inc_index]],
                                              1.0,
                                              0.0),
                             algebra = "",
                             free = TRUE
                           )
          )
        }
      }
      # go to next iteration
      next
    }

    effects <- rbind(effects,
                     data.frame(
                       outgoing = outgoing,
                       incoming = incoming,
                       type     = "undirected",
                       op       = "~~",
                       location = "Psi",
                       label    = paste0("psi_",
                                         process_names,
                                         "_",
                                         process_names),
                       value   = ifelse(outgoing == incoming,
                                        1.0,
                                        0.0),
                       algebra = "",
                       free = TRUE
                     )
    )

  }

  rownames(effects) <- NULL
  return(effects)

}

#' add_latent_residual
#'
#' Creates latent traits for each of the observed variables
#' @param internal_list internal list with info_variables
#' @returns data.frame with latent traits
#' @keywords internal
add_latent_residual <- function(internal_list){

  observed      <- internal_list$info_variables$user_names_time_varying
  process_names <- internal_list$info_variables$names_processes["user_names",]
  effects       <- data.frame()

  if(all(internal_list$info_model$heterogeneity == "homogeneous")){
    # no eta-terms are added
    message("No additive latent trait added.")
    return()
  }

  additive      <- "additive"     %in% internal_list$info_model$heterogeneity
  cross_lagged  <- "cross-lagged" %in% internal_list$info_model$heterogeneity
  if(additive){
    message("Adding additive latent processes.")
  }
  if(cross_lagged){
    message("Adding cross-lagged latent processes.")
  }

  # add etax, etay, ...
  etas_base <- paste0("eta", process_names)
  if(additive){
    etas <- etas_base
  }else{
    etas <- c()
  }
  etas_nonadditive <- c()

  # add etaxy, etayx, ...
  if(cross_lagged){
    for(i in 1:length(process_names)){
      for(j in 1:length(process_names))
        if(i != j)
          etas_nonadditive <- c(etas_nonadditive,
                                paste0("eta", process_names[[i]], process_names[[j]])
          )
    }
  }

  for(i in 1:ncol(observed)){
    incoming <- observed[,i]

    if(i == 1){
      # treat first occasion differently
      if(additive){
        # add additive effect for initial occasion
        for(out in etas){
          for(inc in incoming){

            effects <- rbind(effects,
                             data.frame(
                               outgoing = out,
                               incoming = inc,
                               type     = "directed",
                               op       = "=~",
                               location = "C",
                               label    = paste0("c_",
                                                 inc,
                                                 "_",
                                                 out),
                               value   = 0.0,
                               algebra = "",
                               free = TRUE
                             ))

          }
        }
      }
      if(cross_lagged){
        # add cross-lagged effect for initial occasion
        for(out in etas_nonadditive){
          for(inc in incoming){

            effects <- rbind(effects,
                             data.frame(
                               outgoing = out,
                               incoming = inc,
                               type     = "directed",
                               op       = "=~",
                               location = "C",
                               label    = paste0("c_",
                                                 inc,
                                                 "_",
                                                 out),
                               value   = 0.0,
                               algebra = "",
                               free = TRUE
                             ))

          }
        }
      }
      # go to next iteration
      next
    }

    for(j in 1:length(incoming)){
      if(additive){
        # additive effects at later occasions (time point > 1)
        effects <- rbind(effects,
                         data.frame(
                           outgoing = etas[[j]],
                           incoming = incoming[[j]],
                           type     = "directed",
                           op       = "=~",
                           location = "C",
                           label    = "",
                           value    = 1,
                           algebra  = "",
                           free     = FALSE
                         )
        )
      }

      if(cross_lagged){
        if(i == 1)
          next
        previous_incoming <- observed[,i-1]
        effects <- rbind(effects,
                         data.frame(
                           outgoing = paste0(etas_base[[j]], process_names[which(incoming != incoming[[j]])]),
                           incoming = incoming[[j]],
                           type     = "directed",
                           op       = "=~",
                           location = "C",
                           label    = paste0("data.",previous_incoming[which(incoming != incoming[[j]])]),
                           value    = 0,
                           algebra  = "",
                           free = FALSE
                         )
        )
      }
    }
  }

  # add latent trait variances and covariances
  etas_combined <- c(etas, etas_nonadditive)
  for(out_index in 1:length(etas_combined)){
    for(inc_index in out_index:length(etas_combined)){
      # we add all variances and covariances
      effects <- rbind(effects,
                       expand.grid(
                         outgoing = etas_combined[[out_index]],
                         incoming = etas_combined[[inc_index]],
                         type     = "undirected",
                         op       = "~~",
                         location = "Psi",
                         label    = paste0("psi_",
                                           etas_combined[[inc_index]],
                                           "_",
                                           etas_combined[[out_index]]),
                         value    = ifelse(etas_combined[[out_index]] == etas_combined[[inc_index]],
                                           1.0,
                                           0.0),
                         algebra  = "",
                         free     = TRUE
                       )
      )
    }
  }

  rownames(effects) <- NULL
  return(effects)
}


#' add_time_invariant_predictors
#'
#' Creates exogenous predictors as specified in internal_list$info_variables$info_time_invariant_variables
#' @param internal_list internal list with info_variables
#' @returns data.frame with exogenous predictors
#' @keywords internal
add_time_invariant_predictors <- function(internal_list){

  observed      <- internal_list$info_variables$user_names_time_varying
  process_names <- internal_list$info_variables$names_processes["user_names",]
  effects       <- data.frame()

  time_invariant_variables <- internal_list$info_variables$info_time_invariant_variables

  if(is.null(time_invariant_variables)){
    # no eta-terms are added
    message("No exogenous predictors added.")
    return()
  }

  for(i in 1:ncol(observed)){
    incoming <- observed[,i]

    if(i == 1){
      # treat first occasion differently. Here, we only have
      # the effect of the latent variables eta_x, eta_y, ...
      # on the initial observations x_1, y_1, ...
      for(out in unique(unlist(time_invariant_variables))){
        for(inc in incoming){

          effects <- rbind(effects,
                           data.frame(
                             outgoing = out,
                             incoming = inc,
                             type     = "directed",
                             op       = "~",
                             location = "C",
                             label    = paste0("c_",
                                               inc,
                                               "_",
                                               out),
                             value    = 0.0,
                             algebra  = "",
                             free     = TRUE
                           ))

        }
      }
      # go to next iteration
      next
    }

    for(j in 1:length(incoming)){

      # Note: We are only adding main effects here. All interactions with the
      # variables x1, x2, ... are taken care of by add_autoregressive_cross_lagged.
      # Therefore, we don't have to check if there is anything non-linear etc. going
      # on.
      effects <- rbind(effects,
                       data.frame(
                         outgoing = time_invariant_variables[[j]],
                         incoming = incoming[[j]],
                         type     = "directed",
                         op       = "~",
                         location = "C",
                         label    = paste0("c_", process_names[j], "_",
                                           time_invariant_variables[[j]]),
                         value    = 0.0,
                         algebra  = "",
                         free     = TRUE
                       )
      )
    }
  }

  rownames(effects) <- NULL
  return(effects)
}

#' add_homogeneous_covariances
#'
#' Creates variances between the lagged time varying variables in a
#' homogeneous model and covariances between contemporaneous variables
#' @param internal_list internal list with info_variables
#' @returns data.frame with covariances
#' @keywords internal
add_homogeneous_covariances <- function(internal_list){
  if(!all(internal_list$info_model$heterogeneity == "homogeneous"))
    return()

  observed      <- internal_list$info_variables$user_names_time_varying
  process_names <- internal_list$info_variables$names_processes["user_names",]
  effects       <- data.frame()

  for(i in 2:ncol(observed)){

    # add contemporaneous covariance
    outgoing <- observed[,i]
    incoming <- observed[,i]

    for(out in outgoing){
      for(inc in incoming[which(incoming == out):length(incoming)]){
        # skip variances; they already exist
        if(out == inc)
          next

        effects <- rbind(effects,
                         data.frame(
                           outgoing = out,
                           incoming = inc,
                           type     = "undirected",
                           op       = "~~",
                           location = "Psi",
                           label    = paste0("psi_",
                                             process_names[[which(incoming == inc)]],
                                             "_",
                                             process_names[[which(outgoing == out)]]),
                           value   = ifelse(out == inc,
                                            1.0,
                                            0.0),
                           algebra = "",
                           free    = TRUE
                         ))

      }
    }

    # add cross-lagged covariance
    outgoing <- observed[,i]
    incoming <- observed[,i-1]

    effects <- rbind(effects,
                     data.frame(
                       outgoing = outgoing,
                       incoming = incoming,
                       type     = "undirected",
                       op       = "~~",
                       location = "Psi",
                       label    = paste0("psi_",
                                         incoming,
                                         "_",
                                         outgoing),
                       value   = ifelse(outgoing == incoming,
                                        1.0,
                                        0.0),
                       algebra = "",
                       free    = TRUE
                     ))

  }

  rownames(effects) <- NULL
  return(effects)

}

#' add_observed_exogenous_covariances
#'
#' Add covariances between observed exogenous variables if use_definition_variables = FALSE. These
#' can be computed from the observed data.
#' @param internal_list internal list
#' @returns data.frame with covariances
#' @importFrom stats cov
#' @keywords internal
add_observed_exogenous_covariances <- function(internal_list){

  effects <- c()
  exogenous_names <- unique(unlist(internal_list$info_variables$info_time_invariant_variables))
  if(!internal_list$info_model$use_definition_variables){
    exogenous_names <- c(exogenous_names,
                         unique(unlist(internal_list$info_data$product_names)))
  }

  if(internal_list$info_data$has_data){
    # If data was provided, we compute the observed (co-)variances and fix the parameters
    # to those values.
    N <- internal_list$info_data$n_obs
    product_cov <- ((N-1)/N) * stats::cov(internal_list$info_data$data[,exogenous_names],
                                   use = "all.obs")

    if(any(eigen(product_cov)$values < 0)){
      warning("The covariance matrix of the exogenous variables is not positive definite.")
      free <- TRUE
    }else{
      free <- FALSE
    }
  }else{
    # If no data was provided, we add covariance parameters and do not
    # fix them.
    product_cov <- diag(1, length(exogenous_names))
    dimnames(product_cov) <- list(exogenous_names, exogenous_names)
    free <- TRUE
  }
  for(i in 1:nrow(product_cov)){
    for(j in i:ncol(product_cov)){
      effects <- rbind(effects,
                       data.frame(
                         outgoing = rownames(product_cov)[i],
                         incoming = colnames(product_cov)[j],
                         type     = "undirected",
                         op       = "~~",
                         location = "Psi",
                         label    = paste0("psi_",
                                           rownames(product_cov)[i],
                                           "_",
                                           colnames(product_cov)[j]),
                         value   = product_cov[i, j],
                         algebra = "",
                         free    = free
                       ))
    }
  }

  return(effects)
}

#' add_product_covariances
#'
#' Add covariances between exogenous product terms and x1, x2, ..., y1, y2, ... if use_definition_variables = FALSE
#' @param internal_list internal list
#' @returns data.frame with covariances
#' @keywords internal
add_product_covariances <- function(internal_list){
  if(internal_list$info_model$use_definition_variables){
    return(c())
  }

  exogenous_names <- unique(unlist(internal_list$info_data$product_names))
  process_names <- unique(c(internal_list$info_variables$user_names_time_varying))
  effects <- c()

  for(outgoing in exogenous_names){
    for(incoming in process_names){
      effects <- rbind(effects,
                       data.frame(
                         outgoing = outgoing,
                         incoming = incoming,
                         type     = "undirected",
                         op       = "~~",
                         location = "Psi",
                         label    = paste0("psi_",
                                           outgoing,
                                           "_",
                                           incoming),
                         value   = 0.0,
                         algebra = "",
                         free    = TRUE
                       ))
    }
  }

  return(effects)
}
