
#' fill_RAM_matrices
#'
#' create RAM matrices based on parameter table
#' @param parameter_table data frame with parameters
#' @returns list with C (directed effects), Psi (undirected effects),
#' M (intercepts), and F (filter) matrices
#' @keywords internal
fill_RAM_matrices <- function(parameter_table){

  RAM <- create_RAM_matrices(parameter_table = parameter_table)

  # Fill C matrix with directed effects
  for(i in which(parameter_table$op %in% c("=~", "~"))){

    RAM$C$labels[parameter_table$incoming[i], parameter_table$outgoing[i]] <- ifelse(
      parameter_table$algebra[i] == "",
      # OpenMx wants labels to not be "", so we replace "" with NA
      ifelse(parameter_table$label[i] == "", NA, parameter_table$label[i]),
      # if the parameter is an algebra, we must use the following syntax to access
      # the result in the OpenMx model
      paste0(parameter_table$label[i], "[1,1]")
      )
    RAM$C$values[parameter_table$incoming[i], parameter_table$outgoing[i]] <- parameter_table$value[i]
    RAM$C$free[parameter_table$incoming[i], parameter_table$outgoing[i]] <- parameter_table$free[i]

  }

  # fill Psi matrix with undirected effects
  for(i in which(parameter_table$op %in% c("~~"))){

    RAM$Psi$labels[parameter_table$outgoing[i], parameter_table$incoming[i]] <- ifelse(
      parameter_table$algebra[i] == "",
      # OpenMx wants labels to not be "", so we replace "" with NA
      ifelse(parameter_table$label[i] == "", NA, parameter_table$label[i]),
      # if the parameter is an algebra, we must use the following syntax to access
      # the result in the OpenMx model
      paste0(parameter_table$label[i], "[1,1]")
    )
    RAM$Psi$labels[parameter_table$incoming[i], parameter_table$outgoing[i]] <- ifelse(
      parameter_table$algebra[i] == "",
      # OpenMx wants labels to not be "", so we replace "" with NA
      ifelse(parameter_table$label[i] == "", NA, parameter_table$label[i]),
      # if the parameter is an algebra, we must use the following syntax to access
      # the result in the OpenMx model
      paste0(parameter_table$label[i], "[1,1]")
    )

    RAM$Psi$values[parameter_table$outgoing[i], parameter_table$incoming[i]] <- parameter_table$value[i]
    RAM$Psi$values[parameter_table$incoming[i], parameter_table$outgoing[i]] <- parameter_table$value[i]

    RAM$Psi$free[parameter_table$outgoing[i], parameter_table$incoming[i]] <- parameter_table$free[i]
    RAM$Psi$free[parameter_table$incoming[i], parameter_table$outgoing[i]] <- parameter_table$free[i]
  }

  return(RAM)
}

#' create_RAM_matrices
#'
#' create empty RAM matrices
#' @param parameter_table data frame with parameters
#' @returns list with C (directed effects), Psi (undirected effects),
#' M (intercepts), and F (filter) matrices
#' @keywords internal
create_RAM_matrices <- function(parameter_table){

  variables <- get_variables(parameter_table)
  n_variables <- variables |>
    lengths() |>
    sum()

  C_free <- Psi_free <- Psi_labels <- C_labels <- Psi_values <- C_values <- matrix(0,
                                                                                   nrow = n_variables,
                                                                                   ncol = n_variables,
                                                                                   dimnames = list(c(variables$latents, variables$manifests),
                                                                                                   c(variables$latents, variables$manifests)))
  Psi_labels[] <- NA
  C_labels[]   <- NA
  Psi_free[]   <- FALSE
  C_free[]     <- FALSE

  M_free <- M_labels <- M_values <- matrix(0,
                                           nrow = 1,
                                           ncol = n_variables,
                                           dimnames = list(NULL,
                                                           c(variables$latents, variables$manifests)))
  M_labels[] <- paste0("intercept_", c(variables$latents, variables$manifests))
  M_free[]   <- c(variables$latents, variables$manifests) %in% variables$manifests

  F_values <- cbind(
    matrix(0,
           nrow = length(variables$manifests),
           ncol = length(variables$latents)),
    diag(length(variables$manifests))
  )
  dimnames(F_values) <- list(variables$manifests,
                             c(variables$latents, variables$manifests))
  F_labels   <- F_values
  F_labels[] <- NA
  F_free     <- F_values
  F_free[]   <- FALSE

  return(
    list(
      C = list(values = C_values,
               labels = C_labels,
               free   = C_free),
      Psi = list(values = Psi_values,
                 labels = Psi_labels,
                 free   = Psi_free),
      M = list(values = M_values,
               labels = M_labels,
               free   = M_free),
      F = list(values = F_values,
               labels = F_labels,
               free   = F_free)
    )
  )

}

#' get_variables
#'
#' Get the manifest and latent variables
#' @param parameter_table data frame with parameters
#' @returns list with latents and manifests
#' @keywords internal
get_variables <- function(parameter_table){

  latents <- parameter_table$outgoing[parameter_table$op == "=~"] |>
    unique()

  manifests <- c(parameter_table$outgoing,
                 parameter_table$incoming) |>
    unique()
  manifests <- manifests[!manifests %in% latents]

  return(
    list(
      latents   = latents,
      manifests = manifests)
  )

}
