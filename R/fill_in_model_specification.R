## Changelog:
# CG 0.0.4 2023-08-07: changed part on the linear additive model in the
#                      fill_in_model_specification_open_mx function
# CG 0.0.3 2023-05-24: changed preamble to be consistent with other functions
# 					   replaced arguments homogeneity and additive by heterogeneity
# CG 0.0.2 2023-04-18: insert console output for debugging
# JO 0.0.1 2023-04-18: initial programming

## Documentation
#' @title Specify the Model Syntax
#' @description Creates a \code{lavaan} or \code{OpenMx} model based in the user specified arguments in the \code{fit_panel_sem} function.
#' @param internal_list A list with various information extracted from the
#'    model.
#' @param lbound_variances should variances be assigned a lower bound of 1e-4?
#' @return The inputted internal_list with slot \code{internal_list$model_syntax} filled in.
#' @references Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible
#' Framework for Studying Causal Effects Using Linear Models. Psychometrika 87,
#' 868–901. https://doi.org/10.1007/s11336-021-09811-z
#' @references Gische, C., West, S.G., & Voelkle, M.C. (2021) Forecasting Causal
#'  Effects of Interventions versus Predicting Future Outcomes, Structural
#'  Equation Modeling: A Multidisciplinary Journal, 28:3, 475-492,
#'  DOI: 10.1080/10705511.2020.1780598

fill_in_model_specification <- function(internal_list,
                                        lbound_variances){

  # print console output
  if(internal_list$control$verbose >= 2) logger::log_info('Start.')


  if(internal_list$info_model$use_open_mx)
    return(fill_in_model_specification_open_mx(internal_list,
                                               lbound_variances))

  # print console output
  if(internal_list$control$verbose >= 2) logger::log_info('End.')

  return(fill_in_model_specification_lavaan(internal_list,
                                            lbound_variances))
}

#' fill_in_model_specification_open_mx
#'
#' Creates an OpenMx model based in the user specification in internal_list
#' @param internal_list internal list object
#' @param lbound_variances should variances be assigned a lower bound of 1e-4?
#' @return internal_list, where OpenMx model is added to internal_list$model_syntax
fill_in_model_specification_open_mx <- function(internal_list,
                                                lbound_variances){

  # print console output
  if(internal_list$control$verbose >= 2) logger::log_info('Start.')

  parameter_table <- internal_list$info_parameters$parameter_table

  variables <- get_variables(parameter_table)

  RAM <- fill_RAM_matrices(parameter_table = parameter_table)

  # add parameters from parameter table
  if(internal_list$info_data$has_data){
    mx_model <- OpenMx::mxModel(type = "RAM",
                                manifestVars = variables$manifests,
                                latentVars = variables$latents,
                                OpenMx::mxData(observed = internal_list$info_data$data,
                                               type = "raw"),
                                OpenMx::mxMatrix(type = "Full",
                                                 name = "A",
                                                 nrow = nrow(RAM$C$values),
                                                 ncol = ncol(RAM$C$values),
                                                 free = RAM$C$free == 1,
                                                 values = RAM$C$values,
                                                 labels = RAM$C$labels),
                                OpenMx::mxMatrix(type = "Full",
                                                 name = "S",
                                                 nrow = nrow(RAM$Psi$values),
                                                 ncol = ncol(RAM$Psi$values),
                                                 free = RAM$Psi$free == 1,
                                                 values = RAM$Psi$values,
                                                 labels = RAM$Psi$labels),
                                OpenMx::mxMatrix(type = "Full",
                                                 name = "M",
                                                 nrow = nrow(RAM$M$values),
                                                 ncol = ncol(RAM$M$values),
                                                 free = RAM$M$free == 1,
                                                 values = RAM$M$values,
                                                 labels = RAM$M$labels),
                                OpenMx::mxMatrix(type = "Full",
                                                 name = "F",
                                                 nrow = nrow(RAM$F$values),
                                                 ncol = ncol(RAM$F$values),
                                                 free = RAM$F$free == 1,
                                                 values = RAM$F$values,
                                                 labels = RAM$F$labels),
                                OpenMx::mxExpectationRAM(A = "A",
                                                         S = "S",
                                                         F = "F",
                                                         M = "M"))
  }else{

    mx_model <- OpenMx::mxModel(type = "RAM",
                                manifestVars = variables$manifests,
                                latentVars = variables$latents,
                                OpenMx::mxMatrix(type = "Full",
                                                 name = "A",
                                                 nrow = nrow(RAM$C$values),
                                                 ncol = ncol(RAM$C$values),
                                                 free = RAM$C$free == 1,
                                                 values = RAM$C$values,
                                                 labels = RAM$C$labels),
                                OpenMx::mxMatrix(type = "Full",
                                                 name = "S",
                                                 nrow = nrow(RAM$Psi$values),
                                                 ncol = ncol(RAM$Psi$values),
                                                 free = RAM$Psi$free == 1,
                                                 values = RAM$Psi$values,
                                                 labels = RAM$Psi$labels),
                                OpenMx::mxMatrix(type = "Full",
                                                 name = "M",
                                                 nrow = nrow(RAM$M$values),
                                                 ncol = ncol(RAM$M$values),
                                                 free = RAM$M$free == 1,
                                                 values = RAM$M$values,
                                                 labels = RAM$M$labels),
                                OpenMx::mxMatrix(type = "Full",
                                                 name = "F",
                                                 nrow = nrow(RAM$F$values),
                                                 ncol = ncol(RAM$F$values),
                                                 free = RAM$F$free == 1,
                                                 values = RAM$F$values,
                                                 labels = RAM$F$labels),
                                OpenMx::mxExpectationRAM(A = "A",
                                                         S = "S",
                                                         F = "F",
                                                         M = "M"))
  }

  # check for algebras
  if(internal_list$info_parameters$has_algebras){
    # we have to check if all of the parameters used in the algebras
    # are already in the model:
    algebra_parameters <- c()
    for(i in which(parameter_table$algebra != "")){

      target <- parameter_table$label[i]
      algebra <- parameter_table$algebra[i]
      mx_algebra <- OpenMx::mxAlgebraFromString(algebra, name = target)

      # add algebra to model
      mx_model <- OpenMx::mxModel(mx_model,
                                  mx_algebra
      )

      # check the names of the parameters used in the algebra
      algebra_elements <- extract_algebra_elements(mxAlgebra_formula = mx_algebra$formula)
      # remove definition variables
      algebra_parameters <- c(algebra_parameters,
                              algebra_elements[!grepl(pattern = "^data.", x = algebra_elements)])
    }

    algebra_parameters <- unique(algebra_parameters)

    model_parameters <- OpenMx::omxGetParameters(mx_model)

    add_parameters <- algebra_parameters[!algebra_parameters %in% names(model_parameters)]


    mx_model <- OpenMx::mxModel(mx_model,
                                mxMatrix(name = "algebra_parameters",
                                         values = 0,
                                         free = TRUE,
                                         nrow = 1,
                                         ncol = length(add_parameters),
                                         labels = add_parameters))
  }

  internal_list$model_syntax$OpenMx <- mx_model

  # print console output
  if(internal_list$control$verbose >= 2) logger::log_info('End.')

  # return internal list
  return(internal_list)

}

#' fill_in_model_specification_lavaan
#'
#' Creates a lavaan model based in the user specification in internal_list
#' @param internal_list internal list object
#' @param lbound_variances should variances be assigned a lower bound of 1e-4?
#' @return internal_list, where lavaan is added to internal_list$model_syntax
fill_in_model_specification_lavaan <- function(internal_list,
                                               lbound_variances){

  # print console output
  if(internal_list$control$verbose >= 2) logger::log_info('Start.')

  if(internal_list$info_parameters$has_algebras)
    stop("lavaan does not allow for algebras. Try use_open_mx = TRUE")

  if(lbound_variances)
    warning("Using lbound_variances = TRUE with lavaan can result in long run times. ",
            "Consider setting lbound_variances = FALSE or use OpenMx with use_open_mx = TRUE.")

  parameter_table <- internal_list$info_parameters$parameter_table

  if(any(grepl(pattern = "^data.", x = parameter_table$label)))
    stop("lavaan does not allow for definition variables")

  # add parameters from parameter table
  model_syntax <- c()

  for(i in 1:nrow(parameter_table)){

    if(parameter_table$op[i] %in% c("~", "~~")){

      model_syntax <- c(model_syntax,
                        paste0(parameter_table$incoming[i], " ",
                               parameter_table$op[i], " ",
                               ifelse(!parameter_table$free[i],
                                      paste0(parameter_table$value[i], "*"),
                                      paste0(parameter_table$label[i], "*")),
                               parameter_table$outgoing[i])
      )

      if(lbound_variances &&
         parameter_table$free[i] &&
         (parameter_table$outgoing[i] == parameter_table$incoming[i]) &&
         (parameter_table$op[i] == "~~")){
        model_syntax <- c(model_syntax,
                          paste0(parameter_table$label[i], " > 1e-4")
        )
      }

    }else if(parameter_table$op[i] == "=~"){
      model_syntax <- c(model_syntax,
                        paste0(parameter_table$outgoing[i], " ",
                               parameter_table$op[i], " ",
                               ifelse(!parameter_table$free[i],
                                      paste0(parameter_table$value[i], "*"),
                                      paste0(parameter_table$label[i], "*")),
                               parameter_table$incoming[i])
      )
    }else{
      stop("Unknown operator ", parameter_table$op[i], ".")
    }
  }

  model_syntax <- paste0(
    c(paste0("# panelSEM\n#-- Syntax generated with package version ",
             utils::packageVersion("panelSEM"),
             " --"),
      model_syntax),
    collapse = "\n"
  )

  internal_list$model_syntax$lavaan <- model_syntax

  # print console output
  if(internal_list$control$verbose >= 2) logger::log_info('End.')

  # return internal list
  return(internal_list)
}
