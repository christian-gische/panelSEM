#' set_starting_values
#'
#' changes the starting values of the model object. In case of OpenMx, an OpenMx
#' model is returned, where the starting values are set to those indicated in
#' internal_list$info_parameters$parameter_table$start. In case of lavaan, a
#' parameter table is returned that specifies the starting values in the ustart
#' column
#' @param internal_list internal model list object
#' @returns OpenMx model or lavaan parameter table
#' @keywords internal
set_starting_values <- function(internal_list){

  if(all(is.na(internal_list$info_parameters$parameter_table$start))){
    if(internal_list$info_model$use_open_mx){
      return(internal_list$model_syntax$OpenMx)
    }else{
      return(lavaan::lavaanify(internal_list$model_syntax$lavaan))
    }
  }

  starting_values <- internal_list$info_parameters$parameter_table$start
  names(starting_values) <- internal_list$info_parameters$parameter_table$label
  starting_values <- starting_values[!is.na(starting_values)]

  if(internal_list$info_model$use_open_mx){
    return(set_starting_values_openmx(
      model = internal_list$model_syntax$OpenMx,
      starting_values = starting_values
    ))
  }else{
    return(set_starting_values_lavaan(
      model_syntax = internal_list$model_syntax$lavaan,
      starting_values = starting_values
    ))
  }
}


#' set_starting_values_openmx
#'
#' change the starting values of an OpenMx model object
#' @param model mxModel object
#' @param starting_values labeled vector with values of the starting values
#' @export
#' @examples
#' # Example copied from ?OpenMx::mxRun
#' library(OpenMx)
#' data(demoOneFactor)
#'
#' manifests <- names(demoOneFactor)
#' latents   <- c("G")
#'
#' model <- mxModel(type="RAM",
#'                  manifestVars = names(demoOneFactor),
#'                  latentVars   = "G",
#'                  mxPath(from=latents, to=manifests, labels=paste("b", 1:5, sep="")),
#'                  mxPath(from=manifests, arrows=2, labels=paste("u", 1:5, sep="")),
#'                  mxPath(from=latents  , arrows=2, free=FALSE, values=1.0),
#'                  mxData(cov(demoOneFactor), type="cov", numObs=500))
#' model <- mxRun(model)
#'
#' # get the current parameter values
#' starting_values <- omxGetParameters(model)
#' # change the values
#' starting_values[] <- rnorm(length(starting_values))^2
#'
#' # set starting values
#' model_start <- set_starting_values_openmx(model           = model,
#'                                           starting_values = starting_values)
#' print(omxGetParameters(model_start))
#' print(starting_values)
set_starting_values_openmx <- function(model, starting_values){
  if(is.null(names(starting_values)) |
     !methods::is(starting_values, "vector") |
     !is.numeric(starting_values))
    stop("starting_values must be a numeric vector with names")

  return(OpenMx::omxSetParameters(model = model,
                                  labels = names(starting_values),
                                  values = starting_values))
}

#' set_starting_values_lavaan
#'
#' change the starting values of a lavaan model object.
#' @param model_syntax lavaan model syntax
#' @param starting_values labeled vector with values of the starting values
#' @returns a lavaan parameter table with starting values
#' @export
#' @examples
#' # Example copied from ?lavaan::sem
#' library(lavaan)
#' model <- '
#'   # latent variable definitions
#'      ind60 =~ x1 + x2 + x3
#'      dem60 =~ y1 + a*y2 + b*y3 + c*y4
#'      dem65 =~ y5 + a*y6 + b*y7 + c*y8
#'
#'   # regressions
#'     dem60 ~ ind60
#'     dem65 ~ ind60 + dem60
#'
#'   # residual correlations
#'     y1 ~~ y5
#'     y2 ~~ y4 + y6
#'     y3 ~~ y7
#'     y4 ~~ y8
#'     y6 ~~ y8
#' '
#'
#' fit <- sem(model, data = PoliticalDemocracy)
#'
#' # extract current estimates
#' starting_values <- coef(fit)
#' # change values
#' starting_values[] <- rnorm(length(starting_values))^2
#'
#' new_pt <- set_starting_values_lavaan(model_syntax    = model,
#'                                      starting_values = starting_values)
#' # print new values
#' new_pt
set_starting_values_lavaan <- function(model_syntax, starting_values){
  if(is.null(names(starting_values)) | !is.numeric(starting_values))
    stop("starting_values must be a numeric vector with names")

  parameter_table <- lavaan::lavaanify(model = model_syntax)

  # change labels if none are given
  parameter_table$label[parameter_table$label == ""] <- paste0(parameter_table$lhs,
                                                               parameter_table$op,
                                                               parameter_table$rhs)[parameter_table$label == ""]

  for(i in 1:length(starting_values)){
    current_label <- names(starting_values)[i]
    current_value <- starting_values[i]

    if(!current_label %in% parameter_table$label)
      stop("Could not find the following parameter in the model: ", current_label)

    parameter_table$ustart[parameter_table$label == current_label] <- current_value
  }

  return(parameter_table)
}


