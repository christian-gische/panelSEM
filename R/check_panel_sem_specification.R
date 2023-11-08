#' check_panel_sem_specification
#'
#' checks if the user specified all arguments of fit_panel_sem correctly
#' @param specification list with user specified arguments
#' @return throws error in case of misspecification
#' @keywords internal
check_panel_sem_specification <- function(specification){

  with(data = specification,
       expr = {

         if(is.null(data)){
           warning("Data is NULL. Returning a model without data!")
         }else if((!methods::is(data, "matrix")) && (!is(data, "data.frame"))){
           stop("data must be a matrix or data.frame")
         }

         if(!methods::is(time_varying_variables, "list"))
           stop("time_varying_variables must be a list and not a ", class(time_varying_variables))

         # check if the number of occasions is equal for all processes:
         if(length(unique(lengths(time_varying_variables))) != 1)
           stop("The number of occasions in time_varying_variables must be the same for all processes.")

         if(!methods::is(time_invariant_variables, "list"))
           stop("time_invariant_variables must be a list and not a ", class(time_invariant_variables))

         if(!methods::is(linear, "logical"))
           stop("linear must be a logical and not a ", class(logical))


         if(length(heterogeneity) == 1){
           if(!heterogeneity %in% c("homogeneous", "additive", "cross-lagged"))
             stop("heterogeneity must be one of: ",
                  paste0(c("homogeneous", "additive", "cross-lagged", "c('additive', 'cross-lagged')"), collapse = ", "),
                  ".")
         }else if(length(heterogeneity) == 2){
           if(!all(sort(heterogeneity) == c("additive", "cross-lagged")))
             stop("heterogeneity must be one of: ",
                  paste0(c("homogeneous", "additive", "cross-lagged", "c('additive', 'cross-lagged')"), collapse = ", "),
                  ".")
         }else{
           stop("heterogeneity must be one of: ",
                paste0(c("homogeneous", "additive", "cross-lagged", "c('additive', 'cross-lagged')"), collapse = ", "),
                ".")
         }

         if(!use_definition_variables){
           warning("use_definition_variables = FALSE is experimental. Proceed with caution!")
         }

         if(!methods::is(use_resamples, "logical"))
           stop("use_resamples must be a logical and not a ", class(use_resamples))

         if(!methods::is(linear, "logical"))
           stop("linear must be a logical and not a ", class(linear))

         if(!methods::is(verbose, "numeric"))
           stop("verbose must be an integer and not a ", class(verbose))

         tested <- tested_settings()
         was_tested <- FALSE
         for(i in tested){
           if(i$linear == linear && identical(sort(i$heterogeneity), sort(heterogeneity)))
             was_tested <- TRUE
         }
         if(!was_tested)
           warning("Your current setting for linear and heterogeneity has not yet been tested.",
                   "See tested_settings() for all thoroughly tested settings.")

       })
}
