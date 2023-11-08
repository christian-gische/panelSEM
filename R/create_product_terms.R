#' add_product_terms
#'
#' add product terms of time varying variables and time invariant variables
#' in case of nonlinear models. The product terms will be added to
#' internal_list$info_data$data with the naming convention x3z1.
#' @param internal_list internal list
#' @return internal_list
#' @keywords internal
add_product_terms <- function(internal_list){
  # If linear == FALSE, we can add product terms of
  # observed variables and time invariant predictors
  # to represent the non-linearity
  linear <- internal_list$info_model$linear

  if(linear) # nothing to add
    return(internal_list)

  data          <- internal_list$info_data$data
  observed      <- internal_list$info_variables$user_names_time_varying
  process_names <- internal_list$info_variables$names_processes["user_names",]

  time_invariant_variables <- internal_list$info_variables$info_time_invariant_variables

  if(is.null(time_invariant_variables)){
    # no eta-terms are added
    message("No exogenous predictors added.")
    return()
  }

  info_products <- vector("list", nrow(observed))
  names(info_products) <- process_names

  # iterate over processes
  for(pr in seq_len(nrow(observed))){
    # time_invariant_variables of other processes
    time_invariant_variables_pr <- time_invariant_variables[[-pr]] |>
      unlist()

    # iterate over time points
    for(tp in seq_len(ncol(observed))){
      # add product terms of observation and time_invariant_variables of other processes
      observed_pr_tp <- observed[pr,tp]
      if(length(observed_pr_tp) != 1)
        stop("Error while creating product variables: Too many process variables selected.")

      product_terms <- apply(data[,time_invariant_variables_pr, drop = FALSE],
                             2, function(x) x*data[,observed_pr_tp])
      colnames(product_terms) <- paste0("prod_",
                                        time_invariant_variables_pr,
                                        "_",
                                        observed_pr_tp)

      data <- cbind(data, product_terms)

      info_products[[process_names[pr]]][[tp]] <- colnames(product_terms)
    }

    internal_list$info_data$product_names  <- info_products
    internal_list$info_data$data <- data
  }

  return(internal_list)
}
