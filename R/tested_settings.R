#' list_append
#'
#' Adds an element at the end of a list
#' @param my_list list to which the element should be added
#' @param element element to add
#' @return list with added element
#' @keywords internal
list_append <- function(my_list, element){
  my_list[[length(my_list)+1]] <- element
  return(my_list)
}

#' tested_settings
#'
#' Returns a list with tested settings for linear and heterogeneity
#' @export
tested_settings <- function(){
  tested <- list()

  tested <- tested |>
    list_append(
      list(
        linear = TRUE,
        heterogeneity = "additive"
      )
    )

  tested <- tested |>
    list_append(
      list(
        linear = TRUE,
        heterogeneity = c("additive", "cross-lagged")
      )
    )

  tested <- tested |>
    list_append(
      list(
        linear = FALSE,
        heterogeneity = c("additive", "cross-lagged")
      )
    )

  return(tested)
}
