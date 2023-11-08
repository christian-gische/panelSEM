#' find_common_substring
#'
#' returns the largest common substring of a vector of strings.
#'
#' @param strings vector with strings
#' @return string
#' @keywords internal
find_common_substring <- function(strings){

  # translate the strings to numbers representing the individual
  # characters:
  as_raw_char <- sapply(strings,
                        charToRaw,
                        simplify = FALSE)

  string_min_length <- as_raw_char |>
    lengths() |>
    min()

  common_substring <- ""
  for(i in 1:string_min_length){
    base <- as_raw_char[[1]][i]
    for(j in 1:length(as_raw_char)){
      # if there is a difference, we return the previous substring
      if(as_raw_char[[j]][i] != base)
        return(common_substring)
    }
    common_substring <- paste0(common_substring,
                               rawToChar(base))
  }

  return(common_substring)
}
