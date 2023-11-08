#' initialize_logger
#'
#' Create a logger layout for the logger R package
#' @returns nothing
#' @keywords internal
initialize_logger <- function(){
  pkg_version <- paste0("panelSEM version ",
                        utils::packageVersion("panelSEM")
                        )
  logger_layout <- logger::layout_glue_generator(format = paste0(pkg_version, ': {fn} {time} {level}: {msg}'))

  logger::log_layout(logger_layout)
}

