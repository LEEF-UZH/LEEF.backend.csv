#' Register the cv backend in the LEEF package
#'
#' @return invisibly \code{TRUE} when completed successful
#'
#' @export
#'
register <- function() {
  if (is.null(system.file(package = "LEEF"))) {
    stop("This function requres the package to be installed!")
  }

  LEEF::add_additor( additor_csv )
  ##
  invisible(TRUE)
}

