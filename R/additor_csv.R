#' Write a raw data table
#'
#' Add new data to the database. If a table exist, append it, if not, create a new table with the name \code{name} if \code{create_new_table = TRUE}.
#'
#' @param create_new_table if \code{TRUE}, create a new table if it does not exist. Default \code{FALSE}, i.e. raises error if the table does not exist.
#'
#' @return returns invisible\code{TRUE}
#' @importFrom DBI dbWriteTable dbExistsTable
#' @export
#'
#' @examples
additor_csv <- function(
  input,
  output,
  create_new_table = FALSE
){
  new_data_pattern <- "\\.rds$"

  input_files <- list.files(
    path = input,
    pattern = new_data_pattern,
    full.names = TRUE,
    recursive = TRUE
  )

# create directory structure if it does not exist -------------------------

  dirs <- list.dirs(
    path = input,
    full.names = FALSE,
    recursive = FALSE
  )

  for (dn in file.path(output, dirs)) {
    dir.create(
      path = dn,
      showWarnings = FALSE,
      recursive = TRUE
    )
  }

# Iterate through measurements --------------------------------------------

  measures <- list.dirs(input, full.names = FALSE, recursive = FALSE)

  for (m in measures) {
    input_files <- list.files(
      path = file.path(input, m),
      pattern = new_data_pattern,
      full.names = FALSE,
      recursive = FALSE
    )
    for (fn_in in input_files) {
      x <- readRDS( file.path(input, m, fn_in) )
      fn_out <- grep("\\.rds$", ".csv", fn_in)
      write.csv(
        x = x,
        file = file.path(output, m, fn_out),
        append = TRUE
      )
    }
  }

# Finalise stuff ----------------------------------------------------------

  invisible(TRUE)
}


