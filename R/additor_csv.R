#' Write a raw data table
#'
#' Add new data to the database. If a table exist, append it, if not, create a
#' new table.
#'
#' @param input directory from which to read the data
#' @param output directory to which to write the data to
#'
#' @return returns invisible\code{TRUE}
#'
#' @importFrom utils write.table read.csv
#' @importFrom yaml yaml.load_file
#'
#' @export
additor_csv <- function(
  input,
  output
){
  new_data_pattern <- "\\.csv$"

  smdf <- file.path(input, "sample_metadata.yml")
  smd <- yaml::yaml.load_file( smdf )

  sample_day_id <- paste(smd$name, smd$timestamp, sep = "_")

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


# Copy output to temporary directory --------------------------------------

  tmpdir <- tempfile()
  dir.create( tmpdir, showWarnings = FALSE, recursive = TRUE )
  file.copy(
    from = file.path(output, "."),
    to = tmpdir,
    recursive = TRUE
  )

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
      fn_out <- gsub("\\.csv$", ".csv", fn_in)

      x <- read.csvS( file.path(input, m, fn_in) )

      if (file.exists(file.path(tmpdir, m, fn_out))) {
        x_db <- read.csv(file.path(tmpdir, m, fn_out))
        if (sample_day_id %in% x_db$sample_day_id) {
          stop("Aborting adding to DB - the same sample name has already be added!")
        }
      }

      x$sample_day_id <- sample_day_id

      write.table(
        x = x,
        file = file.path(tmpdir, m, fn_out),
        append = file.exists(file.path(tmpdir, m, fn_out)),
        col.names = !file.exists(file.path(tmpdir, m, fn_out)),
        row.names = FALSE,
        sep = ",",
        dec = ".",
        qmethod = "double"
      )
    }
  }


# Copy output back from tmpdir to output ----------------------------------

  unlink(output, recursive = TRUE, force = TRUE)
  dir.create( output, showWarnings = FALSE, recursive = TRUE )
  file.copy(
    from = file.path(tmpdir, "."),
    to = output,
    recursive = TRUE
  )
  unlink(tmpdir, recursive = TRUE, force = TRUE)

# Finalise stuff ----------------------------------------------------------

  invisible(TRUE)
}


