#' Update the data
#'
#' @param file_path The file path of the data file.
#' @returns Save the new data.csv in the user memory.
#' @import tools utils
#' @export
update_data <- function(file_path) {

  # Try to read the file. If an error occurs, return NA
  read_data <- function() {
    tryCatch({
      data <- utils::read.csv(file_path)
      return(data)
    },
    error = function(e) {
      message("Error reading the file.")
      print(e)
      return(NULL)
    }
    )
  }

  # Try to read the file. If an error occurs, stop
  data <- read_data()
  if (is.null(data)) {
    stop("file_path incorrect.")
  }

  # date, IND, FREQ, DIM, obsValue columns must appear in the file
  cols <- colnames(data)
  req_cols <- c("date", "IND", "FREQ", "DIM", "obsValue")
  if (!all(req_cols %in% cols)) {
    stop("date, IND, FREQ, DIM and obsValue are required columns.")
  }

  # Save the file in the user project memory
  user_data_path <- tools::R_user_dir("eurostatRTool", which = "data")
  if (!dir.exists(user_data_path)) {
    dir.create(user_data_path, recursive=TRUE)
  }
  data_path <- file.path(user_data_path, "data.csv")
  utils::write.csv(data, data_path)

}
