#' Update the dim labels
#'
#' @param file_path The file path of the dim labels file.
#' @returns Save the new dim-labels.csv in the user memory.
#' @import tools utils
#' @export
update_dim_labels <- function(file_path) {

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
  dim_labels <- read_data()
  if (is.null(dim_labels)) {
    stop("file_path incorrect.")
  }

  # code, label columns must appear in the file
  cols <- colnames(dim_labels)
  req_cols <- c("code", "label")
  if (!all(req_cols %in% cols)) {
    stop("code and label are required columns.")
  }

  # Save the file in the user project memory
  user_data_path <- tools::R_user_dir("eurostatRTool", which = "data")
  if (!dir.exists(user_data_path)) {
    dir.create(user_data_path, recursive=TRUE)
  }
  dim_labels_path <- file.path(user_data_path, "dim-labels.csv")
  utils::write.csv(dim_labels, dim_labels_path)

}
