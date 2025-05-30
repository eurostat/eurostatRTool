#' Update the dim labels
#'
#' @param file_path The file path of the dim labels file.
#' @returns Save the new dim-labels.csv in the user memory.
#' @import tools utils
#' @export
update_dim_labels <- function(file_path) {
  
  # Try to read the file with UTF-8 encoding
  read_data <- function() {
    tryCatch({
      data <- utils::read.csv(file_path, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
      
      # Ensure character columns are explicitly encoded
      char_cols <- sapply(data, is.character)
      data[char_cols] <- lapply(data[char_cols], function(x) {
        Encoding(x) <- "UTF-8"
        x
      })
      
      return(data)
    },
    error = function(e) {
      message("Error reading the file.")
      print(e)
      return(NULL)
    })
  }
  
  dim_labels <- read_data()
  if (is.null(dim_labels)) {
    stop("file_path incorrect.")
  }
  
  # code, label columns must appear in the file
  if (!all(c("code", "label") %in% names(dim_labels))) {
    stop("Columns 'code' and 'label' are required in the dim labels file.")
  }
  
  # Save the file in the user project memory using UTF-8
  user_data_path <- tools::R_user_dir("eurostatRTool", which = "data")
  if (!dir.exists(user_data_path)) {
    dir.create(user_data_path, recursive = TRUE)
  }
  dim_labels_path <- file.path(user_data_path, "dim-labels.csv")
  utils::write.csv(dim_labels, dim_labels_path, row.names = FALSE, fileEncoding = "UTF-8")
}
