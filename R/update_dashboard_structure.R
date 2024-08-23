#' Update the dashboard structure
#'
#' @param file_path The file path of the dashboard structure file.
#' @returns Save the new dashboard-structure.xlsx in the user memory.
#' @import tools xlsx dplyr
#' @export
update_dashboard_structure <- function(file_path) {

  # Try to read the file. If an error occurs, return NA
  read_data <- function() {
    tryCatch({
      dashboard_structure_data <- xlsx::read.xlsx(file_path, sheetIndex=1)
      return(dashboard_structure_data)
      },
      error = function(e) {
        message("Error reading the file.")
        print(e)
        return(NULL)
      }
    )
  }

  # Try to read the file. If an error occurs, stop
  dashboard_structure_data <- read_data()
  if (is.null(dashboard_structure_data)) {
    stop("file_path incorrect.")
  }

  # Required columns must appear in the file
  dashboard_structure_data <- as.data.frame(dashboard_structure_data)
  cols <- colnames(dashboard_structure_data)
  req_cols <- c("box", "title", "description", "graph_title")
  if (!all(req_cols %in% cols)) {
    stop("box, title, description and graph_title are required columns.")
  }

  # Convert all columns to character to avoid errors
  dashboard_structure_data <- dashboard_structure_data %>%
    mutate(across(everything(), as.character))

  # Save the dashboard-structure.xlsx in the user project memory
  user_data_path <- tools::R_user_dir("eurostatRTool", which = "data")
  if (!dir.exists(user_data_path)) {
    dir.create(user_data_path, recursive=TRUE)
  }
  dashboard_structure_path <- file.path(user_data_path, "dashboard-structure.xlsx")
  xlsx::write.xlsx(dashboard_structure_data, dashboard_structure_path)

}
