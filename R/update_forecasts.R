#' Update the forecasts
#'
#' @param file_path The file path of the forecasts file.
#' @returns Save the new forecasts.xlsx in the user memory.
#' @import tools xlsx
#' @export
update_forecasts <- function(file_path) {

  # Try to read the file. If an error occurs, return NA
  read_data <- function() {
    tryCatch({
      # Get all the sheet names in the file
      wb <- xlsx::loadWorkbook(file_path)
      sheet_names <- names(xlsx::getSheets(wb))
      return(sheet_names)
    },
    error = function(e) {
      message("Error reading the file.")
      print(e)
      return(NULL)
    }
    )
  }

  # Try to read the file. If an error occurs, stop
  sheet_names <- read_data()
  if (is.null(sheet_names)) {
    stop("file_path incorrect.")
  }

  # Get the user project data directory
  user_data_path <- tools::R_user_dir("eurostatRTool", which = "data")
  if (!dir.exists(user_data_path)) {
    dir.create(user_data_path, recursive=TRUE)
  }
  forecasts_path <- file.path(user_data_path, "forecasts.xlsx")

  # For any sheet in the file, write the sheet in the file in the user project
  # data directory
  i <- 1
  for (name in sheet_names) {
    sheet_data <- xlsx::read.xlsx(file_path, sheetName=name, header=FALSE)
    colnames(sheet_data) <- sheet_data[1,]
    sheet_data <- sheet_data[2:nrow(sheet_data),]
    if (i == 1) {
      xlsx::write.xlsx(sheet_data, forecasts_path, sheetName=name,
                       row.names=FALSE, showNA=TRUE)
    } else {
      xlsx::write.xlsx(sheet_data, forecasts_path, sheetName=name,
                       append=TRUE, row.names=FALSE, showNA=TRUE)
    }
    i <- i + 1
  }

}
