#' Reset the package to its default version
#'
#' Delete user package data and configuration, to reset the package to its
#' default files and settings.
#'
#' @returns Delete all the user package files
#' @import tools
#' @export
reset_to_default <- function() {

  # Delete all user package data
  user_data_path <- tools::R_user_dir("eurostatRTool", which = "data")
  if (dir.exists(user_data_path)) {
    unlink(user_data_path, recursive = TRUE, force = TRUE)
  }

  # Delete the user package configuration
  user_config_path <- tools::R_user_dir("eurostatRTool", which = "config")
  if (dir.exists(user_config_path)) {
    unlink(user_config_path, recursive = TRUE, force = TRUE)
  }

}
