#' Update the logo
#'
#' @param file_path The file path of the logo.
#' @param position The logo position, right or left.
#' @param link The url to redirect the user to, when clicking on the logo.
#' @returns Save the new logo in the user memory.
#' @import tools magick configr
#' @export
update_logo <- function(file_path = NULL, position = NULL, link = NULL) {

  # If file_path is not null, update logo image
  if (!is.null(file_path) && file.exists(file_path)) {
    # Read the image and scale it with 48px height, to fit the flexdashboard layout
    logo <- magick::image_read(file_path)
    logo <- magick::image_scale(logo, "x48")
    # Write the image as logo.png in the user memory
    user_data_path <- tools::R_user_dir("eurostatRTool", which = "data")
    if (!dir.exists(user_data_path)) {
      dir.create(user_data_path, recursive=TRUE)
    }
    logo_path <- file.path(user_data_path, "logo.png")
    magick::image_write(logo, path = logo_path, format = "png")
  }

  new_config <- c()

  # If position is not null, update logo position
  if (!is.null(position)) {
    if (position %in% c("right", "left")) {
      new_config <- c(new_config, list(logo_position = position))
    } else {
      stop('Position not valid. Please choose between "left" and "right"')
    }
  }

  # If link is not null, update it in the configuration file
  if (!is.null(link)) {
    new_config <- c(new_config, list(logo_link = link))
  }

  # If there is some new configuration parameter, write it in the user memory
  if (!is.null(new_config)) {
    update_user_config_file(new_config)
  }

}
