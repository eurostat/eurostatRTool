#' Update the secondary menu, choosing between "storyboard" and "dropdown".
#'
#' @param type "storyboard" or "dropdown"
#' @returns Change the secondary menu type in the configuration file.
#' @import configr
#' @export
update_secondary_menu <- function(type) {

  if (type %in% c("storyboard", "dropdown")) {
    new_config <- list(menu = type)
    update_user_config_file(new_config)
  }

}
