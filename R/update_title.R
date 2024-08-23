#' Update the title of the dashboard
#'
#' @param title The dashboard title.
#' @returns Change the title in the configuration file.
#' @import configr
#' @export
update_title <- function(title) {

  new_config <- list(title = title)
  update_user_config_file(new_config)

}
