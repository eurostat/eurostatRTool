#' Update the meta description of the dashboard
#'
#' @param description The meta description.
#' @returns Change the meta description in the configuration file.
#' @import configr
#' @export
update_meta_description <- function(description) {

  new_config <- list(meta_description = description)
  update_user_config_file(new_config)

}
