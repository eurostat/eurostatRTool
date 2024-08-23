#' Update the edition of the dashboard
#'
#' @param edition The dashboard edition.
#' @returns Change the edition in the configuration file.
#' @import configr
#' @export
update_edition <- function(edition) {

  new_config <- list(edition = edition)
  update_user_config_file(new_config)

}
