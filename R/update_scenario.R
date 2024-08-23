#' Update the scenario, choosing between "eurostat" and "nsi".
#'
#' @param entity "eurostat" or "nsi"
#' @returns Change the entity in the configuration file.
#' @import configr
#' @export
update_scenario <- function(entity) {

  if (entity %in% c("eurostat", "nsi")) {
    new_config <- list(scenario = entity)
    update_user_config_file(new_config)
  }
  else {
    stop("Invalid entity name. Please choose between 'eurostat' and 'nsi'.")
  }

}
