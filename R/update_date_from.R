#' Update the starting date for the plots
#'
#' @param date The starting date, in "yyyy-mm-dd" format.
#' @returns Change the starting date in the configuration file.
#' @import configr
#' @export
update_date_from <- function(date) {

  date_format <- as.Date(date,"%Y-%m-%d")
  if (!is.na(date_format)) {
    new_config <- list(date_from = date)
    update_user_config_file(new_config)
  }

}
