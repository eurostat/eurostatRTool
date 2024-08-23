#' Update the overview section.
#'
#' @param overview_text The overview text.
#' @param latest_changes_summary The latest changes summary text.
#' @param valuebox1_value The value shown in the first value box.
#' @param valuebox1_text The text shown in the first value box.
#' @param valuebox1_icon The icon shown in the first value box.
#' @param valuebox2_value The value shown in the second value box.
#' @param valuebox2_text The text shown in the second value box.
#' @param valuebox2_icon The icon shown in the second value box.
#' @param valuebox3_value The value shown in the third value box.
#' @param valuebox3_text The text shown in the third value box.
#' @param valuebox3_icon The icon shown in the third value box.
#' @returns Change the overview information in the configuration file.
#' @import configr
#' @export
update_overview <- function(overview_text = NULL, latest_changes_summary = NULL,
                            valuebox1_value = NULL, valuebox1_text = NULL,
                            valuebox1_icon = NULL,
                            valuebox2_value = NULL, valuebox2_text = NULL,
                            valuebox2_icon = NULL,
                            valuebox3_value = NULL, valuebox3_text = NULL,
                            valuebox3_icon = NULL
                            ) {

  new_config <- c(list(overview = TRUE),
                  list(overview_text = overview_text),
                  list(latest_changes_summary = latest_changes_summary),
                  list(valueboxes = TRUE),
                  list(valuebox1_value = valuebox1_value),
                  list(valuebox1_text = valuebox1_text),
                  list(valuebox1_icon = valuebox1_icon),
                  list(valuebox2_value = valuebox2_value),
                  list(valuebox2_text = valuebox2_text),
                  list(valuebox2_icon = valuebox2_icon),
                  list(valuebox3_value = valuebox3_value),
                  list(valuebox3_text = valuebox3_text),
                  list(valuebox3_icon = valuebox3_icon))
  update_user_config_file(new_config)

}
