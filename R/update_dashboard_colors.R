#' Update the dashboard colors
#'
#' @param primary_background_color The background color for the navbar.
#' @param primary_color The text color for the navbar.
#' @param secondary_background_color The background color when hovering or
#' selecting an item from the main menu.
#' @param secondary_color The text color when hovering or selecting an item
#' from the main menu.
#' @param navbar_border_color The border color for the navbar.
#' @param storyboard_background_color The background color for the unhovered
#' items from the storyboard menu.
#' @param storyboard_color The text color for the unhovered items from the
#' storyboard menu.
#' @param storyboard_border_color The border color for the selected items from
#' the storyboard menu.
#' @param on_hover_storyboard_background_color The background color when
#' hovering items from the storyboard menu.
#' @param on_hover_storyboard_color The text color when hovering items from
#' the storyboard menu.
#' @param overview_text_background_color The background color for the overview
#' text (first box appearing in the overview section).
#' @param overview_text_color The text color for the overview text (first box
#' appearing in the overview section).
#' @param latest_changes_summary_background_color The background color for the
#' latest changes summary (second box appearing in the overview section).
#' @param latest_changes_summary_color The text color for the latest changes
#' summary (second box appearing in the overview section).
#' @param valuebox1_background_color The background color for the first value
#' box in the overview section.
#' @param valuebox1_color The text color for the first value box in the
#' overview section.
#' @param valuebox2_background_color The background color for the second value
#' box in the overview section.
#' @param valuebox2_color The text color for the second value box in the
#' overview section.
#' @param valuebox3_background_color The background color for the third value
#' box in the overview section.
#' @param valuebox3_color The text color for the third value box in the
#' overview section.
#' @param description_background_color The background color for the description
#' box on the right side.
#' @param description_color The text color for the description box on the right
#' side.
#' @param mobile_chart_title_background_color The background color for the
#' secondary menu items in the mobile version, when the storyboard is disabled.
#' @param mobile_chart_title_color The text color for the secondary menu items
#' in the mobile version, when the storyboard is disabled.
#' @returns Save the new colors.css in the user memory.
#' @import tools
#' @export
update_dashboard_colors <- function(
    primary_background_color = "#004494", # EC Blue
    primary_color = "white",
    secondary_background_color = "#FFD617", # EC Yellow
    secondary_color = "black",
    navbar_border_color = "#002F67", # EC Blue 130
    storyboard_background_color = "#BFD0E4", # EC Blue 25
    storyboard_color = "black",
    storyboard_border_color = "#FBC11D", # EC Yellow 110
    on_hover_storyboard_background_color = "#7FA1C9", # EC Blue 50
    on_hover_storyboard_color = "white",
    overview_text_background_color = "white",
    overview_text_color = "black",
    latest_changes_summary_background_color = "#404040", # EC Grey
    latest_changes_summary_color = "white",
    valuebox1_background_color = "#006FB4", # EC Blue N
    valuebox1_color = "white",
    valuebox2_background_color = "#F29527", # EC Orange
    valuebox2_color = "black",
    valuebox3_background_color = "#467A39", # EC Green
    valuebox3_color = "white",
    description_background_color = "#EBEBEB", # EC Grey 10
    description_color = "black",
    mobile_chart_title_background_color = "#F2F5F9", # EC Blue 5
    mobile_chart_title_color = "#004494" # EC Blue
    ) {

  # Get the user project data directory
  user_data_path <- tools::R_user_dir("eurostatRTool", which = "data")
  if (!dir.exists(user_data_path)) {
    dir.create(user_data_path, recursive=TRUE)
  }
  colors_path <- file.path(user_data_path, "colors.css")

  file <- file(colors_path, open = 'w')
  write(":root {", file, sep="\n")

  write(paste0("--primary-background-color: ", primary_background_color, ";"), file, sep="\n")
  write(paste0("--primary-color: ", primary_color, ";"), file, sep="\n")
  write(paste0("--secondary-background-color: ", secondary_background_color, ";"), file, sep="\n")
  write(paste0("--secondary-color: ", secondary_color, ";"), file, sep="\n")
  write(paste0("--navbar-border-color: ", navbar_border_color, ";"), file, sep="\n")
  write(paste0("--storyboard-background-color: ", storyboard_background_color, ";"), file, sep="\n")
  write(paste0("--storyboard-color: ", storyboard_color, ";"), file, sep="\n")
  write(paste0("--storyboard-border-color: ", storyboard_border_color, ";"), file, sep="\n")
  write(paste0("--on-hover-storyboard-background-color: ", on_hover_storyboard_background_color, ";"), file, sep="\n")
  write(paste0("--on-hover-storyboard-color: ", on_hover_storyboard_color, ";"), file, sep="\n")
  write(paste0("--overview-text-background-color: ", overview_text_background_color, ";"), file, sep="\n")
  write(paste0("--overview-text-color: ", overview_text_color, ";"), file, sep="\n")
  write(paste0("--latest-changes-summary-background-color: ", latest_changes_summary_background_color, ";"), file, sep="\n")
  write(paste0("--latest-changes-summary-color: ", latest_changes_summary_color, ";"), file, sep="\n")
  write(paste0("--valuebox1-background-color: ", valuebox1_background_color, ";"), file, sep="\n")
  write(paste0("--valuebox1-color: ", valuebox1_color, ";"), file, sep="\n")
  write(paste0("--valuebox2-background-color: ", valuebox2_background_color, ";"), file, sep="\n")
  write(paste0("--valuebox2-color: ", valuebox2_color, ";"), file, sep="\n")
  write(paste0("--valuebox3-background-color: ", valuebox3_background_color, ";"), file, sep="\n")
  write(paste0("--valuebox3-color: ", valuebox3_color, ";"), file, sep="\n")
  write(paste0("--description-background-color: ", description_background_color, ";"), file, sep="\n")
  write(paste0("--description-color: ", description_color, ";"), file, sep="\n")
  write(paste0("--mobile-chart-title-background-color: ", mobile_chart_title_background_color, ";"), file, sep="\n")
  write(paste0("--mobile-chart-title-color: ", mobile_chart_title_color, ";"), file, sep="\n")

  write("}", file, sep="\n")
  close(file)

}
