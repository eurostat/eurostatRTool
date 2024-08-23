# Generate the pdf report for an indicator
#
# @returns A pdf.
# @import rmarkdown dplyr mapview
generate_pdf_reports <- function(output_path, data, geo_labels,
                                 dashboard_structure_data,
                                 colors_palette, bar_chart_colors_palette,
                                 menu_categories) {

  reports_folder <- file.path(output_path, "visualisation_files", "reports")
  if (!dir.exists(reports_folder)) {
    dir.create(reports_folder, recursive=TRUE)
  }

  menu_cat <- intersect(unique(dashboard_structure_data$menu_position), 1:length(menu_categories))
  for (ii in menu_cat) {
    menu_subpage_data <- dashboard_structure_data %>%
      dplyr::filter(menu_position == ii)
    for (k in 1:max(menu_subpage_data$indicator_position)) {
      # Filter indicator data for menu data
      indicator_subpage_data <- menu_subpage_data %>%
        dplyr::filter(indicator_position == k)

      # Generate the dashboard
      rmarkdown::render(
        input = file.path(system.file(package = "eurostatRTool"), "rmd", "pdf_report.Rmd"),
        output_file = file.path(output_path, "visualisation_files", "reports",
                                paste0(indicator_subpage_data$title[1], "_",
                                       ii, ".pdf")),
        params = list(data = data,
                      geo_labels = geo_labels,
                      dashboard_structure_indicator_data = indicator_subpage_data,
                      colors_palette = colors_palette,
                      bar_chart_colors_palette = bar_chart_colors_palette),
        encoding = 'UTF-8'
      )
    }
  }

  # Delete all the temporary png files
  rmd_folder <- file.path(system.file(package = "eurostatRTool"), "rmd")
  unlink(file.path(rmd_folder, "*.png"))

}


globalVariables("indicator_position")
