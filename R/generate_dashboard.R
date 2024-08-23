.pkgglobalenv <- new.env(parent=emptyenv())

#' Generate the dashboard
#'
#' @param output_path The path where to save the html.
#' @param pdf_reports Enable generation of pdf reports.
#' @returns A html.
#' @import rmarkdown htmltools flexdashboard metathis
#' @export
generate_dashboard <- function(output_path, pdf_reports = FALSE) {

  # Assign the output_path to a global variable
  assign("output_path", output_path, envir=.pkgglobalenv)

  # Get all the data
  data <- data()
  geo_labels <- geo_labels()
  forecast_data()
  menu_categories <- menu_categories()
  dashboard_structure_data <-
    dashboard_structure_data(unique(data$IND), unique(data$DIM))
  title <- title()
  edition <- edition()
  value_boxes <- value_boxes()
  colors_palette <- colors_palette()
  bar_chart_colors_palette <- bar_chart_colors_palette()
  overview_text <- overview_text()
  latest_changes_summary <- latest_changes_summary()
  dropdown_menu <- dropdown_menu()
  meta_description <- meta_description()
  states()
  gisco_countries()

  # If a visualisation_files folder exists, delete it
  files_folder <- file.path(output_path, "visualisation_files")
  if (dir.exists(files_folder)) {
    unlink(files_folder, recursive = TRUE, force = TRUE)
  }

  suppressWarnings({
    # If the option is enabled, generate the pdf reports for any indicator
    if (pdf_reports) {
      generate_pdf_reports(output_path, data, geo_labels, dashboard_structure_data,
                           colors_palette, bar_chart_colors_palette, menu_categories)
      pdf_reports <- TRUE
    } else {
      pdf_reports <- FALSE
    }

    # Generate the dashboard
    rmarkdown::render(
      input = file.path(system.file(package = "eurostatRTool"), "rmd", "visualisation.Rmd"),
      output_file = file.path(output_path, "visualisation.html"),
      params = list(data = data,
                    geo_labels = geo_labels,
                    menu_categories = menu_categories,
                    dashboard_structure_data = dashboard_structure_data,
                    title = title,
                    edition = edition,
                    value_boxes = value_boxes,
                    colors_palette = colors_palette,
                    bar_chart_colors_palette = bar_chart_colors_palette,
                    overview_text = overview_text,
                    latest_changes_summary = latest_changes_summary,
                    dropdown_menu = dropdown_menu,
                    pdf_reports = pdf_reports,
                    meta_description = meta_description),
      encoding = 'UTF-8'
    )

  })

}
