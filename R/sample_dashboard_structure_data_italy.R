#' Italy dashboard structure data
#'
#' This is the Italy data for building the dashboard structure for the
#' example dashboard.
#'
#' @format ## `sample_dashboard_structure_data_italy`
#' A data frame with 14 rows and 21 columns:
#' \describe{
#'   \item{box}{The position of the indicator in the main and secondary menus}
#'   \item{title}{The title for the indicator, appearing in the secondary menu}
#'   \item{description}{Indicator description, appearing in the right side box}
#'   \item{graph_title}{The title shown for any graph for the indicator}
#'   \item{timeline}{TRUE if the timeline chart is required for the indicator,
#'   else FALSE or NA}
#'   \item{timeline_ind}{The indicator to use for plotting the timeline chart,
#'   corresponding to one of the codes appearing in the column IND of the sample
#'   data}
#'   \item{timeline_subtitle}{The subtitle to show in the timeline chart}
#'   \item{timeline_source}{The source to show below the timeline chart}
#'   \item{comp_barchart}{TRUE if the comp bar chart chart is required for the
#'   indicator, else FALSE or NA}
#'   \item{comp_barchart_ind}{The indicator to use for plotting the
#'   comp bar chart chart, corresponding to one of the codes appearing in the
#'   column IND of the sample data}
#'   \item{comp_barchart_subtitle}{The subtitle to show in the comp bar chart
#'   chart}
#'   \item{comp_barchart_source}{The source to show below the comp bar chart
#'   chart}
#'   \item{double_scale}{TRUE if the double scale chart is required for the
#'   indicator, else FALSE or NA}
#'   \item{double_scale_ind}{The indicator to use for plotting the
#'   double scale chart, corresponding to one of the codes appearing in the
#'   column IND of the sample data}
#'   \item{double_scale_dim}{The dimension to use for plotting the
#'   double scale chart, corresponding to one of the codes appearing in the
#'   column DIM of the sample data}
#'   \item{double_scale_subtitle}{The subtitle to show in the double scale
#'   chart}
#'   \item{double_scale_source}{The source to show below the double scale
#'   chart}
#'   \item{forecast_chart}{TRUE if the forecast chart is required for the
#'   indicator, else FALSE or NA}
#'   \item{forecast_chart_ind}{The indicator to use for plotting the forecast
#'   chart, corresponding to one of the codes appearing in the column IND of
#'   the sample data}
#'   \item{forecast_chart_xlsx_sheet_name}{The sheet name in the forecast xlsx
#'   file, where the information for the forecast chart is located}
#'   \item{forecast_chart_source}{The source to show below the forecast chart}
#' }
#' @source extdata/dashboard-structure-data-italy.xlsx
"sample_dashboard_structure_data_italy"
