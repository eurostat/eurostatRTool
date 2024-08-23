#' Eurostatistics dashboard structure data
#'
#' This is the Eurostatistics data for building the dashboard structure for the
#' example dashboard.
#'
#' @format ## `sample_dashboard_structure_data`
#' A data frame with 26 rows and 37 columns:
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
#'   \item{point_linechart}{TRUE if the point line chart is required for the indicator,
#'   else FALSE or NA}
#'   \item{point_linechart_dim}{The dimension to use for plotting the point line chart,
#'   corresponding to one of the codes appearing in the column DIM of the sample
#'   data}
#'   \item{point_linechart_ind_point_linechart}{The indicator for the point line chart to use for
#'   plotting the point line chart, corresponding to one of the codes appearing in the
#'   column IND of the sample data}
#'   \item{point_linechart_ind_gdp}{The indicator for the gdp to use for
#'   plotting the point line chart, corresponding to one of the codes appearing in the
#'   column IND of the sample data}
#'   \item{point_linechart_subtitle}{The subtitle to show in the point line chart}
#'   \item{point_linechart_source}{The source to show below the timeline chart}
#'   \item{forecast_chart}{TRUE if the forecast chart is required for the
#'   indicator, else FALSE or NA}
#'   \item{forecast_chart_ind}{The indicator to use for plotting the forecast
#'   chart, corresponding to one of the column names appearing in the forecast
#'   xlsx file}
#'   \item{forecast_chart_xlsx_sheet_name}{The sheet name in the forecast xlsx
#'   file, where the information for the forecast chart is located}
#'   \item{forecast_chart_source}{The source to show below the forecast chart}
#'   \item{map}{TRUE if the map chart is required for the indicator, else FALSE
#'   or NA}
#'   \item{map_ind}{The indicator to use for plotting the map chart,
#'   corresponding to one of the codes appearing in the column IND of the
#'   sample data}
#'   \item{map_subtitle}{The subtitle to show in the map chart}
#'   \item{map_source}{The source to show below the map chart}
#'   \item{map_legend_title}{The title to show for the map legend}
#'   \item{table}{TRUE if the table chart is required for the indicator, else
#'   FALSE or NA}
#'   \item{table_ind}{The indicator to use for plotting the table chart,
#'   corresponding to one of the codes appearing in the column IND of the
#'   sample data or one of the column names appearing in the forecast
#'   xlsx file}
#'   \item{table_xlsx_sheet_name}{The sheet name in the forecast xlsx
#'   file, where the information for the table chart is located}
#'   \item{table_column_names}{The column names to show for the table}
#'   \item{table_source}{The source to show below the table chart}
#' }
#' @source extdata/dashboard-structure-data.xlsx
"sample_dashboard_structure_data"
