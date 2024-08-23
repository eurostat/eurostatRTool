#' Update the colors of the lines / bars in the graphs.
#'
#' @param colors_palette The color palette for the line plots.
#' @param bar_chart_colors_palette The color palette for the bar charts.
#' @returns Change the colors_palette in the configuration file.
#' @import configr
#' @export
update_colors_palette <- function(colors_palette = NULL,
                                  bar_chart_colors_palette = NULL) {
  new_config <- c()
  if (!is.null(colors_palette)) {
    new_config <- c(new_config, list(colors_palette = colors_palette))
  }
  if (!is.null(bar_chart_colors_palette)) {
    new_config <- c(new_config, list(bar_chart_colors_palette = bar_chart_colors_palette))
  }
  if (!is.null(new_config)) {
    update_user_config_file(new_config)
  }
}
