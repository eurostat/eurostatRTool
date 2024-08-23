# FUNCTIONS FOR GENERATING THE TAB BOXES
# NONE OF THESE FUNCTIONS ARE EXPORTED


# Add source footer to a graph
#
# @returns A list with the two figs with source as footer.
# @import htmltools
add_footer <- function(figs, chart_source) {

  source <- paste0("<i>Source</i>: ", chart_source)
  figs$fig <- htmltools::browsable(htmltools::tagList(
    figs$fig,
    htmltools::div(htmltools::HTML(source), class="footer")
  ))
  figs$small_fig <- htmltools::browsable(htmltools::tagList(
    figs$small_fig,
    htmltools::div(htmltools::HTML(source), class="footer")
  ))
  return(figs)

}


# Add graph and its footer to the same figs
#
# @returns The a list with two tab boxes, one for desktop view and one for mobile view.
# @import htmltools
add_graph <- function(figs, chart_type, chart_source, data, geo_labels,
                      colors_palette, indicator, geo = "", chart_title = "",
                      chart_subtitle = "", dec_digit_format = "") {

  new_figs <- generate_chart(chart_type, data, geo_labels, colors_palette,
                             indicator, geo, chart_title,
                             chart_subtitle, dec_digit_format)
  source <- paste0("<i>Source</i>: ", chart_source)
  figs$fig <- htmltools::browsable(htmltools::tagList(
    figs$fig,
    new_figs$fig,
    htmltools::div(htmltools::HTML(source), class="footer")
  ))
  figs$small_fig <- htmltools::browsable(htmltools::tagList(
    figs$small_fig,
    new_figs$small_fig,
    htmltools::div(htmltools::HTML(source), class="footer")
  ))
  return(figs)

}


# Generate the dashboard tab boxes
#
# @returns The a list with two tab boxes, one for desktop view and one for mobile view.
# @import shiny shinydashboard htmltools
generate_tab_boxes <- function(data, geo_labels, colors_palette,
                               bar_chart_colors_palette, indicator_subpage_data,
                               tabset_id) {

  # Create an empty dataframe to save figs and icons for the different
  # visualisations to show
  fig1 <- NULL
  fig2 <- NULL
  fig3 <- NULL
  fig4 <- NULL
  fig5 <- NULL
  fig6 <- NULL
  small_fig1 <- NULL
  small_fig2 <- NULL
  small_fig3 <- NULL
  small_fig4 <- NULL
  small_fig5 <- NULL
  small_fig6 <- NULL
  icon1 <- NULL
  icon2 <- NULL
  icon3 <- NULL
  icon4 <- NULL
  icon5 <- NULL
  icon6 <- NULL
  n <- 0

  # If timeline is TRUE
  if (isTRUE(indicator_subpage_data[1,]$timeline)) {
    n <- n + 1
    timeline_figs <- timeline_chart(data, geo_labels, colors_palette,
                                    indicator_subpage_data[1,]$timeline_ind,
                                    indicator_subpage_data[1,]$graph_title,
                                    indicator_subpage_data[1,]$timeline_subtitle,
                                    ".1f")
    timeline_figs <- add_footer(timeline_figs, indicator_subpage_data[1,]$timeline_source)
    if (nrow(indicator_subpage_data) > 1) {
      for(i in 2:nrow(indicator_subpage_data)) {
        timeline_figs <- add_graph(timeline_figs, "timeline",
                                   indicator_subpage_data[i,]$timeline_source,
                                   data, geo_labels, colors_palette,
                                   indicator_subpage_data[i,]$timeline_ind, NULL,
                                   indicator_subpage_data[i,]$graph_title,
                                   indicator_subpage_data[i,]$timeline_subtitle,
                                   ".1f")
      }
    }
    assign(paste0("fig", n), timeline_figs$fig)
    assign(paste0("small_fig", n), timeline_figs$small_fig)
    assign(paste0("icon", n), shiny::icon("chart-line"))
  }

  # If comp barchart is TRUE
  if (isTRUE(indicator_subpage_data[1,]$comp_barchart)) {
    n <- n + 1
    bar_figs <- bar_chart(data, geo_labels, bar_chart_colors_palette,
                          indicator_subpage_data[1,]$comp_barchart_ind,
                          indicator_subpage_data[1,]$graph_title,
                          indicator_subpage_data[1,]$comp_barchart_subtitle, ".1f")
    bar_figs <- add_footer(bar_figs, indicator_subpage_data[1,]$comp_barchart_source)
    if (nrow(indicator_subpage_data) > 1) {
      for(i in 2:nrow(indicator_subpage_data)) {
        bar_figs <- add_graph(bar_figs, "cross_country",
                              indicator_subpage_data[i,]$comp_barchart_source,
                              data, geo_labels, bar_chart_colors_palette,
                              indicator_subpage_data[i,]$comp_barchart_ind, NULL,
                              indicator_subpage_data[i,]$graph_title,
                              indicator_subpage_data[i,]$comp_barchart_subtitle,
                              ".1f")
      }
    }
    assign(paste0("fig", n), bar_figs$fig)
    assign(paste0("small_fig", n), bar_figs$small_fig)
    assign(paste0("icon", n), shiny::icon("chart-column"))
  }

  # If point line chart is TRUE
  if (isTRUE(indicator_subpage_data[1,]$point_linechart)) {
    n <- n + 1
    ecoin_figs <- ecoin_chart(data, colors_palette,
                              indicator_subpage_data[1,]$point_linechart_dim,
                              indicator_subpage_data[1,]$point_linechart_ind_point_linechart,
                              indicator_subpage_data[1,]$point_linechart_ind_gdp,
                              indicator_subpage_data[1,]$graph_title,
                              indicator_subpage_data[1,]$point_linechart_subtitle)
    ecoin_figs <- add_footer(ecoin_figs, indicator_subpage_data[1,]$point_linechart_source)
    if (nrow(indicator_subpage_data) > 1) {
      for(i in 2:nrow(indicator_subpage_data)) {
        ecoin_figs <- add_graph(ecoin_figs, "ecoin",
                               indicator_subpage_data[i,]$point_linechart_source,
                               data, indicator_subpage_data[i,]$point_linechart_dim,
                               colors_palette,
                               indicator_subpage_data[i,]$point_linechart_ind_point_linechart,
                               indicator_subpage_data[i,]$point_linechart_ind_gdp,
                               indicator_subpage_data[i,]$graph_title,
                               indicator_subpage_data[i,]$point_linechart_subtitle,
                               "")
      }
    }
    assign(paste0("fig", n), ecoin_figs$fig)
    assign(paste0("small_fig", n), ecoin_figs$small_fig)
    assign(paste0("icon", n), shiny::icon("chart-line"))
  }

  # If double_scale is TRUE
  if (isTRUE(indicator_subpage_data[1,]$double_scale)) {
    n <- n + 1
    double_scale_figs <-
      double_scale_chart(data, geo_labels, colors_palette,
                         indicator_subpage_data[1,]$double_scale_ind,
                         indicator_subpage_data[1,]$double_scale_dim,
                         indicator_subpage_data[1,]$graph_title,
                         indicator_subpage_data[1,]$double_scale_subtitle, ".1f")
    double_scale_figs <- add_footer(double_scale_figs, indicator_subpage_data[1,]$double_scale_source)
    if (nrow(indicator_subpage_data) > 1) {
      for(i in 2:nrow(indicator_subpage_data)) {
        double_scale_figs <-
          add_graph(double_scale_figs, "double_scale",
                    indicator_subpage_data[i,]$double_scale_source,
                    data, geo_labels, colors_palette,
                    indicator_subpage_data[i,]$double_scale_ind,
                    indicator_subpage_data[i,]$double_scale_dim,
                    indicator_subpage_data[i,]$graph_title,
                    indicator_subpage_data[i,]$double_scale_subtitle, ".1f")
      }
    }
    assign(paste0("fig", n), double_scale_figs$fig)
    assign(paste0("small_fig", n), double_scale_figs$small_fig)
    assign(paste0("icon", n), shiny::icon("chart-area"))
  }

  # If forecast is TRUE
  if (isTRUE(indicator_subpage_data[1,]$forecast_chart)) {
    n <- n + 1
    forecast_figs <-
      forecast_chart(colors_palette, indicator_subpage_data[1,]$forecast_chart_ind,
                     indicator_subpage_data[1,]$forecast_chart_xlsx_sheet_name,
                     indicator_subpage_data[1,]$graph_title)
    forecast_figs <- add_footer(forecast_figs, indicator_subpage_data[1,]$forecast_chart_source)
    if (nrow(indicator_subpage_data) > 1) {
      for(i in 2:nrow(indicator_subpage_data)) {
        forecast_figs <-
          add_graph(forecast_figs, "forecast",
                    indicator_subpage_data[i,]$forecast_chart_source,
                    NULL, geo_labels, colors_palette, indicator_subpage_data[i,]$forecast_chart_ind,
                    indicator_subpage_data[i,]$forecast_chart_xlsx_sheet_name,
                    indicator_subpage_data[i,]$graph_title, "", "")
      }
    }
    assign(paste0("fig", n), forecast_figs$fig)
    assign(paste0("small_fig", n), forecast_figs$small_fig)
    assign(paste0("icon", n), shiny::icon("chart-bar"))
  }

  # If map is TRUE
  if (isTRUE(indicator_subpage_data[1,]$map)) {
    n <- n + 1
    map_figs <- map(data, geo_labels, indicator_subpage_data[1,]$map_ind,
                    indicator_subpage_data[1,]$graph_title,
                    indicator_subpage_data[1,]$map_subtitle,
                    indicator_subpage_data[1,]$map_legend_title)
    map_figs <- add_footer(map_figs, indicator_subpage_data[1,]$map_source)
    if (nrow(indicator_subpage_data) > 1) {
      for(i in 2:nrow(indicator_subpage_data)) {
        map_figs <- add_graph(map_figs, "map",
                              indicator_subpage_data[i,]$map_source,
                              data, geo_labels, NULL,
                              indicator_subpage_data[i,]$map_ind, NULL,
                              indicator_subpage_data[i,]$graph_title,
                              indicator_subpage_data[i,]$map_subtitle,
                              indicator_subpage_data[i,]$map_legend_title)
      }
    }
    assign(paste0("fig", n), map_figs$fig)
    assign(paste0("small_fig", n), map_figs$small_fig)
    assign(paste0("icon", n), shiny::icon("map"))
  }

  # If table is TRUE
  if (isTRUE(indicator_subpage_data[1,]$table)) {
    n <- n + 1
    table_figs <-
      table(data, geo_labels, indicator_subpage_data[1,]$graph_title,
            indicator_subpage_data[1,]$table_xlsx_sheet_name,
            indicator_subpage_data[1,]$table_ind,
            indicator_subpage_data[1,]$table_column_names)
    table_figs <- add_footer(table_figs, indicator_subpage_data[1,]$table_source)
    if (nrow(indicator_subpage_data) > 1) {
      for(i in 2:nrow(indicator_subpage_data)) {
        table_figs <-
          add_graph(table_figs, "table",
                    indicator_subpage_data[i,]$forecast_chart_source,
                    data, geo_labels, NULL, indicator_subpage_data[i,]$table_ind,
                    indicator_subpage_data[i,]$table_xlsx_sheet_name,
                    indicator_subpage_data[i,]$graph_title,
                    indicator_subpage_data[i,]$table_column_names, "")
      }
    }
    assign(paste0("fig", n), table_figs$fig)
    assign(paste0("small_fig", n), table_figs$small_fig)
    assign(paste0("icon", n), shiny::icon("table"))
  }

  tab_box <- NULL
  small_tab_box <- NULL
  if (n == 1) {
    # TabBox
    tab_box <-
      shinydashboard::tabBox(
        width = NULL,
        title = "",
        id = tabset_id,
        shiny::tabPanel("", fig1, icon = icon1),
      )
    small_tab_box <-
      shinydashboard::tabBox(
        width = NULL,
        title = "",
        id = tabset_id,
        shiny::tabPanel("", small_fig1, icon = icon1),
      )
  } else if (n == 2) {
    # TabBox
    tab_box <-
      shinydashboard::tabBox(
        width = NULL,
        title = "",
        id = tabset_id,
        shiny::tabPanel("", fig1, icon = icon1),
        shiny::tabPanel("", fig2, icon = icon2),
      )
    small_tab_box <-
      shinydashboard::tabBox(
        width = NULL,
        title = "",
        id = tabset_id,
        shiny::tabPanel("", small_fig1, icon = icon1),
        shiny::tabPanel("", small_fig2, icon = icon2),
      )
  } else if (n == 3) {
    # TabBox
    tab_box <-
      shinydashboard::tabBox(
        width = NULL,
        title = "",
        id = tabset_id,
        shiny::tabPanel("", fig1, icon = icon1),
        shiny::tabPanel("", fig2, icon = icon2),
        shiny::tabPanel("", fig3, icon = icon3),
      )
    small_tab_box <-
      shinydashboard::tabBox(
        width = NULL,
        title = "",
        id = tabset_id,
        shiny::tabPanel("", small_fig1, icon = icon1),
        shiny::tabPanel("", small_fig2, icon = icon2),
        shiny::tabPanel("", small_fig3, icon = icon3),
      )
  } else if (n == 4) {
    # TabBox
    tab_box <-
      shinydashboard::tabBox(
        width = NULL,
        title = "",
        id = tabset_id,
        shiny::tabPanel("", fig1, icon = icon1),
        shiny::tabPanel("", fig2, icon = icon2),
        shiny::tabPanel("", fig3, icon = icon3),
        shiny::tabPanel("", fig4, icon = icon4),
      )
    small_tab_box <-
      shinydashboard::tabBox(
        width = NULL,
        title = "",
        id = tabset_id,
        shiny::tabPanel("", small_fig1, icon = icon1),
        shiny::tabPanel("", small_fig2, icon = icon2),
        shiny::tabPanel("", small_fig3, icon = icon3),
        shiny::tabPanel("", small_fig4, icon = icon4),
      )
    } else if (n == 5) {
      # TabBox
      tab_box <-
        shinydashboard::tabBox(
          width = NULL,
          title = "",
          id = tabset_id,
          shiny::tabPanel("", fig1, icon = icon1),
          shiny::tabPanel("", fig2, icon = icon2),
          shiny::tabPanel("", fig3, icon = icon3),
          shiny::tabPanel("", fig4, icon = icon4),
          shiny::tabPanel("", fig5, icon = icon5),
        )
      small_tab_box <-
        shinydashboard::tabBox(
          width = NULL,
          title = "",
          id = tabset_id,
          shiny::tabPanel("", small_fig1, icon = icon1),
          shiny::tabPanel("", small_fig2, icon = icon2),
          shiny::tabPanel("", small_fig3, icon = icon3),
          shiny::tabPanel("", small_fig4, icon = icon4),
          shiny::tabPanel("", small_fig5, icon = icon5),
        )
    } else if (n == 6) {
      # TabBox
      tab_box <-
        shinydashboard::tabBox(
          width = NULL,
          title = "",
          id = tabset_id,
          shiny::tabPanel("", fig1, icon = icon1),
          shiny::tabPanel("", fig2, icon = icon2),
          shiny::tabPanel("", fig3, icon = icon3),
          shiny::tabPanel("", fig4, icon = icon4),
          shiny::tabPanel("", fig5, icon = icon5),
          shiny::tabPanel("", fig6, icon = icon6),
        )
      small_tab_box <-
        shinydashboard::tabBox(
          width = NULL,
          title = "",
          id = tabset_id,
          shiny::tabPanel("", small_fig1, icon = icon1),
          shiny::tabPanel("", small_fig2, icon = icon2),
          shiny::tabPanel("", small_fig3, icon = icon3),
          shiny::tabPanel("", small_fig4, icon = icon4),
          shiny::tabPanel("", small_fig5, icon = icon5),
          shiny::tabPanel("", small_fig6, icon = icon6),
        )
    } else {
      tab_box <- NULL
      small_tab_box <- NULL
    }

  return(list(tab_box = tab_box, small_tab_box = small_tab_box))

}
