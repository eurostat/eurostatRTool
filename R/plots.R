# PLOT FUNCTIONS
# NONE OF THESE FUNCTIONS ARE EXPORTED


# Function for generating plot title
get_title <- function(chart_title, chart_subtitle) {

  if ((!is.na(chart_title) && nchar(chart_title)>0) && (!is.na(chart_subtitle) && nchar(chart_subtitle)>0)) {
    return(paste0(chart_title, "<BR><span style='font-size:12px;'>", chart_subtitle, "</span>"))
  } else if (!is.na(chart_title) && nchar(chart_title)>0) {
    return(chart_title)
  } else if (!is.na(chart_subtitle) && nchar(chart_subtitle)>0) {
    return(paste0("<span style='font-size:12px;'>", chart_subtitle, "</span>"))
  } else {
    return("")
  }

}


# Function for generating any chart
generate_chart <- function(chart_type, data = NULL, geo_labels = NULL,
                           colors_palette, indicator, geo_or_sheet_name = "",
                           chart_title = "", chart_subtitle = "", dec_digit_format = "",
                           png = FALSE) {

  if (chart_type == "timeline") {
    return(timeline_chart(data, geo_labels, colors_palette, indicator, chart_title,
                          chart_subtitle, dec_digit_format, png))
  } else if (chart_type == "cross_country") {
    return(bar_chart(data, geo_labels, colors_palette, indicator, chart_title,
                     chart_subtitle, dec_digit_format))
  } else if (chart_type == "ecoin") {
    return(ecoin_chart(data, colors_palette, geo_labels, indicator, geo_or_sheet_name,
                       chart_title, chart_subtitle, png))
  } else if (chart_type == "double_scale") {
    return(double_scale_chart(data, geo_labels, colors_palette, indicator,
                              geo_or_sheet_name, chart_title, chart_subtitle,
                              dec_digit_format, png))
  } else if (chart_type == "forecast") {
    return(forecast_chart(colors_palette, indicator, geo_or_sheet_name, chart_title,
                          dec_digit_format, png))
  } else if (chart_type == "map") {
    return(map(data, geo_labels, indicator, chart_title, chart_subtitle, dec_digit_format,
               png, geo_or_sheet_name))
  } else {
    return(table(data, geo_labels, chart_title, geo_or_sheet_name, indicator,
                 chart_subtitle))
  }

}


# Function for plotting line chart
#
# @return plot
# @import dplyr plotly
timeline_chart <- function(data, geo_labels, colors_palette, indicator, chart_title = "",
                           chart_subtitle = "", dec_digit_format, png = FALSE) {

  custom_hoverformat <- paste("(%{x}, %{y:", dec_digit_format, "})", sep="")
  chart_title <- get_title(chart_title, chart_subtitle)

  # If there is a minimum date in the configuration file, filter data
  min_date <- read_user_config_param("date_from")
  if (is.null(min_date)) {
    min_date <- "2015-01-01"
  }

  data_filtered <- data %>%
    dplyr::filter(!is.na(obsValue),
                  IND == indicator,
                  date >= min_date) %>%
    dplyr::mutate(DIM = dplyr::case_when(DIM == "EU27" ~ "EU",
                                         DIM == "EA20" ~ "EA", # EA changed 1
                                         TRUE ~ DIM)) %>%
    dplyr::arrange(date) %>%
    dplyr::inner_join(., geo_labels, by = dplyr::join_by(DIM == code)) %>%
    dplyr::select(-DIM) %>%
    dplyr::mutate(DIM = label) %>%
    dplyr::mutate(date = dplyr::case_when(FREQ == "Q" ~ lubridate::add_with_rollback(date, months(-2), roll_to_first = TRUE), #to better fit y scale with quarterly dates
                                          TRUE ~ date))

  # Factorize the DIM column to keep the same order as the data
  filtered_geo_labels <- intersect(geo_labels$label, unique(data_filtered$DIM))
  order <- filtered_geo_labels[1:length(filtered_geo_labels)]
  data_filtered$DIM <- factor(data_filtered$DIM, levels=order)

  num_colours <- nlevels(data_filtered$DIM)

  #Number of colours in palette
  palette_subset <- colors_palette[1:num_colours]

  # If only one trace is going to be plotted, the legend will not be shown and the y-axis limits are defined in code
  # For the axis we compute a range that covers all values and is broken into EIGHT intervals
  # This is done as follows:
  # We require 8 interval, i.e. nine ticks. This can be changed but a number is needed to be able to set and align ticks.
  # We then compute an appropriate starting tick (yLL) and intervals between ticks (steptick).
  # We use a heuristic algorithm to set steptick and yLL. Essentially steptick is near the ratio of
  # the data's range divided by the number of intervals.

  visible_legend <- TRUE # start by assuming that we will show a legend
  yLL <- NULL
  yUL <- NULL
  steptick <-NULL

  if (num_colours == 1)  { #Only one trace will be shown

    visible_legend <- FALSE
    num_int <- 8 # number of intervals to break the y axis range into
    ymin <- min(data_filtered$obsValue)
    ymax <- max(data_filtered$obsValue)
    expon_chart <- floor(log10((ymax-ymin)/num_int)) # the exponent of the highest power of ten that does not exceed the order of magnitude of the ratio range/number of intervals. E.g. if the ratio is 120000, the exponent is five
    d_mult_chart <- floor((ymax-ymin)/(num_int*(10^expon_chart))) # the multiplier of this power of ten that give an interval length below the required length
    ind <- 1:20 # we will examine twenty possible lengths, each one larger than the previous one by 0.1 X the power of ten computed earlier
    step <- (d_mult_chart+0.1*ind)*(10^expon_chart) # the candidate lengths
    LL <- step*floor(ymin/step) # the lower limit of the range corresponding to the interval length and the values' range
    UL <- LL+num_int*step # the corresponding end of the range
    temp_i <- min(ind[UL >= ymax]) # Find the right multiple; the first that gives upper limit >= the maximum value of the variables
    steptick <- step[temp_i] # set the interval length
    yLL <- LL[temp_i] # set the range's lower limit
    yUL <- UL[temp_i] # set the range's upper limit

  }

  fig <-
    plotly::plot_ly(
      data = data_filtered %>%
        dplyr::filter(group == 1), x = ~ date) %>%
    plotly::config(displaylogo = FALSE) %>%
    plotly::config(modeBarButtonsToRemove = c("sendDataToCloud",
                                              "editInChartStudio", "zoom2d",
                                              "pan2d", "select2d", "lasso2d",
                                              "drawclosedpath", "drawopenpath",
                                              "drawline", "drawrect",
                                              "drawcircle", "eraseshape",
                                              "autoScale2d")) %>%
    plotly::add_lines(y = ~obsValue, color = ~DIM, colors = palette_subset,
                      line = list(width = 2.5), legendgroup = ~group,
                      showlegend = visible_legend,
                      hovertemplate=custom_hoverformat) %>%
    plotly::add_lines(data = data_filtered %>%
                        dplyr::filter(group != 1),
                      y = ~obsValue, color = ~DIM, colors = palette_subset,
                      visible = "legendonly", line = list(width = 2.5),
                      legendgroup = ~group, showlegend = visible_legend,
                      hovertemplate=custom_hoverformat)

  # Normal screen layout
  normal_fig <- fig %>%
    plotly::layout(
      margin = list(t = 70, b = 0), # l=, r=,  pad = , b= 70
      title = chart_title,
      hovermode = "x",
      separators = ". ",
      # height = chartHeight,

      legend = list(
        itemdoubleclick = "toggle",
        groupclick = "toggleitem",
        tracegroupgap = 7
      ),

      xaxis = list(
        title = FALSE,
        tickformat = ifelse(data_filtered[1, "FREQ"] == "Q", "Q%q-%Y",
                            ifelse(data_filtered[1, "FREQ"] == "M", "%b-%Y", "%Y")),
        rangeslider = list(bgcolor="#F5F5F5",  type = "date",
                           yaxis = list(range=list(NULL,NULL), rangemode="fixed"))
      )
    )

  # Small screen layout
  small_fig <- fig %>%
    plotly::layout(
      margin = list(t = 70, b = 0), # l=, r=,  pad = , b= 70
      title = list(pad = list(b = 50),
                   text = chart_title),
      hovermode = "closest",
      separators = ". ",

      legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.2),

      xaxis = list(
        title = FALSE,
        tickformat = ifelse(data_filtered[1, "FREQ"] == "Q", "Q%q-%Y", ifelse(data_filtered[1, "FREQ"] == "M", "%b-%Y", "%Y"))
      )
    ) %>%
    plotly::style(legendgroup = NULL)

  if (num_colours > 1)  { #more than one areas have data
    # Update layout for normal screen
    normal_fig <- normal_fig %>%
      plotly::layout(yaxis = list(
        zeroline = FALSE,
        automargin = TRUE,
        exponentformat = "none", # show the values without exponentiation
        separatethousands = TRUE,
        title = FALSE
        ))
    # Update layout for small screen
    small_fig <- small_fig %>%
      plotly::layout(
        yaxis = list(
          zeroline = FALSE,
          automargin = TRUE,
          exponentformat = "none", # show the values without exponentiation
          separatethousands = TRUE,
          title = FALSE,
          fixedrange = TRUE
        ),
        xaxis = list(fixedrange = TRUE))
  } else { #only one area has data. We then, declare limits for the chart
    # Update layout for normal screen
    normal_fig <- normal_fig %>%
      plotly::layout(yaxis = list(
        zeroline = FALSE,
        automargin = TRUE,
        exponentformat = "none", # show the values without exponentiation
        separatethousands = TRUE,
        range = list(yLL,yUL),
        tick0 = yLL,
        dtick = steptick,
        title = FALSE
        ))
    # Update layout for small screen
    small_fig <- small_fig %>%
      plotly::layout(
        yaxis = list(
          zeroline = FALSE,
          automargin = TRUE,
          exponentformat = "none", # show the values without exponentiation
          separatethousands = TRUE,
          range = list(yLL,yUL),
          tick0 = yLL,
          dtick = steptick,
          title = FALSE,
          fixedrange = TRUE
        ),
        xaxis = list(fixedrange = TRUE))

  }

  # Remove rangeslider when plotting the graph for the pdf report
  if (png) {
    normal_fig <- normal_fig %>%
      plotly::layout(
        xaxis = list(
          rangeslider = list(visible = FALSE)
        )
      )
  }

  return(list(fig = normal_fig, small_fig = small_fig))

}


# Function for plotting bar chart
#
# @return plot
# @import dplyr plotly lubridate
bar_chart <- function(data, geo_labels, colors_palette, indicator, chart_title = "",
                      chart_subtitle = "", dec_digit_format) {

  custom_hoverformat <- paste("%{y:", dec_digit_format, "}", sep="")
  chart_title <- get_title(chart_title, chart_subtitle)

  #Number of colours in palette
  palette_subset <- colors_palette[1:2]

  data_filtered <- data %>%
    dplyr::filter(IND == indicator)

  date_last <- data_filtered %>%
    dplyr::filter(is.na(obsValue) == FALSE) %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::select(date) %>%
    unique() %>%
    dplyr::pull(date) %>%
    as.Date()

  if(data_filtered[1, "FREQ"] == "Q") {
    date_previous <- lubridate::add_with_rollback(date_last, months(-3), roll_to_first = TRUE)
    } else if (data_filtered[1, "FREQ"] == "M") {
      date_previous <- lubridate::add_with_rollback(date_last, months(-1), roll_to_first = TRUE)
      } else {
        stop("BarChart function supports data only with monthly or quarterly frequency ")
        }

  data_filtered <- data_filtered %>%
    dplyr::filter(date == date_last | date == date_previous) %>%
    dplyr::mutate(DIM = dplyr::case_when(DIM == "EU27" ~ "EU",
                                         DIM == "EA20" ~ "EA", # EA changed 4
                                         TRUE ~ DIM),
                  obsValue = round_half_up(obsValue, 1)
    ) %>%
    dplyr::mutate(date2 = dplyr::case_when(
      FREQ == "Q" ~ paste0("Q", lubridate::quarter(date), "-", lubridate::year(date)),
      FREQ == "M" ~
        paste0(lubridate::month(date, label = TRUE, locale = "English"), "-", lubridate::year(date)),
      TRUE ~ "NA")
      ) %>%
    dplyr::inner_join(., geo_labels, by = dplyr::join_by(DIM == code)) %>%
    dplyr::select(-DIM) %>%
    dplyr::rename(DIM = label) %>%
    dplyr::filter(is.na(obsValue) == FALSE)

  date_ordered <- data_filtered %>%
    dplyr::arrange(date) %>%
    dplyr::pull(date2) %>%
    unique()

  geo_ordered <- data_filtered %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::arrange(-obsValue) %>%
    dplyr::pull(DIM)

  data_filtered <- data_filtered %>%
    dplyr::mutate(date = as.character(date)) %>%
    dplyr::mutate(DIM = factor(DIM, levels = geo_ordered)) %>%
    dplyr::mutate(date2 = factor(date2, levels = date_ordered)) %>%
    dplyr::select(-date) %>%
    dplyr::rename(date = date2) %>%
    dplyr::group_by(date) %>%
    dplyr::arrange(date)

  fig_widths <-
    data_filtered[!is.na(data_filtered$DIM), c("DIM","group")] %>%
    dplyr::distinct(DIM, group) %>%
    dplyr::count(group) %>%
    {apply(.["n"], 2, function(x) x/sum(x))} %>%
    as.vector()

  plot_list <- list()
  for (i in 1:length(fig_widths)) {
    name = paste0("plot", i)
    plot_list[[name]] <- plotly::plotly_build(
      plotly::plot_ly(
        data = data_filtered %>%
          dplyr::filter(group == i) %>%
          droplevels(),
        x = ~DIM,
        y = ~obsValue,
        color = ~date,
        colors = palette_subset,
        type = "bar",
        legendgroup = ~date,
        showlegend = ifelse(i == 1, TRUE, FALSE),
        hovertemplate = custom_hoverformat) %>%
        plotly::config(displaylogo = FALSE) %>%
        plotly::config(modeBarButtonsToRemove = c("sendDataToCloud", "editInChartStudio", "zoom2d", "pan2d",
                                                  "select2d", "lasso2d", "drawclosedpath", "drawopenpath", "drawline",
                                                  "drawrect", "drawcircle", "eraseshape", "autoScale2d", "resetScale2d")) %>%
        plotly::layout(hovermode = "x",
                       xaxis = list(
                         fixedrange = TRUE,
                         tickangle = -90
                       ),
                       separators = ". ",
                       yaxis = list(
                         fixedrange = TRUE,
                         title = FALSE
                       ))
    )
  }
  fig <- plotly::subplot(
    plot_list,
    widths = fig_widths,
    margin = 0.01,
    shareY = TRUE
  )

  fig <- fig %>%
    plotly::layout(
      margin = list(t = 70, b = 0), # l=, r=,  pad = , b= 70
      # height = chartHeight,
      title = chart_title
      )

  # Mobile layout

  custom_hoverformat <- paste("(%{x:", dec_digit_format, "}, %{y})", sep="")
  plot_list <- list()
  for (i in 1:length(fig_widths)) {
    name = paste0("plot", i)
    plot_list[[name]] <- plotly::plotly_build(
      plotly::plot_ly(
        data = data_filtered %>%
          dplyr::filter(group == i) %>%
          droplevels(),
        y = ~DIM,
        x = ~obsValue,
        color = ~date,
        colors = palette_subset,
        type = "bar",
        orientation = "h",
        legendgroup = ~date,
        showlegend = ifelse(i == 1, TRUE, FALSE),
        hovertemplate = custom_hoverformat) %>%
        plotly::config(displaylogo = FALSE) %>%
        plotly::config(modeBarButtonsToRemove = c("sendDataToCloud", "editInChartStudio", "zoom2d", "pan2d",
                                                  "select2d", "lasso2d", "drawclosedpath", "drawopenpath", "drawline",
                                                  "drawrect", "drawcircle", "eraseshape", "autoScale2d", "resetScale2d")) %>%
        plotly::layout(
          xaxis = list(
            fixedrange = TRUE,
            title = FALSE
          ),
          separators = ". ",
          yaxis = list(
            autorange = "reversed",
            fixedrange = TRUE,
            # tickfont = list(size = 9),
            title = FALSE
          )
        )
    )
  }
  small_fig <-
    plotly::subplot(
      plot_list,
      nrows = length(fig_widths),
      heights = fig_widths,
      margin = 0.005,
      shareX = TRUE) %>%
    plotly::layout(
      margin = list(t = 70, b = 0), # l=, r=,  pad = , b= 70
      height = 800,
      title = chart_title
    )

  return(list(fig = fig, small_fig = small_fig))

}


# Function for ecoin chart
#
# @return plot
# @import dplyr plotly lubridate
ecoin_chart <- function(data, colors_palette, geo, ecoin_indicator, gdp_indicator,
                        chart_title = "", chart_subtitle = "", png = FALSE) {

  chart_title <- get_title(chart_title, chart_subtitle)

  #Number of colours in palette
  palette_subset <- colors_palette[1:3]

  # If there is a minimum date in the configuration file, filter data
  min_date <- read_user_config_param("date_from")
  if (is.null(min_date)) {
    min_date <- "2015-01-01"
  }

  data_filtered <- data %>%
    dplyr::filter(!is.na(obsValue),
                  DIM == geo,
                  IND %in% c(ecoin_indicator, gdp_indicator),
                  date >= min_date) %>%
    dplyr::select(-DIM) %>%
    dplyr::mutate(date = dplyr::case_when(FREQ == "Q" ~ lubridate::add_with_rollback(date, months(-1), roll_to_first = TRUE),
                                          TRUE ~ date)) %>% #to better fit y scale with quarterly dates
    dplyr::arrange(date) %>%
    dplyr::mutate(quarter_temp = lubridate::quarter(date, with_year = TRUE)) %>%
    dplyr::mutate(quarter = paste("Q", substr(quarter_temp, 6, 6),"-", substr(quarter_temp, 1, 4), sep="")) %>%
    dplyr::select(-quarter_temp)

  # Calculate y axis limits. One set for ECOIN and one set for GDP Q-on-Q
  y_ecoin_ll <- floor(min(data_filtered$obsValue[data_filtered$IND == ecoin_indicator])) # Lower limit
  y_ecoin_ul <- ceiling(max(data_filtered$obsValue[data_filtered$IND == ecoin_indicator])) # Upper limit

  temp_data <- data_filtered %>%
    dplyr::filter(IND==gdp_indicator) %>% # temporary copy of the lines with data for GDP_QOQ
    dplyr::mutate(IND = "OUTL")

  temp_data$temp_value <- NA
  temp_data$temp_value[temp_data$obsValue > y_ecoin_ul] <- y_ecoin_ul - 0.03
  temp_data$temp_value[temp_data$obsValue < y_ecoin_ll] <- y_ecoin_ll + 0.03
  temp_data$obsValue <- temp_data$temp_value
  temp_data <- temp_data %>% dplyr::select(-temp_value)

  data_filtered <- rbind(data_filtered, temp_data)
  rm(temp_data)

  data_filtered <- data_filtered %>%
    dplyr::mutate(ind_group = dplyr::case_when(
      IND %in% c(gdp_indicator, "OUTL") ~ "GDP", #GDP and outliers
      IND == ecoin_indicator ~ "ECOIN")) #E-coin

  fig <- plotly::plot_ly(data = data_filtered, x = ~ date) %>%
    plotly::config(displaylogo = FALSE) %>%
    plotly::config(modeBarButtonsToRemove =
                     c("sendDataToCloud", "editInChartStudio", "zoom2d", "pan2d",
                       "select2d", "lasso2d", "drawclosedpath", "drawopenpath",
                       "drawline", "drawrect", "drawcircle", "eraseshape",
                       "autoScale2d")) %>%
    plotly::add_lines(
      data = data_filtered %>%
        dplyr::filter(IND == ecoin_indicator),
      y = ~obsValue, name = "Euro-coin",
      line = list(color = palette_subset[1], width = 2.5), legendgroup = ~ind_group) %>%
    plotly::add_trace(data = data_filtered %>%
                dplyr::filter(IND == gdp_indicator),
                y = ~obsValue,
                name = "GDP q-o-q growth rate",
                type = 'scatter', mode = "markers",
                marker = list(color = palette_subset[3]), hoverinfo = "text+name",
                text = ~paste("(",quarter,",",obsValue,")",sep=""), legendgroup = ~ind_group) %>%
    plotly::add_trace(data = data_filtered %>%
                        dplyr::filter(IND == "OUTL"),
                      y = ~obsValue,
                      name = "GDP outliers", type = 'scatter', mode = "markers",
                      marker = list(color = palette_subset[2]), hoverinfo = "none", legendgroup = ~ind_group) %>%
    plotly::layout(
      margin = list(t = 70, b = 0), # l=, r=,  pad = , b= 70
      title = list(text = chart_title, font = list(
        family = "Arial"
      )),
      hovermode = "x",
      separators = ". ",
      xaxis = list(
        title = FALSE,
        tickformat = "%b-%Y"
      ),# end xaxis
      yaxis = list(
        automargin = TRUE,
        exponentformat = "none",
        range = list(y_ecoin_ll,y_ecoin_ul),
        tick0 = y_ecoin_ll,
        dtick = 1,
        tickformat = ".2f",
        title = FALSE
      ) #end left-hand-side axis
    )# end layout

  fig <- fig %>%
    plotly::layout(
      xaxis=list(
        rangeslider = list(bgcolor="#F5F5F5", type = "date", yaxis = list(range=list(NULL,NULL), rangemode="fixed"))
      )
    )

  # Mobile view chart
  small_fig <- fig %>%
    plotly::layout(
      legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.5)
    )

  # Remove rangeslider when plotting the graph for the pdf report
  if (png) {
    fig <- fig %>%
      plotly::layout(
        xaxis = list(
          rangeslider = list(visible = FALSE)
        )
      )
  }

  return(list(fig = fig, small_fig = small_fig))

}


# Function for plotting double scale chart
#
# @return plot
# @import dplyr plotly lubridate
double_scale_chart <- function(data, geo_labels, colors_palette, indicator, geo,
                               chart_title = "", chart_subtitle = "",
                               dec_digit_format, png = FALSE) {

  custom_hoverformat <- paste("%{y:", dec_digit_format, "}", sep="")

  # If there is a minimum date in the configuration file, filter data
  min_date <- read_user_config_param("date_from")
  if (is.null(min_date)) {
    min_date <- "2015-01-01"
  }

  # We are processing the data as follows:
  #
  # 1. We retain only the variables starting with "TCD_" and then, the three letters matching
  #    the three-letter acronym given as indicator in the function's call: GDP, IPI or EMP.
  # 2. We create two new columns in the data frame:
  #    a) TCDFilter with values
  #        --- "" when IND is the indicator,
  #        --- "HP" when IND contains HP
  #        --- "CF" when IND contains CF
  #        --- "UC" when IND contains UC
  #    b) TCDIND with values
  #        --- "GDP" when IND is the indicator and contains GDP,
  #        --- "IPI" when IND is the indicator and contains IPI,
  #        --- "Employment" when IND is the indicator and contains EMP,
  #        --- "Trend" when IND is a Trend
  #        --- "Cycle" when IND is a Cycle
  # All other processing is as in the other two graphing functions defined above.

  chart_title <- get_title(chart_title, chart_subtitle)

  data_filtered <- data %>%
    dplyr::filter(!is.na(obsValue),
                  substr(IND,start=5,stop=7) == indicator,
                  substr(IND,start=1,stop=3) == "TCD",
                  DIM == geo,
                  date >= min_date) %>%
    dplyr::mutate(DIM = dplyr::case_when(DIM == "EU27" ~ "EU",
                                         DIM == "EA20" ~ "EA", # EA changed 9
                                         TRUE ~ DIM)) %>%
    dplyr::mutate(TCD_filter = dplyr::case_when(substr(IND,start=9,stop=10) == "HP" ~ "HP",
                                                substr(IND,start=9,stop=10) == "CF" ~ "CF",
                                                substr(IND,start=9,stop=10) == "UC" ~ "UC",
                                                TRUE ~ "")) %>%
    dplyr::mutate(TCD_ind = dplyr::case_when(substring(IND,12) == "TREND" ~ "Trend",
                                             substring(IND,12) == "CYCLE" ~ "Cycle",
                                             indicator == "GDP" ~ "GDP",
                                             indicator == "IPI" ~ "IPI",
                                             indicator == "EMP" ~ "Empl.")) %>%
    dplyr::arrange(date)

  # For each axis we computer a range that covers all values and is broken into EIGHT intervals
  # This enables the alignment of the primary and secondary y axis' grids
  # This is done as follows:
  # We require 8 interval, i.e. nine ticks. This can be changed but a number is needed to be able to set and align ticks.
  # We then compute an appropriate starting tick (tick0) and intervals between ticks (dtick). If you set
  # tick0 and dtick for both axes and they have the ame number of ticks the grids will be aligned.
  # We use a heuristic algorithm to set dtick and tick0. Essentially dtick is near the ratio of
  # the data's range divided by the number of intervals.

  num_int <- 8 # number of intervals to break the y axis range into
  # TODO: unificar
  y_trend_min <- min(data_filtered$obsValue[(data_filtered$DIM == substr(geo,1,2)) & (data_filtered$TCD_ind != "Cycle")])
  y_trend_max <- max(data_filtered$obsValue[(data_filtered$DIM == substr(geo,1,2)) & (data_filtered$TCD_ind != "Cycle")])
  expon_chart <- floor(log10((y_trend_max - y_trend_min)/num_int)) # the exponent of the highest power of ten that does not exceed the order of magnitude of the ratio range/number of intervals. E.g. if the ratio is 120000, the exponent is five
  d_mult_chart <- floor((y_trend_max - y_trend_min)/(num_int*(10^expon_chart))) # the multiplier of this power of ten that give an interval length below the required length
  ind_temp <- 1:20 # we will examine twenty possible lengths, each one larger than the previous one by 0.1 X the power of ten computed earlier
  step_temp <- (d_mult_chart + 0.1*ind_temp)*(10^expon_chart) # the candidate lengths
  ll_temp <- step_temp*floor(y_trend_min/step_temp) # the lower limit of the range corresponding to the interval length and the values' range
  ul_temp <- ll_temp + num_int*step_temp # the corresponding end of the range
  temp_i <- min(ind_temp[ul_temp >= y_trend_max]) # Find the right multiple; the first that gives upper limit >= the maximum value of the variables
  step_trend <- step_temp[temp_i] # set the interval length
  y_trend_ll <- ll_temp[temp_i] # set the range's lower limit
  y_trend_ul <- ul_temp[temp_i] # set the range's upper limit

  y_cycle_min <- min(data_filtered$obsValue[(data_filtered$DIM == substr(geo,1,2)) & (data_filtered$TCD_ind == "Cycle")])
  y_cycle_max <- max(data_filtered$obsValue[(data_filtered$DIM == substr(geo,1,2)) & (data_filtered$TCD_ind == "Cycle")])
  expon_chart <- floor(log10((y_cycle_max - y_cycle_min)/num_int))
  d_mult_chart <- floor((y_cycle_max-y_cycle_min)/(num_int*(10^expon_chart)))
  ind_temp <- 1:20
  step_temp <- (d_mult_chart+0.1*ind_temp)*(10^expon_chart)
  ll_temp <- step_temp*floor(y_cycle_min/step_temp)
  ul_temp <- ll_temp+num_int*step_temp
  temp_i <- min(ind_temp[ul_temp >= y_cycle_max]) # Find the right multiple; the first that gives upper limit >= the maximum value of the variables
  step_cycle <- step_temp[temp_i]
  y_cycle_ll <- ll_temp[temp_i]
  y_cycle_ul <- ul_temp[temp_i]


  # Continue the preparation of the data
  data_filtered <- data_filtered %>%
    dplyr::inner_join(., geo_labels, by = dplyr::join_by(DIM == code)) %>%
    dplyr::select(-DIM) %>%
    dplyr::rename(DIM = label) %>%
    dplyr::mutate(date = dplyr::case_when(FREQ == "Q" ~ lubridate::add_with_rollback(date, months(-2), roll_to_first = TRUE), #to better fit y scale with quarterly dates
                                          TRUE ~ date))

  ind_levels_order <- c("GDP","IPI","Empl.","Trend","Cycle")

  ind_included <- data_filtered$TCD_ind %>%
    unique() %>%
    intersect(ind_levels_order, .)

  data_filtered <- data_filtered %>%
    dplyr::mutate(TCD_ind = factor(TCD_ind, levels = ind_included))

  num_colours <- 3   # We need only three colours: indicator, trend, cycle
  palette_subset <- colors_palette[1:num_colours]

  # This figure contains the linechart of the indicator, the three trends and the three cycles for each of the filters.
  # We set only the HP trend visible by default.

  fig <- plotly::plot_ly(
    data = data_filtered %>%
      dplyr::filter(!TCD_ind %in% c("Cycle")) %>%
      dplyr::filter(TCD_filter %in% c("","HP")), x = ~ date) %>%
    plotly::config(displaylogo = FALSE) %>%
    # config(modeBarButtonsToAdd = c("renamedCam")) %>%
    plotly::config(modeBarButtonsToRemove = c("sendDataToCloud", "editInChartStudio", "zoom2d", "pan2d",
                                              "select2d", "lasso2d", "drawclosedpath", "drawopenpath", "drawline",
                                              "drawrect", "drawcircle", "eraseshape", "autoScale2d")) %>%
    plotly::add_lines(
      data = data_filtered %>%
        dplyr::filter(!TCD_ind %in% c("Cycle")) %>%
        dplyr::filter(TCD_filter %in% c("","HP")), y = ~obsValue, color = ~TCD_ind, colors = palette_subset,
              line = list(width = 2.5), hovertemplate=custom_hoverformat) %>%
    plotly::add_lines(
      data = data_filtered %>%
        dplyr::filter(!TCD_ind %in% c("Cycle")) %>%
        dplyr::filter(TCD_filter %in% c("CF")),
      y = ~obsValue, color = ~TCD_ind, colors = palette_subset,
      line = list(width = 2.5), visible=FALSE,
      hovertemplate=custom_hoverformat) %>%
    plotly::add_lines(
      data = data_filtered %>%
        dplyr::filter(!TCD_ind %in% c("Cycle")) %>%
        dplyr::filter(TCD_filter %in% c("UC")),
      y = ~obsValue, color = ~TCD_ind, colors = palette_subset,
      line = list(width = 2.5), visible=FALSE,
      hovertemplate=custom_hoverformat)

  # Desktop view

  # Here, we define a dropdown list for the selection of filter.
  # In fact, each choice defines which of the trend and cycle lines will be visible.
  # The indicator itself is always visible.

  updatemenus <- list(
    list(
      y = 1.2,
      x = 0.2,
      active = 0,
      bgcolor = "#F5F5F5",
      type= "dropdown",
      font = list(
        color = "#0A2744",
        family = "Arial",
        size = 12
      ),
      showactive = FALSE,
      buttons = list(
        list(
          label = "Hodrick-Prescott filter",
          method = "update",
          args = list(list(visible = c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE)))), # Indicator and HP decomp. visible
        list(
          label = "Christiano-Fitzgerald filter",
          method = "update",
          args = list(list(visible = c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE)))), # Indicator and CF decomp. visible
        list(
          label = "Unobserved components",
          method = "update",
          args = list(list(visible = c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE)))) # Indicator and UC decomp. visible
      )
    )
  )

  normal_fig <- fig %>%
    plotly::add_lines(
      data = data_filtered %>%
        dplyr::filter(TCD_ind %in% c("Cycle"))  %>%
        dplyr::filter(TCD_filter == "HP"),
      y = ~obsValue, xaxis = "x2", yaxis = "y2", color = ~TCD_ind,
      line = list(width = 2.5), colors = palette_subset,
      hovertemplate=custom_hoverformat) %>%
    plotly::add_lines(
      data = data_filtered %>%
        dplyr::filter(TCD_ind %in% c("Cycle")) %>%
        dplyr::filter(TCD_filter == "CF"),
      y = ~obsValue, xaxis = "x2", yaxis = "y2", color = ~TCD_ind,
      line = list(width = 2.5), colors = palette_subset, visible=FALSE,
      hovertemplate=custom_hoverformat) %>%
    plotly::add_lines(
      data = data_filtered %>%
        dplyr::filter(TCD_ind %in% c("Cycle")) %>%
        dplyr::filter(TCD_filter == "UC"),
      y = ~obsValue, xaxis = "x2", yaxis = "y2", color = ~TCD_ind,
      line = list(width = 2.5), colors = palette_subset, visible=FALSE,
      hovertemplate=custom_hoverformat) %>%
    plotly::layout(
      margin = list(t = 70, b = 0), # l=, r=,  pad = , b= 70

      title = list(text = chart_title, font = list(
        family = "Arial"
      )),

      #legend = list(x = 1, y = 1.22),
      legend = list(orientation = 'h', xanchor = "center", x = 0.5,y=-0.4),
      hovermode = "x",
      separators = ". ",

      xaxis = list(
        title = FALSE,
        tickformat = ifelse(data_filtered[1, "FREQ"] == "Q", "Q%q-%Y", ifelse(data_filtered[1, "FREQ"] == "M", "%b-%Y", "%Y")),
        rangeslider = list(bgcolor="#F5F5F5",  type = "date", yaxis = list(range=list(NULL,NULL),
                                                                           rangemode="fixed"))
      ),# end xaxis

      xaxis2 = list(
        overlaying = "x",
        matches = "x",
        title = FALSE,
        tickformat = ifelse(data_filtered[1, "FREQ"] == "Q", "Q%q-%Y", ifelse(data_filtered[1, "FREQ"] == "M", "%b-%Y", "%Y"))
      ),# end xaxis

      yaxis = list(
        title = list(text = paste(indicator,",Trend",sep = ""), standoff = 10),
        automargin = TRUE,
        zeroline = FALSE,
        exponentformat = "none", # show the values without exponentiation
        separatethousands = TRUE,
        range = list(y_trend_ll,y_trend_ul),
        tick0 = y_trend_ll,
        dtick = step_trend
      ), #end left-hand-side axis

      yaxis2 = list(
        title = list(text = "Cycle", standoff = 10),
        automargin = TRUE,
        zeroline = FALSE,
        exponentformat = "none",
        separatethousands = TRUE,
        range = list(y_cycle_ll,y_cycle_ul),
        tick0 = y_cycle_ll,
        dtick = step_cycle,
        side = "right",
        overlaying = "y"
      ),
      updatemenus = updatemenus
    )

  # Mobile view

  # Here, we define a dropdown list for the selection of filter.
  # In fact, each choice defines which of the trend and cycle lines will be visible.
  # The indicator itself is always visible.

  small_updatemenus <- list(
    list(
      y = -0.25,
      x = 0.5,
      direction = "right",
      xanchor = "center",
      active = 0,
      bgcolor = "#F5F5F5",
      type= "buttons",
      font = list(
        color = "#0A2744",
        family = "Arial",
        size = 9
      ),
      showactive = TRUE,
      pad = list(
        t = 0,
        b = 0
      ),
      buttons = list(
        list(
          label = "Hodrick-Prescott filter",
          method = "update",
          args = list(list(visible = c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE)))), # Indicator and HP decomp. visible
        list(
          label = "Christiano-Fitzgerald filter",
          method = "update",
          args = list(list(visible = c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE)))), # Indicator and CF decomp. visible
        list(
          label = "Unobserved components",
          method = "update",
          args = list(list(visible = c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE)))) # Indicator and UC decomp. visible
      )
    )
  )
  small_fig <- fig %>%
    plotly::add_lines(
      data = data_filtered %>%
        dplyr::filter(TCD_ind %in% c("Cycle"))  %>%
        dplyr::filter(TCD_filter == "HP"),
      y = ~obsValue, yaxis = "y2", color = ~TCD_ind,
      line = list(width = 2.5), colors = palette_subset,
      hovertemplate=custom_hoverformat) %>%
    plotly::add_lines(
      data = data_filtered %>%
        dplyr::filter(TCD_ind %in% c("Cycle")) %>%
        dplyr::filter(TCD_filter == "CF"),
      y = ~obsValue, yaxis = "y2", color = ~TCD_ind,
      line = list(width = 2.5), colors = palette_subset, visible=FALSE,
      hovertemplate=custom_hoverformat) %>%
    plotly::add_lines(
      data = data_filtered %>%
        dplyr::filter(TCD_ind %in% c("Cycle")) %>%
        dplyr::filter(TCD_filter == "UC"),
      y = ~obsValue, yaxis = "y2", color = ~TCD_ind,
      line = list(width = 2.5), colors = palette_subset, visible=FALSE,
      hovertemplate=custom_hoverformat) %>%
    plotly::layout(
      margin = list(t = 90, b = 0), # l=, r=,  pad = , b= 70

      title = list(text = chart_title, font = list(
        family = "Arial"
      )),

      legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.05),
      hovermode = "closest",
      separators = ". ",

      xaxis = list(
        title = FALSE,
        tickfont = list(size = 10),
        tickangle=0,
        tickformat = ifelse(data_filtered[1, "FREQ"] == "Q", "Q%q-%Y", ifelse(data_filtered[1, "FREQ"] == "M", "%b-%Y", "%Y"))
      ),# end xaxis

      yaxis = list(
        title = list(text = paste(indicator,",Trend",sep = ""), standoff = 10),
        automargin = TRUE,
        zeroline = FALSE,
        exponentformat = "none", # show the values without exponentiation
        separatethousands = TRUE,
        tickfont = list(size = 10),
        range = list(y_trend_ll,y_trend_ul),
        tick0 = y_trend_ll,
        dtick = step_trend
      ), #end left-hand-side axis

      yaxis2 = list(
        title = list(text = "Cycle", standoff = 10),
        automargin = TRUE,
        zeroline = FALSE,
        exponentformat = "none",
        separatethousands = TRUE,
        tickfont = list(size = 10),
        range = list(y_cycle_ll,y_cycle_ul),
        tick0 = y_cycle_ll,
        dtick = step_cycle,
        side = "right",
        overlaying = "y",
        tickmode="sync"
      ),
      updatemenus = small_updatemenus)# end layout

  # Remove rangeslider when plotting the graph for the pdf report
  if (png) {
    normal_fig <- normal_fig %>%
      plotly::layout(
        xaxis = list(
          rangeslider = list(visible = FALSE)
        )
      )
  }

  return(list(fig = normal_fig, small_fig = small_fig))

}


# Function for plotting forecast chart type 1
#
# @return plot
# @import dplyr plotly
forecast_chart_type1 <- function(colors_palette, forecast_data, chart_title) {

  # Get the years to compare
  years <- unique(forecast_data$year)
  # Factorize entities to keep the same order as the data
  forecast_data$entity <- factor(forecast_data$entity, levels=unique(forecast_data$entity))
  # Get the color palette
  num_colours <- length(years)
  palette_subset <- colors_palette[1:num_colours]

  # Use a counter for the subplots, to show the legend just for the first one
  count_subplots <- 0
  # Function for plotting a subplot for an entity
  generate_subplot <- function(entity_data) {
    # Increase the subplots counter
    count_subplots <<- count_subplots+1
    # Split the entity name in lines, maximum 13 characters long, for the
    # correct visualization in y axis title
    entity_name <- paste(
      strwrap(
        entity_data$entity[1],
        width = 13
      ),
      collapse = "\n"
    )
    # Generate the subplot for the entity
    plot <- entity_data %>%
      dplyr::group_by(period) %>%
      plotly::plot_ly(
        x = ~value,
        y = ~period,
        color= ~year,
        colors = palette_subset,
        type = 'bar',
        orientation = 'h',
        legendgroup=~year,
        showlegend = ifelse(count_subplots == 1, TRUE, FALSE)) %>%
      plotly::layout(
        yaxis = list(title = list(
          text = entity_name, standoff = 50L, font = list(size = 10),
          xanchor = "left", align = "left")),
        legend = list(orientation = 'h'))
    # Return the subplot for the entity
    return(plot)
  }

  # For every entity, generate the subplot and join them to build the final plot
  fig <- forecast_data %>%
    split(.$entity) %>%
    lapply(function(entity_data) generate_subplot(entity_data)) %>%
    plotly::subplot(nrows = NROW(.), shareX = TRUE, titleX = FALSE, titleY = TRUE) %>%
    plotly::layout(showlegend = TRUE, title = chart_title,
                   margin = list(t = 70, b = 0)) %>%
    plotly::config(displaylogo = FALSE) %>%
    plotly::config(modeBarButtonsToRemove = c("sendDataToCloud",
                                              "editInChartStudio", "zoom2d",
                                              "pan2d", "select2d", "lasso2d",
                                              "drawclosedpath", "drawopenpath",
                                              "drawline", "drawrect",
                                              "drawcircle", "eraseshape",
                                              "autoScale2d"))

  # For mobile view, split the title in two lines if it's too long
  small_chart_title <- paste(
    strwrap(
      chart_title,
      width = 40
    ),
    collapse = "\n"
  )
  if (chart_title == small_chart_title) {
    small_fig <- fig
  } else {
    small_fig <- fig %>%
      plotly::layout(title = small_chart_title,
                     margin = list(t = 90, b = 0))
  }

  return(list(fig = fig, small_fig = small_fig))

}


# Function for plotting forecast chart type 2
#
# @return plot
# @import htmltools dplyr plotly
forecast_chart_type2 <- function(colors_palette, forecast_data, chart_title,
                                 iterator = 0, png = FALSE) {

  # Get indicators in the dataframe
  indicators <- colnames(forecast_data)[! colnames(forecast_data) %in% c("geo")]
  # Get entities in the dataframe
  entities <- c()
  for (ind in indicators) {
    entities <- c(entities, strsplit(ind, split = "-")[[1]][1])
  }
  entities <- unique(entities)

  # Function for plotting a chart with 2 entities
  plot_chart_2 <- function(first_entity, second_entity) {
    # Select only the 2 entities data
    to_match <- c("geo", first_entity, second_entity)
    plot_data <- forecast_data[, grepl(paste(to_match,collapse="|"), names(forecast_data))]
    # Factorize the geo column to keep the same order as the data
    reverse_order <- plot_data$geo[rev(1:length(plot_data$geo))]
    plot_data$geo <- factor(plot_data$geo, levels=reverse_order)
    # Get the color palette
    num_colours <- length(colnames(plot_data))
    palette_subset <- colors_palette[1:num_colours]

    # Generate the plot
    fig <- plot_data %>%
      tidyr::pivot_longer(cols=-c("geo"),
                          names_to="indicator", values_to="value") %>%
      plotly::plot_ly(
        x = ~value,
        y = ~geo,
        color= ~indicator,
        colors = palette_subset,
        type = 'bar',
        orientation = 'h',
        legendgroup=~indicator,
        showlegend = TRUE) %>%
      plotly::layout(showlegend = TRUE, title = chart_title,
                     margin = list(t = 70, b = 0),
                     legend = list(orientation = 'h'),
                     xaxis = list(title=""), yaxis = list(title=""),
                     autosize = F, height = 800) %>%
      plotly::config(displaylogo = FALSE) %>%
      plotly::config(modeBarButtonsToRemove = c("sendDataToCloud",
                                                "editInChartStudio", "zoom2d",
                                                "pan2d", "select2d", "lasso2d",
                                                "drawclosedpath", "drawopenpath",
                                                "drawline", "drawrect",
                                                "drawcircle", "eraseshape",
                                                "autoScale2d"))

    # For mobile view, split the title in two lines if it's too long
    small_chart_title <- paste(
      strwrap(
        chart_title,
        width = 40
      ),
      collapse = "\n"
    )
    if (chart_title == small_chart_title) {
      small_fig <- fig
    } else {
      small_fig <- fig %>%
        plotly::layout(title = small_chart_title,
                       margin = list(t = 90, b = 0))
    }
    # Return the subplot for the entity
    return(list(fig = fig, small_fig = small_fig))
  }

  # For any 2 entities, generate a plot and join it with previous ones
  fig <- NULL
  small_fig <- NULL
  for (i in seq(1, length(entities), 2)) {
    first_entity <- entities[i]
    second_entity <- entities[i+1]
    temp_figs <- plot_chart_2(first_entity, second_entity)
    if (png && iterator > 0) {
      plotly::save_image(temp_figs$fig, paste0("forecast_temp_", iterator, "_", i, ".png"))
    }
    fig <- htmltools::browsable(htmltools::tagList(
      fig,
      temp_figs$fig
    ))
    small_fig <- htmltools::browsable(htmltools::tagList(
      small_fig,
      temp_figs$small_fig
    ))
  }

  return(list(fig = fig, small_fig = small_fig))

}


# Function for plotting forecasts chart
#
# @return plot
# @import dplyr plotly htmltools
forecast_chart <- function(colors_palette, indicator, sheet_name, chart_title = "",
                           iterator = 0, png = FALSE) {

  # Get the forecast data depending on the sheet name and indicator
  if (is.na(indicator)) {
    forecast_data <- get(paste0("forecast_data_", sheet_name))
  } else {
    forecast_data <- get(paste0("forecast_data_", sheet_name, "_", indicator))
  }
  # If 'entity' is the first column, then plot the I type of forecast
  if (colnames(forecast_data)[1] == "entity") {
    figs <- forecast_chart_type1(colors_palette, forecast_data, chart_title)
  } else {
    figs <- forecast_chart_type2(colors_palette, forecast_data, chart_title,
                                 iterator, png)
  }

  return(figs)

}


# Function for plotting maps
#
# @return leaflet map
# @import dplyr leaflet sp htmltools
map <- function(data, geo_labels, indicator, chart_title = "",
                chart_subtitle = "", legend_title = NULL, png = FALSE,
                filename = "") {

  chart_title <- get_title(chart_title, chart_subtitle)

  data_filtered <- data %>%
    dplyr::filter(IND == indicator)

  date_last <- data_filtered %>%
    dplyr::filter(is.na(obsValue) == FALSE) %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::select(date) %>%
    unique() %>%
    dplyr::pull(date) %>%
    as.Date()

  if(data_filtered[1, "FREQ"] == "Q") {
    months_range <- c(3, 6, 9)
  } else if (data_filtered[1, "FREQ"] == "M") {
    months_range <- c(1, 2, 3)
  } else {
    stop("Map supports data only with monthly or quarterly frequency ")
  }

  dates_previous <- as.Date(character(0))
  for (i in months_range) {
    date_previous <- lubridate::add_with_rollback(date_last, months(-i), roll_to_first = TRUE)
    dates_previous <- c(dates_previous, date_previous)
  }

  data_filtered <- data_filtered %>%
    dplyr::filter(date == date_last | date %in% dates_previous) %>%
    dplyr::mutate(DIM = dplyr::case_when(DIM == "EU27" ~ "EU",
                                         DIM == "EA20" ~ "EA", # EA changed 4
                                         TRUE ~ DIM),
                  obsValue = round_half_up(obsValue, 1)
    ) %>%
    dplyr::mutate(date2 = dplyr::case_when(
      FREQ == "Q" ~ paste0("Q", lubridate::quarter(date), "-", lubridate::year(date)),
      FREQ == "M" ~
        paste0(lubridate::month(date, label = TRUE, locale = "English"), "-", lubridate::year(date)),
      TRUE ~ "NA")
    ) %>%
    dplyr::inner_join(., geo_labels, by = dplyr::join_by(DIM == code)) %>%
    dplyr::filter(is.na(obsValue) == FALSE)

  date_ordered <- data_filtered %>%
    dplyr::arrange(desc(date)) %>%
    dplyr::pull(date2) %>%
    unique()

  data_filtered <- data_filtered %>%
    dplyr::mutate(date = as.character(date)) %>%
    dplyr::mutate(date2 = factor(date2, levels = date_ordered)) %>%
    dplyr::select(-date) %>%
    dplyr::rename(date = date2) %>%
    dplyr::group_by(date) %>%
    dplyr::arrange(date) %>%
    dplyr::select(DIM, label, date, obsValue)

  all_data <- sp::merge(get("states", envir = .pkgglobalenv), data_filtered,
                        by.x = "CNTR_CODE", by.y = "DIM", all.x = FALSE,
                        duplicateGeoms = TRUE)

  pal <- leaflet::colorNumeric(palette = "Blues", domain = all_data$obsValue)

  labels <- sprintf(
    "<strong>%s</strong><br/>%g",
    all_data$label, all_data$obsValue
  ) %>% lapply(htmltools::HTML)

  m <-
    leaflet::leaflet(all_data) %>%
    leaflet::addPolygons(data = get("gisco_countries", envir = .pkgglobalenv), opacity = 1,
                         color = "#EBEBEB", fillColor = "#EBEBEB",
                         fillOpacity = 0.7, weight = 2) %>%
    leaflet::setView(lat = 52.52437, lng = 13.41053, zoom = 2.8) %>%
    leaflet::addPolygons(fillColor = ~pal(obsValue),
                         weight = 2,
                         opacity = 1,
                         color = "#EBEBEB",
                         fillOpacity = 0.7,
                         highlightOptions = leaflet::highlightOptions(
                           weight = 5,
                           color = "#FFD617",
                           fillOpacity = 0.7,
                           bringToFront = TRUE),
                         group = ~date,
                         label = labels,
                         labelOptions = leaflet::labelOptions(
                           style = list("font-weight" = "normal", padding = "3px 8px"),
                           textsize = "15px",
                           direction = "auto")) %>%
    leaflet::addLegend(title = legend_title, pal = pal, values = ~obsValue, opacity = 0.7)  %>%
    # Layers control
    leaflet::addLayersControl(
      baseGroups = date_ordered,
      options = leaflet::layersControlOptions(collapsed = FALSE)
    ) %>%
    leaflet::addTiles(urlTemplate = "",
                      attribution = "\u00A9 EuroGeographics for the administrative boundaries")

  if (png && !is.na(filename)) {
    mapview::mapshot(m, file = filename,
                     remove_controls = c("zoomControl", "homeButton", "scaleBar",
                                         "drawToolbar", "easyButton"))
  }

  fig <- htmltools::browsable(htmltools::tagList(
    htmltools::div(htmltools::HTML(chart_title), class="map-table-title"),
    htmltools::div(m, class="map")
  ))

  return(list(fig = fig, small_fig = fig))

}


# Function for plotting tables
#
# @return table
# @import DT dplyr
table <- function(data, geo_labels, chart_title, sheet_name,
                  indicator, geo_title) {

  # If the sheet_name is not null, build a table on forecast data, else on data
  if (!is.na(sheet_name)) {

    # Get the forecast data depending on the sheet name and indicator
    if (is.na(indicator)) {
      forecast_data <- get(paste0("forecast_data_", sheet_name))
    } else {
      forecast_data <- get(paste0("forecast_data_", sheet_name, "_", indicator))
    }

    colnames <- colnames(forecast_data)
    geo_title <- strsplit(geo_title, split = ",")[[1]]
    i <- 1
    for (title in geo_title) {
      colnames[i] <- title
      i <- i+1
    }

    table <- DT::datatable(forecast_data, rownames = FALSE, colnames = colnames,
                           filter = 'top',
                           options = list(pageLength = 5, autoWidth = TRUE,
                                          scrollX = FALSE))

    small_table <- DT::datatable(forecast_data, rownames = FALSE,
                                 colnames = colnames, filter = 'top',
                                 options = list(pageLength = 5,
                                                autoWidth = TRUE,
                                                scrollX = TRUE))

  } else {

    data_filtered <- data %>%
      dplyr::filter(IND == indicator) %>%
      dplyr::mutate(DIM = dplyr::case_when(DIM == "EU27" ~ "EU",
                                           DIM == "EA20" ~ "EA", # EA changed 4
                                           TRUE ~ DIM),
                    obsValue = round_half_up(obsValue, 1)
      ) %>%
      dplyr::inner_join(., geo_labels, by = dplyr::join_by(DIM == code)) %>%
      dplyr::filter(is.na(obsValue) == FALSE) %>%
      dplyr::select(date, label, obsValue)

    data_filtered$date <- format(data_filtered$date, format = "%d-%m-%Y")

    table <- DT::datatable(data_filtered, rownames = FALSE,
                           colnames = c("Date", geo_title, "Observed value"),
                           filter = 'top',
                           options = list(pageLength = 5, autoWidth = TRUE,
                                          scrollX = FALSE))

    small_table <- DT::datatable(data_filtered, rownames = FALSE,
                                 colnames = c("Date", geo_title, "Observed value"),
                                 filter = 'top',
                                 options = list(pageLength = 5, autoWidth = TRUE,
                                                scrollX = TRUE))

  }

  fig <- htmltools::browsable(htmltools::tagList(
    htmltools::div(htmltools::HTML(chart_title), class="map-table-title"),
    htmltools::div(table, class="table")
  ))

  # For mobile view, split the title in two lines if it's too long
  small_chart_title <- paste(
    strwrap(
      chart_title,
      width = 40
    ),
    collapse = "\n"
  )

  small_fig <- htmltools::browsable(htmltools::tagList(
    htmltools::div(htmltools::HTML(small_chart_title), class="map-table-title"),
    htmltools::div(small_table, class="table-mobile")
  ))

  return(list(fig = fig, small_fig = small_fig))

}


globalVariables(c("obsValue", "IND", ".", "date2", "DIM", "label", "group",
                  "quarter_temp", "temp_value", "TCD_ind", "TCD_filter",
                  "count_subplots"))

