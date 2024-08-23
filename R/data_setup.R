# DATA SETUP FUNCTIONS
# SOME OF THESE FUNCTIONS ARE EXPORTED


# Setup the Eurostat indicators dataframe
#
# Performs cleaning operations to the Eurostat indicators dataframe.
#
# @return A data frame.
#
# @import dplyr utils tools
data <- function(){

  # If there is a data.csv file available in the user memory, read it. If not,
  # read example data for Eurostatistics or Italy, depending from the
  # selected scenario
  user_data_path <- tools::R_user_dir("eurostatRTool", which = "data")
  data_path <- file.path(user_data_path, "data.csv")
  data <- NULL
  if (file.exists(data_path)) {
    data <- utils::read.csv(data_path)
  } else {
    scenario <- read_user_config_param("scenario")
    if (!is.null(scenario) && scenario == "nsi") {
      data <- eurostatRTool::sample_data_italy
    } else {
      data <- eurostatRTool::sample_data
    }
  }

  # Transform data
  data <- data %>%
    data.frame(stringsAsFactors = FALSE) %>%
    dplyr::mutate(date = as.Date(date,"%Y-%m-%d"))
  return(data)

}


# Setup the geo_labels dataframe (in the new version, it's called dim_labels)
#
# @return A data frame.
#
# @import utils tools
geo_labels <- function(){

  # If there is a dim-labels.csv file available in the user memory, read it.
  # If not, read example data for Eurostatistics or Italy, depending from the
  # selected scenario
  user_data_path <- tools::R_user_dir("eurostatRTool", which = "data")
  geo_labels_path <- file.path(user_data_path, "dim-labels.csv")
  geo_labels <- NULL
  if (file.exists(geo_labels_path)) {
    geo_labels <- utils::read.csv(geo_labels_path)
  } else {
    scenario <- read_user_config_param("scenario")
    if (!is.null(scenario) && scenario == "nsi") {
      geo_labels <- eurostatRTool::sample_dim_labels_italy
    } else {
      geo_labels <- eurostatRTool::sample_dim_labels
    }
  }

  # If the group column is not set, add it with 1 as default group
  if (!"group" %in% colnames(geo_labels)) {
    geo_labels["group"] <- 1
  }
  return(geo_labels)

}


# Get colors palette used for line charts
#
# @return A list.
colors_palette <- function() {

  # If there is a colors palette in the user configuration file, read from it
  colors_palette <- read_user_config_param("colors_palette")
  if (!is.null(colors_palette)) {
    return(colors_palette)
  }
  # Else use Eurostat or Italy palette
  scenario <- read_user_config_param("scenario")
  if (is.null(scenario) || scenario != "nsi") {
    # EC Blue, EC Yellow + get 34 colours palette from Eurostat palette A, B and C. Additional options:
    # #7F1148 (darkest berry), #EFD18C (lightest gold), #A2DDB2 (lightest green),
    # #A5DAF9 (lightest cornflower).
    # Creating additional colours to have palette with number of colours equal
    # to number of geo lables minus 2 in geoLevelLabels.
    return(c('#2644A7', '#9CC1FA', '#738CE5', '#17256B', '#B09120', '#EFD18C',
             '#C6AF5D', '#866110', '#208486', '#92C1C0', '#5FADAD', '#00525C',
             '#388AE2', '#A5DAF9', '#80BFFA', '#104F99', '#E04040', '#FFA3A3',
             '#FF8080', '#982630', '#AA5F18', '#DFB18B', '#C19062', '#734921',
             '#B656BD', '#E2B3DD', '#D38DD9', '#782A78', '#672DC4', '#CAADF9',
             '#A77DF9', '#3A1380', '#AF155C', '#F5AFCC', '#DB76A5', '#7F1148'))
  }
  else {
    return(c('#244c5a', '#d22630', '#004494', '#FFD617', '#782A78', '#866110',
             '#9CC1FA', '#3A1380', '#DB76A5', '#17256B', '#80BFFA', '#B656BD',
             '#208486', '#104F99', '#A77DF9', '#6DB56D', '#33A033', '#982630',
             '#007243', '#F5AFCC', '#CAADF9', '#388AE2', '#672DC4', '#C19062',
             '#D38DD9', '#AF155C', '#734921', '#AA5F18', '#C6AF5D', '#5FADAD',
             '#738CE5', '#FF8080', '#DFB18B', '#92C1C0', '#FFA3A3', '#E2B3DD'))
  }

}


# Get colours palette used for bar charts
#
# @return A list.
bar_chart_colors_palette <- function() {

  # If there is a colors palette in the user configuration file, read from it
  bar_chart_colors_palette <- read_user_config_param("bar_chart_colors_palette")
  if (!is.null(bar_chart_colors_palette)) {
    return(bar_chart_colors_palette)
  }
  # Else use Eurostat or Italy palette
  scenario <- read_user_config_param("scenario")
  if (is.null(scenario) || scenario != "nsi") {
    # Use the Eurostat palette: EC Blue 50, EC Blue
    return(c('#2644A7', '#B09120'))
  }
  else {
    return(c('#244c5a', '#d22630'))
  }

}


# Get the menu categories
#
# @return A list.
menu_categories <- function() {

  # Get the menu category, depending on the selected scenario
  scenario <- read_user_config_param("scenario")
  if (!is.null(scenario) && scenario == "nsi") {
    return(c("Economic situation", "Prices outlook", "Foreign trade",
             "Labour market outlook"))
  }
  return(c("Economy", "Business and Trade", "Labour market",
           "Cyclical indicators", "Trend-cycle estimates", "Forecasts"))

}


# Get the dashboard structure
#
# @return A data frame.
#
# @import dplyr xlsx tools
dashboard_structure_data <- function(indicators, dims=NULL) {

  # If there is a dashboard-structure.xlsx file available in the user memory,
  # read it. If not, read example data for Eurostatistics or Italy, depending
  # from the selected scenario
  user_data_path <- tools::R_user_dir("eurostatRTool", which = "data")
  dashboard_structure_path <- file.path(user_data_path, "dashboard-structure.xlsx")
  structure_data <- NULL
  if (file.exists(dashboard_structure_path)) {
    structure_data <- xlsx::read.xlsx(dashboard_structure_path, sheetIndex=1)
  } else {
    scenario <- read_user_config_param("scenario")
    if (!is.null(scenario) && scenario == "nsi") {
      structure_data <- eurostatRTool::sample_dashboard_structure_data_italy
    }
    else {
      structure_data <- eurostatRTool::sample_dashboard_structure_data
    }
  }

  structure_data <- as.data.frame(structure_data)
  cols <- colnames(structure_data)

  # If one of the secondary columns for a visualisation typology is not
  # present, set the primary column to FALSE. Else if all the columns
  # contains allowed values, set it to TRUE
  # Timeline
  req_cols <- c("timeline", "timeline_ind", "timeline_subtitle",
                "timeline_source")
  if (!all(req_cols %in% cols)) {
    structure_data["timeline"] <- FALSE
  } else {
    structure_data <- structure_data %>%
      dplyr::mutate(
        timeline = dplyr::case_when(
          (timeline == "TRUE" & timeline_ind %in% indicators &
             !is.na(timeline_source)) ~ TRUE,
          .default = FALSE
        ))
  }
  # Cross-country
  req_cols <- c("comp_barchart", "comp_barchart_ind", "comp_barchart_subtitle",
                "comp_barchart_source")
  if (!all(req_cols %in% cols)) {
    structure_data["comp_barchart"] <- FALSE
  } else {
    structure_data <- structure_data %>%
      dplyr::mutate(
        comp_barchart = dplyr::case_when(
          (comp_barchart == "TRUE" & comp_barchart_ind %in% indicators &
             !is.na(comp_barchart_source)) ~ TRUE,
          .default = FALSE
        ))
  }
  # Double scale
  req_cols <- c("double_scale", "double_scale_ind", "double_scale_dim",
                "double_scale_subtitle", "double_scale_source")
  if (!all(req_cols %in% cols)) {
    structure_data["double_scale"] <- FALSE
  } else {

    check_double_scale_ind <- function(double_scale_ind) {
      double_scale_inds <- c()
      for (suffix in c("IND", "HP_TREND", "HP_CYCLE", "CF_TREND", "CF_CYCLE",
                       "UC_TREND", "UC_CYCLE")) {
        double_scale_inds <- c(double_scale_inds,
                               paste0("TCD_", double_scale_ind, "_", suffix))
      }
      return(all(double_scale_inds %in% indicators))
    }

    structure_data <- structure_data %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        double_scale = ifelse(
          !is.na(double_scale_ind), check_double_scale_ind(double_scale_ind), FALSE)) %>%
      dplyr::ungroup()

    structure_data <- as.data.frame(structure_data) %>%
      dplyr::mutate(
        double_scale = dplyr::case_when(
          (double_scale == "TRUE" &
             double_scale_dim %in% dims &
             !is.na(double_scale_source)) ~ TRUE,
          .default = FALSE
        ))
  }
  # Point line chart
  req_cols <- c("point_linechart", "point_linechart_dim", "point_linechart_ind_point_linechart",
                "point_linechart_ind_gdp", "point_linechart_subtitle",
                "point_linechart_source")
  if (!all(req_cols %in% cols)) {
    structure_data["point_linechart"] <- FALSE
  } else {
    structure_data <- structure_data %>%
      dplyr::mutate(
        point_linechart = dplyr::case_when(
          (point_linechart == "TRUE" & point_linechart_dim %in% dims &
             point_linechart_ind_point_linechart %in% indicators &
             point_linechart_ind_gdp %in% indicators &
             !is.na(point_linechart_source)) ~ TRUE,
          .default = FALSE
        ))
  }
  # Forecast chart
  req_cols <- c("forecast_chart", "forecast_chart_ind",
                "forecast_chart_xlsx_sheet_name", "forecast_chart_source")
  if (!all(req_cols %in% cols)) {
    structure_data["forecast_chart"] <- FALSE
  } else {
    structure_data <- structure_data %>%
      dplyr::mutate(
        forecast_chart = dplyr::case_when(
          (forecast_chart == "TRUE" &
             !is.na(forecast_chart_source)) ~ TRUE,
          .default = FALSE
        ))
  }
  # Map
  req_cols <- c("map", "map_ind", "map_subtitle", "map_source")
  if (!all(req_cols %in% cols)) {
    structure_data["map"] <- FALSE
  } else {
    if (!("map_legend_title" %in% cols)) {
      structure_data[, "map_legend_title"] <- NA
    }
    structure_data <- structure_data %>%
      dplyr::mutate(
        map = dplyr::case_when(
          (map == "TRUE" & map_ind %in% indicators &
           !is.na(map_source)) ~ TRUE,
          .default = FALSE
        ),
        map_legend_title = dplyr::case_when(
          (!is.na(map_legend_title)) ~ map_legend_title,
          .default = NULL
        ))
  }
  # Table
  req_cols <- c("table", "table_ind",
                "table_column_names", "table_source")
  if (!all(req_cols %in% cols)) {
    structure_data["table"] <- FALSE
  } else {
    structure_data <- structure_data %>%
      dplyr::mutate(
        table = dplyr::case_when(
          (table == "TRUE" & !is.na(table_column_names) &
             !is.na(table_source)) ~ TRUE,
          .default = FALSE
        ))
  }

  # From the box position, get the element position in the primary and secondary
  # menus
  structure_data <- structure_data %>%
    dplyr::filter(!is.na(box)) %>%
    dplyr::mutate(
      box = as.numeric(box),
      menu_position = floor(box),
      indicator_position = round(((box - menu_position) * 10))) %>%
    dplyr::select(-box)

  structure_data

}


# Get the forecast data
#
# @return A data frame.
#
# @import dplyr xlsx tidyr tools
forecast_data <- function() {

  user_data_path <- tools::R_user_dir("eurostatRTool", which = "data")
  forecast_path <- file.path(user_data_path, "forecasts.xlsx")
  # If exist a forecasts.xlsx file in the user device
  if (file.exists(forecast_path)) {
    path <- forecast_path
  } else {
    scenario <- read_user_config_param("scenario")
    if (!is.null(scenario) && scenario == "nsi") {
      path <- system.file("extdata", "forecasts-italy.xlsx", package = "eurostatRTool")
    }
    else {
      path <- system.file("extdata", "forecasts.xlsx", package = "eurostatRTool")
    }
  }
  # Get all the sheet names in the file
  wb <- xlsx::loadWorkbook(path)
  sheet_names <- names(xlsx::getSheets(wb))
  # For any sheet in the file
  for (name in sheet_names) {
    # Read the data in the sheet
    sheet_data <- xlsx::read.xlsx(path, sheetName=name, header=FALSE)
    # Distinguish between data format type 1 (2 y axes) and type 2 (1 y axes)
    # If there is y2 (data format type 1)
    if (is.na(sheet_data[1,2])) {
      # Rename first columns
      sheet_data[1,1] <- "entity"
      sheet_data[2,1] <- "entity"
      sheet_data[1,2] <- "period"
      sheet_data[2,2] <- "period"
      # List the indicators appearing in the sheet
      indicators <- unique(as.character(sheet_data[1,][!(sheet_data[1,] %in% c("entity", "period", "NA"))]))
      # For any indicator in the sheet
      for (indicator in indicators) {
        # Select the columns for the indicator
        indicator_data <-
          sheet_data[,(sheet_data[1,] %in% c(indicator, "entity", "period"))]
        # Set the colnames
        colnames(indicator_data) <- indicator_data[2,]
        # Transform the dataframe from wide to long, to be ready to be used
        # for building the graphs
        indicator_data <-
          indicator_data[3:nrow(indicator_data),] %>%
          dplyr::filter(!is.na(entity), !is.na(period)) %>%
          tidyr::pivot_longer(cols=-c("entity", "period"),
                              names_to="year", values_to="value")
        # Transform the value to numeric to avoid type errors
        indicator_data$value <- as.numeric(indicator_data$value)
        # Assign the forecast data for the sheet and indicator to a
        # global variable
        assign(paste0("forecast_data_", name, "_", indicator), indicator_data,
               envir = .pkgglobalenv)
      }
    } else { # Data format type 2
      # Rename first column
      sheet_data[1,1] <- "geo"
      # Set the colnames
      colnames(sheet_data) <- sheet_data[1,]
      # Transform the dataframe, to be ready to be used for building the graphs
      sheet_data <-
        sheet_data[2:nrow(sheet_data),] %>%
        dplyr::filter(!is.na(geo)) %>%
        dplyr::mutate(across(-geo, as.numeric))
      # Assign the forecast data for the sheet to a global variable
      assign(paste0("forecast_data_", name), sheet_data, envir = .pkgglobalenv)
    }
  }

}


#' Get the logo
#'
#' @return The logo path.
#'
#' @import tools
#' @export
logo <- function() {

  user_data_path <- tools::R_user_dir("eurostatRTool", which = "data")
  logo_path <- file.path(user_data_path, "logo.png")
  if (!file.exists(logo_path)) {
    scenario <- read_user_config_param("scenario")
    if (!is.null(scenario) && scenario == "nsi") {
      logo_path <- system.file("png", "istat.png", package = "eurostatRTool")
    } else {
      logo_path <- system.file("png", "eurostat.png", package = "eurostatRTool")
    }
  }

  # self_contained false do not copy external files... I'm manually doing it
  resources_folder <- file.path(.pkgglobalenv$output_path, "visualisation_files")
  if (!dir.exists(resources_folder)) {
    dir.create(resources_folder, recursive=TRUE)
  }
  file.copy(logo_path, resources_folder, overwrite = TRUE)
  relative_path <- file.path("visualisation_files", basename(logo_path))

  return(relative_path)

}


# Get the logo link
#
# @return An url.
logo_link <- function() {

  # If there is a logo link in the user configuration file, read from it
  logo_link <- read_user_config_param("logo_link")
  if (!is.null(logo_link)) {
    return(logo_link)
  }

  # Else use the Eurostat or Italy logo link
  scenario <- read_user_config_param("scenario")
  if (!is.null(scenario) && scenario == "nsi") {
    return("https://www.istat.it/")
  }
  else {
    return("https://ec.europa.eu/eurostat/web/main/home")
  }

}


#' Get the css
#
#' @return A css list.
#
#' @import tools
#' @export
css <- function() {

  # The basic css stylesheet depending if storyboard or dropdown menu
  menu <- read_user_config_param("menu")
  if (is.null(menu) || menu != "dropdown") {
    css <- system.file("css", "style.css", package = "eurostatRTool")
  }
  else {
    css <- system.file("css", "dropdown-style.css", package = "eurostatRTool")
  }

  # Add the css for the colors. If exists, read it from user memory. If not,
  # read the standard colors
  user_data_path <- tools::R_user_dir("eurostatRTool", which = "data")
  colors_path <- file.path(user_data_path, "colors.css")
  if (!file.exists(colors_path)) {
    scenario <- read_user_config_param("scenario")
    if (is.null(scenario) || scenario != "nsi") {
      colors_path <- system.file("css", "colors.css", package = "eurostatRTool")
    }
    else {
      colors_path <- system.file("css", "colors-italy.css", package = "eurostatRTool")
    }
  }
  css <- c(css, colors_path)

  # Add the css stylesheet for the logo if it needs to be on the right side
  logo_position <- read_user_config_param("logo_position")
  if (!is.null(logo_position)) {
    if (logo_position == "right") {
      logo_path <- system.file("css", "logo-right.css", package = "eurostatRTool")
      css <- c(css, logo_path)
    }
  } else {
    scenario <- read_user_config_param("scenario")
    if (is.null(scenario) || scenario != "nsi") {
      logo_path <- system.file("css", "logo-right.css", package = "eurostatRTool")
      css <- c(css, logo_path)
    }
  }

  # self_contained false do not copy external files... I'm manually doing it
  resources_folder <- file.path(.pkgglobalenv$output_path, "visualisation_files")
  if (!dir.exists(resources_folder)) {
    dir.create(resources_folder, recursive=TRUE)
  }
  css_new <- c()
  for (file in css) {
    file.copy(file, resources_folder, overwrite = TRUE)
    css_new <- c(css_new, file.path("visualisation_files", basename(file)))
  }

  return(css_new)

}


# Get the title
#
# @return The dashboard title.
title <- function() {

  # If there is a title in the user configuration file, read from it
  title <- read_user_config_param("title")
  if (!is.null(title)) {
    return(title)
  }

  # Else use the Eurostat or Italy title
  scenario <- read_user_config_param("scenario")
  if (!is.null(scenario) && scenario == "nsi") {
    return("Economic situation in Italy")
  }
  else {
    return("Eurostatistics 12/2023")
  }

}


# Get the dashboard edition text
#
# @return The edition text.
edition <- function() {

  # If there is an edition in the user configuration file, read from it
  edition <- read_user_config_param("edition")
  if (!is.null(edition)) {
    return(edition)
  }

  # Else use the Eurostat or Italy edition
  scenario <- read_user_config_param("scenario")
  if (!is.null(scenario) && scenario == "nsi") {
    return("Edition: December 2023")
  }
  else {
    return("Edition: December 2023. Next update: 26 January 2024")
  }

}


#' Get the social
#'
#' @return The social.html.
#'
#' @import tools
#' @export
social <- function() {

  # If exists a social.html in the user memory, read it
  user_data_path <- tools::R_user_dir("eurostatRTool", which = "data")
  social_path <- file.path(user_data_path, "social.html")
  if (file.exists(social_path)) {
    # Transform the absolute path into relative one, for passing it to the rmd
    rel_path <- relative_path(social_path, getwd())
    return(rel_path)
  } else {
    scenario <- read_user_config_param("scenario")
    if (!is.null(scenario) && scenario == "nsi") {
      social_path <- file.path("..", "extra_html", "social-italy.html")
    } else {
      social_path <- file.path("..", "extra_html", "social.html")
    }
    return(social_path)
  }

}


#' Get the footer
#'
#' @return The footer.html.
#'
#' @import tools
#' @export
footer <- function() {

  # If exists a footer.html in the user memory, read it
  user_data_path <- tools::R_user_dir("eurostatRTool", which = "data")
  footer_path <- file.path(user_data_path, "footer.html")
  if (file.exists(footer_path)) {
    # Transform the absolute path into relative one, for passing it to the rmd
    rel_path <- relative_path(footer_path, getwd())
    return(rel_path)
  } else {
    scenario <- read_user_config_param("scenario")
    if (!is.null(scenario) && scenario == "nsi") {
      footer_path <- file.path("..", "extra_html", "footer-italy.html")
    } else {
      footer_path <- file.path("..", "extra_html", "footer.html")
    }
    return(footer_path)
  }

}


# Get the overview
#
# @return The overview text.
overview_text <- function() {

  # If there is an overview text in the user configuration file, read from it
  overview <- read_user_config_param("overview")
  overview_text <- read_user_config_param("overview_text")
  if (!is.null(overview_text)) {
    return(overview_text)
  }
  if (!is.null(overview) && overview == TRUE) {
    return(NULL)
  }

  # Else use the Eurostat or Italy overview text
  scenario <- read_user_config_param("scenario")
  if (!is.null(scenario) && scenario == "nsi") {
    return("_**Data and interpretation for short-term economic analysis**
           showing the evolution of the economic activity in **Italy**.
           This review gives a synthetic picture of the economic
           situation in the recent past in the country._")
  }
  else {
    return("_**Data and interpretation for short-term economic analysis**
           showing the evolution of the economic activity in the
           [European Union](https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Glossary:European_Union_(EU))
           (EU), [Euro area](https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Glossary:Euro-area)
           (EA) and Member States. The tool complements a
           [monthly article](https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Eurostatistics_-_data_for_short-term_economic_analysis)
           offering additional indicators and interactive graphs linked to
           source data. This monthly review gives a synthetic picture of the
           macroeconomic situation in the recent past. It is based on Principal
           European Economic Indicators (PEEIs), complemented by some business
           cycle indicators._")
  }

}


# Get the latest changes summary text
#
# @return The latest changes summary text.
latest_changes_summary <- function() {

  # If there is a latest changes summary in the user configuration file, read from it
  overview <- read_user_config_param("overview")
  latest_changes_summary <- read_user_config_param("latest_changes_summary")
  if (!is.null(latest_changes_summary)) {
    return(latest_changes_summary)
  }
  if (!is.null(overview) && overview == TRUE) {
    return(NULL)
  }

  # Else use the Eurostat or Italy latest changes summary
  scenario <- read_user_config_param("scenario")
  if (!is.null(scenario) && scenario == "nsi") {
    return("At the end of 2023, international economic perspectives remain
           uncertain due to geopolitical tensions, the resolution of which is
           not imminent, and still tight financial conditions for households
           and firms.")
  }
  else {
    return("Euro area inflation continues to decelerate, while economics
           sentiment picks up and retail trade increases. Industrial production
           declines.")
  }

}


# Get the overview value boxes
#
# @return The value boxes values and texts.
value_boxes <- function() {

  # If there are value boxes in the user configuration file, read from it
  valueboxes <- read_user_config_param("valueboxes")
  if (!is.null(valueboxes)) {
    values <- c()
    texts <- c()
    icons <- c()
    for (i in 1:3) {
      value <- read_user_config_param(paste0("valuebox", i, "_value"))
      text <- read_user_config_param(paste0("valuebox", i, "_text"))
      icon <- read_user_config_param(paste0("valuebox", i, "_icon"))
      if (!is.null(value) && !is.null(text) && !is.null(icon)) {
        values <- c(values, value)
        texts <- c(texts, text)
        icons <- c(icons, icon)
      }
    }
    if (length(values) > 0 && length(texts) > 0 && length(icons) > 0) {
      return(data.frame(text = texts, value = values, icon = icons))
    } else {
      return(NULL)
    }
  }

  # Else use the Eurostat or Italy latest changes summary
  scenario <- read_user_config_param("scenario")
  if (!is.null(scenario) && scenario == "nsi") {
    return(data.frame(text = c("**GDP** (Italy, 3^rd^ quarter 2023)",
                               "**Economic sentiment** (ESI) (Italy, December 2023)",
                               "**Harmonized consumer price index (HICP)** (December 2023)"),
                      value = c("+0.2%", "+2.6 points", "+0.5%"),
                      icon = c("fa-up-long", "fa-up-long", "fa-up-long")
    ))
  }
  else {
    return(data.frame(text = c("**GDP** (Euro Area, 3^rd^ quarter 2023)",
                               "**Economic sentiment** (Euro Area, November 2023)",
                               "**Industrial production** (Euro Area, October 2023)"),
                      value = c("-0.1%", "+0.3 points", "-0.7%"),
                      icon = c("fa-down-long", "fa-up-long", "fa-down-long")
    ))
  }

}


# Get if the dropdown menu is enabled or disabled (storyboard layout)
#
# @return TRUE if the dropdown menu is enabled, else FALSE
dropdown_menu <- function() {

  # If the menu is set in the configuration file as dropdown, then return TRUE.
  # Else return FALSE (storyboard layout)
  menu <- read_user_config_param("menu")
  if (is.null(menu) || menu != "dropdown") {
    return(FALSE)
  }
  else {
    return(TRUE)
  }

}


# Get the meta description
#
# @return The meta description.
meta_description <- function() {

  # If there is a meta description in the user configuration file, read from it
  meta_description <- read_user_config_param("meta_description")
  if (!is.null(meta_description)) {
    return(meta_description)
  }

  # Else use the Eurostat or Italy meta description
  scenario <- read_user_config_param("scenario")
  if (!is.null(scenario) && scenario == "nsi") {
    return("Data visualization and interpretation for short-term economic
    analysis showing the evolution of the economic activity in Italy.")
  }
  else {
    return("Data visualization and interpretation for short-term economic
    analysis showing the evolution of the economic activity in the European
    Union (EU), euro area (EA) and/or Member States.")
  }

}


# Get the states SpatialPolygonsDataFrame
#
# @return Assign the states SpatialPolygonsDataFrame to a package global variable.
states <- function() {
  states <- eurostatRTool::states
  # Assign the states to a package global variable
  assign("states", states, envir = .pkgglobalenv)
}


# Get the gisco_countries SpatialPolygonsDataFrame
#
# @return Assign the gisco_countries SpatialPolygonsDataFrame to a package global variable.
gisco_countries <- function() {
  gisco_countries <- eurostatRTool::gisco_countries
  # Assign the states to a package global variable
  assign("gisco_countries", gisco_countries, envir = .pkgglobalenv)
}


utils::globalVariables(c("box", "menu_position", "entity", "period", "geo",
                         "double_scale_ind"))
