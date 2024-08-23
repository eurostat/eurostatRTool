#' Gisco countries SpatialPolygonsDataFrame
#'
#' This is the countries SpatialPolygonsDataFrame, included to draw the polygons
#' for the all the countries in the maps. It is obtained from the giscoR package.
#'
#' Generation process:
#'
#' install.packages("giscoR")
#'
#' install.packages("sf")
#'
#' install.packages("sp")
#'
#' library(giscoR)
#'
#' library(sf)
#'
#' library(sp)
#'
#' # Get countries borders
#'
#' borders <- giscoR::gisco_get_countries(epsg = "4326", year = "2020", spatialtype = "RG", resolution = "60")
#'
#' gisco_countries <- sf::as_Spatial(sf::st_geometry(borders), IDs = as.character(1:nrow(borders)))
#'
#' # Grab the data from the sf object
#'
#' df <- borders
#'
#' df$geometry <- NULL
#'
#' df <- as.data.frame(df)
#'
#' # Create the SpatialPolygonsDataFrame
#'
#' gisco_countries <- sp::SpatialPolygonsDataFrame(gisco_countries, data = df)
#'
#' use_data(gisco_countries, overwrite = TRUE)
#'
#' @source https://cran.r-project.org/web/packages/giscoR/giscoR.pdf
"gisco_countries"
