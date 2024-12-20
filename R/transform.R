#' Convert spatial data to mapindia projection
#'
#' @description Converting a spatial object of map coordinates will
#'   allow those points to line up with the regular mapindia plot by applying
#'   the same World Geodetic System 1984 projection to those points as well.
#'
#'   The input `data` is assumed to contain longitude and latitude coordinates
#'   by default. If this is not the case, provide an [sf::st_crs] object
#'   to the `crs` parameter with the appropriate coordinate reference system.
#'
#' @param data A data frame containing coordinates in a two column format
#'   where the first column represents longitude and the second data frame
#'   represents latitude. The names of the data frame column do not matter,
#'   just that the order of the columns is kept intact.
#'
#' @param ... Additional parameters passed onto [sf::st_as_sf].
#'   By default, `crs = sf::st_crs(4326)` is used, implying longitude and latitude
#'   coordinates.
#'
#' @param input_names A character vector of length two which specifies the
#'   longitude and latitude columns of the input data (the ones that should be
#'   transformed), respectively. Only required if the input data is
#'   a `data.frame` object. Defaults to `c("lon", "lat")`.
#'
#' @return An `sf` object containing the transformed coordinates from the
#'   input data frame with the World Geodetic System 1984 projection applied.
#'   The transformed columns will be appended to the data frame so that all
#'   original columns should remain intact.
#'
#' @examples
#' data <- data.frame(
#'   lon = c(77.10, 88.36, 80.27, 72.87, 77.59, 78.03),
#'   lat = c(28.71, 22.57, 13.08, 19.07, 12.97, 30.31),
#'   pop = c(8398748, 2325502, 3990456, 2705994, 32113, 347397)
#' )
#'
#' # Transform data
#' transformed_data <- mapindia_transform(data)
#'
#' # Plot transformed data on map
#' library(ggplot2)
#'
#' plot_india() + geom_sf(
#'   data = transformed_data,
#'   aes(size = pop),
#'   color = "red", alpha = 0.5
#' )
#'
#' @rdname mapindia_transform
#' @export
mapindia_transform <- function(data, ...) {
  UseMethod("mapindia_transform")
}

#' @rdname mapindia_transform
#' @export
mapindia_transform.sf <- function(data, ...) {
  perform_transform(data, ...)
}

#' @rdname mapindia_transform
#' @export
mapindia_transform.data.frame <- function(data,
                                          ...,
                                          input_names = c("lon", "lat")) {
  # ensure input is data.frame
  data <- as.data.frame(data)

  # validation
  if (length(input_names) != 2 && !any(is.na(as.character(input_names)))) {
    stop("`input_names` must be a character vector of length 2.")
  } else {
    input_names <- as.character(input_names)
  }

  if (!all(input_names %in% colnames(data))) {
    stop("All `input_names` must exist as column names in `data`.")
  }

  if (ncol(data) < 2 ||
      !is.numeric(data[, input_names[1]]) ||
      !is.numeric(data[, input_names[2]])) {
    stop("`data` must contain at least two numeric columns.")
  }

  # convert to sf and perform transformation
  data <- sf::st_as_sf(data, coords = input_names)
  perform_transform(data, ...)
}

#' Transform `sf` coordinates to `mapindia` transform
#'
#' Internal function with common functionality for transforming coordinates.
#' Using this function directly is not recommended.
#'
#' @keywords internal
perform_transform <- function(data, ...) {
  data_sf <- sf::st_as_sf(data, ...)

  if (is.na(sf::st_crs(data_sf))) {
    crs <- list(...)[["crs"]]
    if (is.null(crs)) crs <- sf::st_crs(4326)
    sf::st_crs(data_sf) <- crs
  }

  # Transform to canonical projection
  transformed <- sf::st_transform(data_sf, mapindia_crs())
  sf::st_agr(transformed) <- "constant"

  transformed
}

#' mapindia coordinate reference system
#'
#' @description This coordinate reference system (CRS) represents
#' the canonical projection used by the \code{mapindia} package. It can
#' be used to transform shape files, spatial points, spatial data
#' frames, etc. to the same coordinate representation that is used
#' by the \code{plot_india} function.
#'
#' @return An `sf::st_crs` object representing the Coordinate Reference System (CRS)
#' for India, specifically EPSG:4326 (WGS 84). This CRS uses latitude and longitude
#' coordinates and is commonly used for geographic data.
#'
#' @export
#'
#' @examples
#' mapindia_crs()
mapindia_crs <- function() {
  mapindiatools:::ea_crs()
}
