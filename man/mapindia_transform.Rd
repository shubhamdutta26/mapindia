% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform.R
\name{mapindia_transform}
\alias{mapindia_transform}
\alias{mapindia_transform.sf}
\alias{mapindia_transform.data.frame}
\title{Convert spatial data to mapindia projection}
\usage{
mapindia_transform(data, ...)

\method{mapindia_transform}{sf}(data, ...)

\method{mapindia_transform}{data.frame}(data, ..., input_names = c("lon", "lat"))
}
\arguments{
\item{data}{A data frame containing coordinates in a two column format
where the first column represents longitude and the second data frame
represents latitude. The names of the data frame column do not matter,
just that the order of the columns is kept intact.}

\item{...}{Additional parameters passed onto [sf::st_as_sf].
By default, `crs = sf::st_crs(4326)` is used, implying longitude and latitude
coordinates.}

\item{input_names}{A character vector of length two which specifies the
longitude and latitude columns of the input data (the ones that should be
transformed), respectively. Only required if the input data is
a `data.frame` object. Defaults to `c("lon", "lat")`.}
}
\value{
An `sf` object containing the transformed coordinates from the
  input data frame with the World Geodetic System 1984 projection applied.
  The transformed columns will be appended to the data frame so that all
  original columns should remain intact.
}
\description{
Converting a spatial object of map coordinates will
  allow those points to line up with the regular mapindia plot by applying
  the same World Geodetic System 1984 projection to those points as well.

  The input `data` is assumed to contain longitude and latitude coordinates
  by default. If this is not the case, provide an [sf::st_crs] object
  to the `crs` parameter with the appropriate coordinate reference system.
}
\examples{
data <- data.frame(
  lon = c(77.10, 88.36, 80.27, 72.87, 77.59, 78.03),
  lat = c(28.71, 22.57, 13.08, 19.07, 12.97, 30.31),
  pop = c(8398748, 2325502, 3990456, 2705994, 32113, 347397)
)

# Transform data
transformed_data <- mapindia_transform(data)

# Plot transformed data on map
library(ggplot2)

plot_india() + geom_sf(
  data = transformed_data,
  aes(size = pop),
  color = "red", alpha = 0.5
)

}
