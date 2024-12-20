% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map_india.R
\name{map_india}
\alias{map_india}
\title{Retrieve India map data}
\usage{
map_india(
  regions = c("states", "state", "districts", "district"),
  include = c(),
  exclude = c()
)
}
\arguments{
\item{regions}{The region breakdown for the map, can be one of
(\code{"states"}, \code{"state"}, \code{"districts"}, \code{"district"}).
The default is \code{"states"}.}

\item{include}{The regions to include in the resulting map. If \code{regions} is
\code{"states"}/\code{"state"}, the value can be either a state name, abbreviation or code.
For districts, the district codes must be provided as there can be multiple districts with the
same name. If states are provided in the districts map, only districts in the included states
will be returned.}

\item{exclude}{The regions to exclude in the resulting map. If \code{regions} is
\code{"states"}/\code{"state"}, the value can be either a state name, abbreviation or code.
For districts, the district codes must be provided as there can be multiple districts with the
same name. The regions listed in the \code{include} parameter are applied first and the
\code{exclude} regions are then removed from the resulting map. Any excluded regions
not present in the included regions will be ignored.}
}
\value{
A data frame of India map coordinates divided by the desired \code{regions}.
}
\description{
Retrieve India map data
}
\examples{
str(map_india())

df <- map_india(regions = "districts")
states <- map_india(include = c("WB", "NCT", "AP"))

}
\seealso{
[mapindiatools::map_india()] of which this function is a wrapper for.
}
