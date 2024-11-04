#' Join district or state level data to India mapping data
#'
#' @inheritParams map_india
#' @param data The data that should be joined to a India map. This
#'   parameter should be a data frame consisting of two columns,
#'   a code (2 characters for state, 5 characters for district where first 2
#'   characters correspond to the respective state) and the value that should
#'   be associated with that region. The columns of \code{data} \emph{must} be
#'   \code{code} or \code{state} and the value of the `values` parameter. If
#'   both \code{code} and \code{state} are provided, this function uses the
#'   \code{code}.
#' @param values The name of the column that contains the values to be associated
#'   with a given region. The default is \code{"values"}.
#' @param na The value to be inserted for states or districts that don't have
#'   a value in \code{data}. This value must be of the same type as the \code{value}
#'   column of \code{data}.
#'
#' @return A data frame composed of the map data frame (from [map_india()]) except
#'   an extra column containing the values in \code{data} is included.
#'
#'   The result can be plotted using [ggplot2::ggplot()] or [plot_india()].
#'
#' @seealso [plot_india()]
#'
#' @examples
#' data_01 <- data.frame(code = c("01", "02", "04"), values = c(1, 5, 8))
#' df <- map_using_data(data_01, na = 0)
#'
#' data_02 <- data.frame(state = c("AP", "WB", "Tamil Nadu"), values = c(6, 9, 3))
#' df <- map_using_data(data_02, na = 0)
#'
#' @export
map_using_data <- function(data,
                           values = "values",
                           include = c(),
                           exclude = c(),
                           na = NA) {

  # Validate input data frame
  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame.")
  }

  # Handle empty data case
  if (nrow(data) == 0) {
    region_type <- if (length(include) == 0 || nchar(include[1]) == 2) "state" else "district"
    rlang::warn(paste("`data` is empty, returning basic", region_type, "India map data frame."))
    return(mapindia::map_india(regions = region_type, include = include, exclude = exclude))
  }

  # Validate values column existence
  if (!values %in% names(data)) {
    rlang::abort(paste0("\"", values, "\" column not found in `data`."))
  }

  # Ensure 'code' column exists; derive from 'state' if absent
  if (!"code" %in% names(data)) {
    if ("state" %in% names(data)) {
      data$code <- mapindia::codes(data$state)
    } else {
      rlang::abort("`data` must contain either a `state` or `code` column.")
    }
  }

  # Prepare `code11` and set region type
  data$code11 <- sprintf(ifelse(nchar(data$code[1]) <= 2, "%02d", "%05d"), as.numeric(data$code))
  region_type <- if (nchar(data$code11[1]) <= 2) "state" else "district"

  # Fetch map data and merge with input data
  map_df <- mapindia::map_india(regions = region_type, include = include, exclude = exclude)

  # Remove any columns in data that conflict with map_df columns
  data <- data[!names(data) %in% c("abbr", "stname", "dtname", "geom")]

  # Merge and handle NA values
  result <- merge(map_df, data, by = "code11", all.x = TRUE, sort = FALSE)
  result[[values]][is.na(result[[values]])] <- na

  # Order results by region and specific attributes
  if (region_type == "state") {
    result <- result[order(result$stname), ]
  } else {
    result <- result[order(result$stname, result$dtname), ]
  }

  result
}
