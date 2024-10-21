test_that("data frame is transformed", {
data <- data.frame(
  lon = c(77.10, 88.36, 80.27, 72.87),
  lat = c(28.71, 22.57, 13.08, 19.07)
)

  result <- sf::st_as_sf(
    data.frame(
      geometry = sf::st_sfc(
        sf::st_point(c(77.1, 28.71)),
        sf::st_point(c(88.36, 22.57)),
        sf::st_point(c(80.27, 13.08)),
        sf::st_point(c(72.87, 19.07))
      )
    ),
    crs = mapindia_crs()
  )

  expect_equal(mapindia_transform(data), result, tolerance = 1e-02)
})

test_that("sf object is transformed", {
  sf <- sf::st_as_sf(
    data.frame(
      geometry = sf::st_sfc(
        sf::st_point(c(77.1, 28.71)),
        sf::st_point(c(88.36, 22.57)),
        sf::st_point(c(80.27, 13.08)),
        sf::st_point(c(72.87, 19.07))
      )
    ),
    crs = mapindia_crs()
  )

  result <- sf::st_as_sf(
    data.frame(
      geometry = sf::st_sfc(
        sf::st_point(c(77.1, 28.71)),
        sf::st_point(c(88.36, 22.57)),
        sf::st_point(c(80.27, 13.08)),
        sf::st_point(c(72.87, 19.07))
      )
    ),
    crs = mapindia_crs()
  )

  expect_equal(mapindia_transform(sf), result, tolerance = 1e-02)
})

test_that("sf object with non-lon/lat CRS is transformed", {
  sf <- sf::st_as_sf(
    data.frame(
      geometry = sf::st_sfc(
        sf::st_point(c(77.1, 28.71)),
        sf::st_point(c(88.36, 22.57)),
        sf::st_point(c(80.27, 13.08)),
        sf::st_point(c(72.87, 19.07))
      )
    ),
    crs = mapindia_crs()
  )

  result <- sf::st_as_sf(
    data.frame(
      geometry = sf::st_sfc(
        sf::st_point(c(77.1, 28.71)),
        sf::st_point(c(88.36, 22.57)),
        sf::st_point(c(80.27, 13.08)),
        sf::st_point(c(72.87, 19.07))
      )
    ),
    crs = mapindia_crs()
  )

  # test without explicit CRS
  expect_equal(mapindia_transform(sf), result, tolerance = 1e-02)
  # test with explicit CRS
  expect_equal(mapindia_transform(sf, crs = sf::st_crs(4326)), result, tolerance = 1e-02)
})

test_that("error occurs for data with less than 2 columns", {
  invalid_data <- data.frame(
    lon = c(-74.01, -95.36, -118.24, -87.65)
  )

  expect_error(mapindia_transform(invalid_data))
  expect_error(mapindia_transform(data.frame()))
})

test_that("error occurs for invalid input names", {
  data <- data.frame(
    lon = c(-74.01, -95.36, -118.24, -87.65),
    lat = c(40.71, 29.76, 34.05, 41.85)
  )

  expect_error(mapindia_transform(data, input_names = c("longitude")))
  expect_error(mapindia_transform(data, input_names = c("longitude", "latitude")))
})

test_that("error occurs for data with non-numeric columns", {
  invalid_data1 <- data.frame(
    lon = c("a", "b", "c"),
    lat = c("d", "e", "f")
  )

  invalid_data2 <- data.frame(
    lon = c("a", "b", "c"),
    lat = c(1, 2, 3)
  )

  invalid_data3 <- data.frame(
    lon = c(1, 2, 3),
    lat = c("d", "e", "f")
  )

  expect_error(mapindia_transform(invalid_data1))
  expect_error(mapindia_transform(invalid_data2))
  expect_error(mapindia_transform(invalid_data3))
})
