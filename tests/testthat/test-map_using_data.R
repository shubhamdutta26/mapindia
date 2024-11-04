state_data <- data.frame(code = c("01", "02", "04"), values = c(3, 5, 10))
district_data <- data.frame(code = c("19333", "19336", "03049"), values = c(3, 5, 10))

test_that("values are assigned to states correctly", {
  df <- map_with_data(state_data)

  expect_equal(unique(state_data$value[state_data$codes == "01"]), unique(df$value[df$codes == "01"]))
  expect_equal(unique(state_data$value[state_data$codes == "02"]), unique(df$value[df$codes == "02"]))
  expect_equal(unique(state_data$value[state_data$codes == "04"]), unique(df$value[df$codes == "04"]))
})

test_that("values are assigned to districts correctly", {
  df <- map_with_data(district_data)

  expect_equal(unique(district_data$value[district_data$codes == "19333"]), unique(df$value[df$codes == "19333"]))
  expect_equal(unique(district_data$value[district_data$codes == "19336"]), unique(df$value[df$codes == "19336"]))
  expect_equal(unique(district_data$value[district_data$codes == "03049"]), unique(df$value[df$codes == "03049"]))
})

test_that("values are appropriately assigned if district codes are missing leading zeroes", {
  district_data2 <- data.frame(code = c(3049, 3050, 3051), values = c(3, 5, 10))
  df <- map_with_data(district_data2)

  expect_equal(unique(district_data2$value[district_data2$codes == 3049]), unique(df$value[df$codes == "03049"]))
  expect_equal(unique(district_data2$value[district_data2$codes == 3050]), unique(df$value[df$codes == "03050"]))
  expect_equal(unique(district_data2$value[district_data2$codes == 3051]), unique(df$value[df$codes == "03051"]))
})

test_that("values are appropriately assigned if state codes are missing leading zeroes", {
  state_data2 <- data.frame(code = c(1, 2, 4), values = c(3, 5, 10))
  df <- map_with_data(state_data2)

  expect_equal(unique(state_data2$value[state_data2$codes == 1]), unique(df$value[df$codes == "01"]))
  expect_equal(unique(state_data2$value[state_data2$codes == 2]), unique(df$value[df$codes == "02"]))
  expect_equal(unique(state_data2$value[state_data2$codes == 4]), unique(df$value[df$codes == "04"]))
})

test_that("error occurs for invalid column names", {
  bad_data_states <- data.frame(state_codes <- c("01", "02"), values = c(3, 5))
  bad_data_values <- data.frame(code <- c("01", "02"), the_values = c(3, 5))

  expect_error(map_with_data(bad_data_states))
  expect_error(map_with_data(bad_data_values))
})

test_that("error occurs for non-data frame input", {
  expect_error(map_with_data(data = "1"))
})

test_that("warning occurs for empty (yet valid) data frame", {
  expect_warning(map_with_data(data.frame(code = c(), values = c())))
  expect_equal(suppressWarnings(map_with_data(data.frame(code = c(), values = c()))), map_india())

  expect_warning(map_with_data(data.frame(code = c(), values = c()), include = c("01")), "*state*")
  expect_warning(map_with_data(data.frame(code = c(), values = c()), include = c("03049")), "*district*")
})
