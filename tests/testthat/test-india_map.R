test_that("state data frame is returned", {
  data <- map_india()

  expect_equal(ncol(data), 4)
  expect_equal(nrow(data), 37)

  expect_identical(map_india("state"), data)
  expect_identical(map_india("states"), data)
})

test_that("county data frame is returned", {
  data <- map_india("districts")

  expect_equal(ncol(data), 5)
  expect_equal(nrow(data), 755)

  expect_identical(map_india("district"), data)
})
