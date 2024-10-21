test_that("all codes returned when no state specified", {
  expect_equal(codes(),
               c("35", "37", "12", "18", "10", "22", "04", "25", "26", "30",
                 "24", "02", "06", "01", "20", "29", "32", "38", "31", "27",
                 "17", "14", "23", "15", "07", "13", "21", "03", "34", "08",
                 "11", "36", "33", "16", "05", "09", "19"))
})

test_that("returns correct code for state", {
  expect_equal(codes(state = "wb"), "19")
  expect_equal(codes(state = "WB"), "19")
  expect_equal(codes(state = "West Bengal"), "19")
  expect_equal(codes(state = "west bengal"), "19")
})

test_that("multiple states return appropriate codes", {
  expect_equal(codes(c("WB", "TN", "AP", "XX", "NCT")), c("19", "33", "37", NA, "07"))
  expect_equal(codes(c("WB", "tamil Nadu", "AP", "XX", "dELhI")), c("19", "33", "37", NA, "07"))
})

test_that("returns correct code for district", {
  expect_equal(codes(state = "WB", district = "kolkata"), "19342")
  expect_equal(codes(state = "WB", district = "Kolkata"), "19342")
})

test_that("multiple districts in same state return appropriate codes", {
  expect_equal(codes(state = "WB", district = c("Murshidabad", "Nadia")), c("19333", "19336"))
  expect_equal(codes(state = "WB", district = c("Nadia", "Murshidabad")), c("19336", "19333"))
  expect_equal(codes(state = "WB", district = c("murshidabad", "nadia")), c("19333", "19336"))
})

test_that("returns correct codes if it starts with a 0", {
  expect_equal(codes(state = "PB"), "03")
  expect_equal(codes(state = "RJ"), "08")
  expect_equal(codes(state = "PB", district = "amritsar"), "03049")
  expect_equal(codes(state = "PB", district = "Amritsar"), "03049")
})

test_that("NA is returned for invalid state", {
  expect_true(is.na(codes(state = "Puerto Rico")))
})

test_that("error occurs for missing state", {
  expect_error(codes(district = "Amritsar"))
})

test_that("error occurs for district with list of states", {
  expect_error(codes(state = c("WB", "PB"), district = "Amritsar"))
})

test_that("error occurs for invalid districts", {
  expect_error(codes(state = "WB", district = "Fake County"))
  expect_error(codes(state = "PB", district = c("Fake County 1", "Fake County 2")))
})
