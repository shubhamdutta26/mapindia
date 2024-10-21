test_that("all states returned when no code specified", {
  expect_equal(code_info()$code, codes())
})

test_that("returns correct state code information", {
  result <- data.frame(
    abbr = c("WB", "TN", "WB"),
    code = c("19", "33", "19"),
    stname = c("West Bengal", "Tamil Nadu", "West Bengal"),
    stringsAsFactors = FALSE
  )

  expect_equal(code_info(c(19, 33, 19)), result, ignore_attr = TRUE)
  expect_equal(code_info(c("19", "33", "19")), result, ignore_attr = TRUE)

  result_sorted <- data.frame(
    abbr = c("TN", "WB"),
    code = c("33", "19"),
    stname = c("Tamil Nadu", "West Bengal"),
    stringsAsFactors = FALSE
  )

  expect_equal(code_info(c(19, 33, 19), sortAndRemoveDuplicates = TRUE),
               result_sorted,
               ignore_attr = TRUE)
  expect_equal(code_info(c("19", "33", "19"), sortAndRemoveDuplicates = TRUE),
               result_sorted,
               ignore_attr = TRUE)
})

test_that("returns correct district code information", {
  result <- data.frame(
    stname = rep("West Bengal", 3),
    abbr = rep("WB", 3),
    dtname = c("Murshidabad", "Nadia", "Kolkata"),
    code = c("19333", "19336", "19342"),
    stringsAsFactors = FALSE
  )

  expect_equal(code_info(c(19333, 19336, 19342)), result, ignore_attr = TRUE)
  expect_equal(code_info(c("19333", "19336", "19342")), result, ignore_attr = TRUE)

  ak_result_sorted <- data.frame(
    stname = rep("West Bengal", 2),
    abbr = rep("WB", 2),
    dtname = c("Bankura", "Puruliya"),
    code = c("19339", "19340"),
    stringsAsFactors = FALSE
  )

  expect_equal(code_info(c(19340, 19339, 19340), sortAndRemoveDuplicates = TRUE),
               ak_result_sorted,
               ignore_attr = TRUE)
  expect_equal(code_info(c("19340", "19339", "19340"), sortAndRemoveDuplicates = TRUE),
               ak_result_sorted,
               ignore_attr = TRUE)
})

test_that("warning occurs for non-existent code", {
  expect_warning(code_info(c("39", "04")))
  expect_warning(code_info("03055"))
})

test_that("error occurs for non-numeric/character code", {
  expect_error(code_info(data.frame()))
})

test_that("error occurs for invalid code", {
  expect_error(code_info("999999"))
  expect_error(code_info(999))
})
